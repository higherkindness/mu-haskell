{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# options_ghc -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Logger              (LoggingT, runStderrLoggingT)
import           Data.Conduit                      (ConduitT, Void, runConduit, (.|))
import           Data.Conduit.Combinators          (yieldMany)
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import           Database.Persist.Sqlite
import           Mu.Adapter.Persistent             (Pool, runDbPool)
import           Mu.GraphQL.Server                 (graphQLApp, liftServerConduit)
import           Mu.Instrumentation.Prometheus     (initPrometheus, prometheus)
import           Mu.Schema                         (Mapping ((:->)), Proxy (Proxy))
import           Mu.Server
import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Schema

main :: IO ()
main = do
  -- Setup CORS
  let hm = addHeaders [
             ("Access-Control-Allow-Origin", "*")
           , ("Access-Control-Allow-Headers", "Content-Type")
           ]
  -- Set up Prometheus
  p <- initPrometheus "library"
  -- Run the whole thing
  runStderrLoggingT $
    withSqlitePool @(LoggingT IO) ":memory:" 1000 $ \conn -> do
      runDbPool conn $ runMigration migrateAll
      -- Insert demo data
      insertSeedData conn
      liftIO $ putStrLn "starting GraphQL server on port 8000"
      liftIO $ run 8000 $ hm $
        graphQLApp (prometheus p $ libraryServer conn)
                   (Proxy @('Just "Query"))
                   (Proxy @('Just "Mutation"))
                   (Proxy @('Just "Subscription"))

{- | Inserts demo data to make this example valueable for testing with different clients
     Returns Nothing in case of any failure, including attempts to insert non-unique values
-}
insertSeedData :: Pool SqlBackend -> LoggingT IO (Maybe ())
insertSeedData conn = sequence_ <$> traverse (uncurry $ insertAuthorAndBooks conn) seedData
  where seedData =
          [ (Author "Robert Louis Stevenson",
            [ Book "Treasure Island" "https://m.media-amazon.com/images/I/51C6NXR94gL.jpg"
            , Book "Strange Case of Dr Jekyll and Mr Hyde" "https://m.media-amazon.com/images/I/51e8pkDxjfL.jpg"
            ])
          , (Author "Immanuel Kant",
            [ Book "Critique of Pure Reason" "https://m.media-amazon.com/images/I/51h+rBXrYeL.jpg"])
          , (Author "Michael Ende",
            [ Book "The Neverending Story" "https://m.media-amazon.com/images/I/51AnD2Fki3L.jpg"
            , Book "Momo" "https://m.media-amazon.com/images/I/61AuiRa4nmL.jpg"
            ])
          ]

{- | Inserts Author and Books
     Returns Nothing in case of any failure, including attempts to insert non-unique values
-}
insertAuthorAndBooks :: Pool SqlBackend -> Author -> [Key Author -> Book] -> LoggingT IO (Maybe ())
insertAuthorAndBooks conn author books =
  runDbPool conn . fmap sequence_ $ do
    authorResult <- insertUnique author
    case authorResult of
      Just authorId -> traverse (\kBook -> insertUnique (kBook authorId)) books
      Nothing       -> pure [Nothing]

type ObjectMapping = '[
    "Book"   ':-> Entity Book
  , "Author" ':-> Entity Author
  ]

libraryServer :: Pool SqlBackend -> ServerT ObjectMapping i Library ServerErrorIO _
libraryServer conn = resolver
  ( object @"Book"
    ( field @"id"       bookId
    , field @"title"    bookTitle
    , field @"author"   bookAuthor
    , field @"imageUrl" bookImage
    )
  , object @"Author"
    ( field @"id"    authorId
    , field @"name"  authorName
    , field @"books" authorBooks
    )
  , object @"Query"
    ( method @"authors" allAuthors
    , method @"books"   allBooks
    )
  , object @"Mutation"
    ( method @"newAuthor" newAuthor
    , method @"newBook"   newBook
    )
  , object @"Subscription"
    ( method @"allBooks" allBooksConduit )
  )
  where
    bookId :: Entity Book -> ServerErrorIO Integer
    bookId (Entity (BookKey k) _) = pure $ toInteger k
    bookTitle :: Entity Book -> ServerErrorIO T.Text
    bookTitle (Entity _ Book { bookTitle }) = pure bookTitle
    bookAuthor :: Entity Book -> ServerErrorIO (Entity Author)
    bookAuthor (Entity _ Book { bookAuthor }) = do
      author <- runDbPool conn $ get bookAuthor
      pure $ Entity bookAuthor (fromJust author)
    bookImage :: Entity Book -> ServerErrorIO T.Text
    bookImage (Entity _ Book { bookImageUrl }) = pure bookImageUrl

    authorId :: Entity Author -> ServerErrorIO Integer
    authorId (Entity (AuthorKey k) _) = pure $ toInteger k
    authorName :: Entity Author -> ServerErrorIO T.Text
    authorName (Entity _ Author { authorName }) = pure authorName
    authorBooks :: Entity Author -> ServerErrorIO [Entity Book]
    authorBooks (Entity author _)
      = runDbPool conn $
          selectList [BookAuthor ==. author] [Asc BookTitle]

    allAuthors :: T.Text -> ServerErrorIO [Entity Author]
    allAuthors nameFilter
      = runDbPool conn $
          selectList [Filter AuthorName (FilterValue nameFilter)
                             (BackendSpecificFilter "LIKE")]
                     [Asc AuthorName]

    allBooks :: T.Text -> ServerErrorIO [Entity Book]
    allBooks titleFilter
      = runDbPool conn $
          selectList [Filter BookTitle (FilterValue titleFilter)
                             (BackendSpecificFilter "LIKE")]
                     [Asc BookTitle]

    allBooksConduit :: ConduitT (Entity Book) Void ServerErrorIO () -> ServerErrorIO ()
    allBooksConduit sink = do
      -- do not convert to a single selectConduit!
      -- there seems to be a problem running nested runDb's
      -- so we break it into two steps, assuming that the
      -- list of books would fit in memory
      -- see https://github.com/higherkindness/mu-haskell/issues/259
      lst <- liftIO $ runDbPool conn $ selectList [] [Asc BookTitle]
      runConduit $ yieldMany lst .| liftServerConduit sink

    newAuthor :: NewAuthor -> ServerErrorIO (Entity Author)
    newAuthor (NewAuthor name) = do
      maybeEntity <- runDbPool conn $ do
        let new = Author name
        result <- insertUnique new
        pure $ Entity <$> result <*> pure new
      let errorMsg = "Author \"" <> T.unpack name <> "\" already exists"
      maybe (serverError $ ServerError Invalid errorMsg) pure maybeEntity

    newBook :: NewBook -> ServerErrorIO (Entity Book)
    newBook (NewBook title authorId img) = do
      maybeEntity <- runDbPool conn $ do
        let new = Book title img (toAuthorId $ fromInteger authorId)
        result <- insertUnique new
        pure $ Entity <$> result <*> pure new
      let errorMsg = "Book \"" <> T.unpack title <> "\" already exists"
      maybe (serverError $ ServerError Invalid errorMsg) pure maybeEntity
