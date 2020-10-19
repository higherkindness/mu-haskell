{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Monad.Except              (catchError)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Mu.Adapter.Persistent             (runDb)
import           Mu.GraphQL.Quasi
import           Mu.GraphQL.Server
import           Mu.Instrumentation.Prometheus
import           Mu.Schema
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
    withSqliteConn @(LoggingT IO) ":memory:" $ \conn -> do
      runDb conn $ runMigration migrateAll
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
insertSeedData :: SqlBackend -> LoggingT IO (Maybe ())
insertSeedData conn = sequence_ <$> sequence
  [ insertAuthorAndBooks conn (Author "Robert Louis Stevenson")
      [Book "Treasure Island", Book "Strange Case of Dr Jekyll and Mr Hyde"]
  , insertAuthorAndBooks conn (Author "Immanuel Kant")
      [Book "Critique of Pure Reason"]
  , insertAuthorAndBooks conn (Author "Michael Ende")
      [Book "The Neverending Story", Book "Momo"]
  ]

{- | Inserts Author and Books
Returns Nothing in case of any failure, including attempts to insert non-unique values
-}
insertAuthorAndBooks :: SqlBackend -> Author -> [Key Author -> Book] -> LoggingT IO (Maybe ())
insertAuthorAndBooks conn author books =
  (`catchError` (const $ pure Nothing)) . runDb conn . fmap sequence_ $ do
    Just authorId <- insertUnique author
    traverse (insertUnique . ($ authorId)) books


type ObjectMapping = '[
    "Book"   ':-> Entity Book
  , "Author" ':-> Entity Author
  ]

libraryServer :: forall i.
                 SqlBackend
              -> ServerT ObjectMapping i Library ServerErrorIO _
libraryServer conn
  = resolver ( object @"Book"     ( field  @"id"        bookId
                                  , field  @"title"     bookTitle
                                  , field  @"author"    bookAuthor )
             , object @"Author"   ( field  @"id"        authorId
                                  , field  @"name"      authorName
                                  , field  @"books"     authorBooks )
             , object @"Query"    ( method @"authors"   allAuthors
                                  , method @"books"     allBooks )
             , object @"Mutation" ( method @"newAuthor" newAuthor
                                  , method @"newBook"   newBook )
             , object @"Subscription" ( method @"allBooks" allBooksConduit )
             )
  where
    bookId :: Entity Book -> ServerErrorIO Integer
    bookId (Entity (BookKey k) _) = pure $ toInteger k
    bookTitle :: Entity Book -> ServerErrorIO T.Text
    bookTitle (Entity _ Book { bookTitle = t }) = pure t
    bookAuthor :: Entity Book -> ServerErrorIO (Entity Author)
    bookAuthor (Entity _ Book { bookAuthor = a })
      = runDb conn $ Entity a . fromJust <$> get a
    authorId :: Entity Author -> ServerErrorIO Integer
    authorId (Entity (AuthorKey k) _) = pure $ toInteger k
    authorName :: Entity Author -> ServerErrorIO T.Text
    authorName (Entity _ Author { authorName = t }) = pure t
    authorBooks :: Entity Author -> ServerErrorIO [Entity Book]
    authorBooks (Entity a _)
      = runDb conn $ selectList [BookAuthor ==. a] [Asc BookTitle]

    allAuthors :: T.Text -> ServerErrorIO [Entity Author]
    allAuthors nameFilter
      = runDb conn $ selectList [Filter AuthorName (FilterValue nameFilter) (BackendSpecificFilter "LIKE")]
                                [Asc AuthorName]
    allBooks :: T.Text -> ServerErrorIO [Entity Book]
    allBooks titleFilter
      = runDb conn $ selectList [Filter BookTitle (FilterValue titleFilter) (BackendSpecificFilter "LIKE")]
                                [Asc BookTitle]
    allBooksConduit :: ConduitT (Entity Book) Void ServerErrorIO () -> ServerErrorIO ()
    allBooksConduit sink
      = runDb conn $ runConduit $
          selectSource [] [Asc BookTitle] .| liftServerConduit sink

    newAuthor :: NewAuthor -> ServerErrorIO (Maybe (Entity Author))
    newAuthor (NewAuthor name) = runDb conn $ do
      let new = Author name
      result <- insertUnique new
      pure $ Entity <$> result <*> pure new

    newBook :: NewBook -> ServerErrorIO (Maybe (Entity Book))
    newBook (NewBook title authorId) = runDb conn $ do
      let new = Book title (toAuthorId $ fromInteger authorId)
      result <- insertUnique new
      pure $ Entity <$> result <*> pure new
