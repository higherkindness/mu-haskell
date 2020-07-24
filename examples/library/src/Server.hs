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

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Maybe                        (fromJust)
import qualified Data.Text                         as T
import           Database.Persist
import           Database.Persist.Sqlite
import           Mu.Adapter.Persistent             (runDb)
import           Mu.GraphQL.Quasi
import           Mu.GraphQL.Server
import           Mu.Schema
import           Mu.Server
import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Schema

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8000"
  let hm = addHeaders [
             ("Access-Control-Allow-Origin", "*")
           , ("Access-Control-Allow-Headers", "Content-Type")
           ]
  runStderrLoggingT $
    withSqliteConn @(LoggingT IO) ":memory:" $ \conn -> do
      runDb conn $ runMigration migrateAll
      liftIO $ run 8000 $ hm $
        graphQLApp (libraryServer conn)
                   (Proxy @('Just "Query"))
                   (Proxy @('Just "Mutation"))
                   (Proxy @('Just "Subscription"))

type ObjectMapping = '[
    "Book"   ':-> Entity Book
  , "Author" ':-> Entity Author
  ]

libraryServer :: forall i.
                 SqlBackend
              -> ServerT ObjectMapping i Library ServerErrorIO _
libraryServer conn
  = resolver ( object @"Book"     ( field  @"id"       bookId
                                  , field  @"title"    bookTitle
                                  , field  @"author"   bookAuthor )
             , object @"Author"   ( field  @"id"       authorId
                                  , field  @"name"     authorName
                                  , field  @"books"    authorBooks )
             , object @"Query"    ( method @"authors"  allAuthors
                                  , method @"books"    allBooks )
             , object @"Mutation" (method @"newAuthor" newAuthor)
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
      = runDb conn $ runConduit $ selectSource [] [Asc BookTitle] .| transPipe raiseErrors sink
      where raiseErrors :: forall m a. MonadIO m => ServerErrorIO a -> m a
            raiseErrors h
              = liftIO $ do
                  h' <- runExceptT h
                  case h' of
                    Right r -> pure r
                    Left  e -> throw e

    newAuthor :: NewAuthor -> ServerErrorIO (Maybe (Entity Author))
    newAuthor (NewAuthor name) = runDb conn $ do
      let new = Author name
      result <- insertUnique new
      pure $ Entity <$> result <*> pure new
