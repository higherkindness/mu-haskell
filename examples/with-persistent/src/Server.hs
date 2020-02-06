{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Mu.Adapter.Persistent   (runDb)
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running app with persistent"
  runStderrLoggingT $
    withSqliteConn @(LoggingT IO) ":memory:" $ \conn -> do
      runDb conn $ runMigration migrateAll
      liftIO $ runGRpcApp msgProtoBuf 1234 (server conn)

server :: SqlBackend -> ServerT Maybe PersistentService ServerErrorIO _
server p = Server (getPerson p :<|>: newPerson p :<|>: allPeople p :<|>: H0)

getPerson :: SqlBackend -> MPersonRequest -> ServerErrorIO (Entity Person)
getPerson conn (MPersonRequest (Just idf)) = do
  r <- runDb conn $ do
    let pId = PersonKey $ SqlBackendKey idf
    maybePerson <- get pId
    pure $ Entity pId <$> maybePerson
  case r of
    Just p  -> pure p
    Nothing -> serverError $ ServerError NotFound "unknown person"
getPerson _ _ = serverError $ ServerError Invalid "missing person id"

newPerson :: SqlBackend -> MPerson -> ServerErrorIO MPersonRequest
newPerson conn (MPerson _ (Just name) (Just age)) = runDb conn $ do
  PersonKey (SqlBackendKey nId) <- insert (Person name age)
  pure $ MPersonRequest (Just nId)
newPerson _ _ = serverError $ ServerError Invalid "missing person data"

allPeople :: SqlBackend -> ConduitT (Entity Person) Void ServerErrorIO () -> ServerErrorIO ()
allPeople conn sink = runDb conn $
  runConduit $ selectSource [] [] .| liftServerConduit sink
