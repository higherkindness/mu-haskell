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
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running app with persistent"
  runStderrLoggingT $
    withSqliteConn @(LoggingT IO) ":memory:" $ \conn -> do
      liftIO $ flip runSqlPersistM conn $ runMigration migrateAll
      liftIO $ runGRpcApp 1234 (server conn)

server :: SqlBackend -> ServerT PersistentService ServerErrorIO _
server p = Server (getPerson p :<|>: newPerson p :<|>: allPeople p :<|>: H0)

runDb = (liftIO .) . flip runSqlPersistM

getPerson :: SqlBackend -> PersonRequest -> ServerErrorIO (Entity Person)
getPerson conn (PersonRequest idf) = do
  r <- runDb conn $ do
    let pId = PersonKey $ SqlBackendKey idf
    maybePerson <- get pId
    pure $ Entity pId <$> maybePerson
  case r of
    Just p  -> pure p
    Nothing -> serverError $ ServerError NotFound "unknown person"

newPerson :: SqlBackend -> Entity Person -> ServerErrorIO PersonRequest
newPerson conn (Entity _ p@(Person name _)) = runDb conn $ do
  PersonKey (SqlBackendKey nId) <- insert p
  pure $ PersonRequest nId

allPeople :: SqlBackend -> ConduitT (Entity Person) Void ServerErrorIO () -> ServerErrorIO ()
allPeople conn sink = runDb conn $
  runConduit $ selectSource [] [] .| liftServerConduit sink
