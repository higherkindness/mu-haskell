{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Maybe              (fromMaybe)
import           Data.Pool
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running app with persistent"
  runStderrLoggingT $
    withSqlitePool @(LoggingT IO) @SqlBackend ":memory:" 10 $ \pool -> do
      liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAll
      runGRpcApp 1234 (server pool)

server :: MonadServer m => Pool SqlBackend -> ServerT PersistentService m _
server p = Server (getPerson p :<|>: newPerson p :<|>: allPeople p :<|>: H0)

getPerson :: (MonadServer m, MonadLogger m)
          => Pool SqlBackend
          -> PersonRequest
          -> m Person
getPerson pool (PersonRequest idf) = liftIO $ flip runSqlPersistMPool pool $ do
  logDebugN $ T.pack $ "getting by identifier: " ++ show idf
  maybePerson <- get $ PersonKey $ SqlBackendKey idf
  case maybePerson of
    Just person -> pure person
    Nothing     -> serverError $ ServerError NotFound "unknown person"

newPerson :: (MonadServer m, MonadLogger m)
          => Pool SqlBackend
          -> Person
          -> m PersonRequest
newPerson pool p@(Person name _) = liftIO $ flip runSqlPersistMPool pool $ do
  logDebugN $ T.pack $ "inserting person: " ++ show p
  newId <- insert p
  pure $ case newId of
    PersonKey (SqlBackendKey nId) -> PersonRequest nId

allPeople :: (MonadServer m, MonadLogger m)
          => Pool SqlBackend
          -> ConduitT Person Void m ()
          -> m ()
allPeople pool = liftIO $ flip runSqlPersistMPool pool $ do
  logDebugN $ T.pack "streaming people..."
  pure $ selectSource [] []
