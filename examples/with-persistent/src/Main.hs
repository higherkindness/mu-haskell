{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Pool
import           Database.Persist.Sqlite
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running application with persistent"
  runMigration migrateAll
  runNoLoggingT $
    withSqlitePool @(NoLoggingT IO) @SqlBackend ":memory:" 10 $ \pool ->
      runGRpcApp 1234 server pool

server :: MonadServer m => Pool SqlBackend -> ServerT PersistentService m _
server p = Server (getPerson p :<|>: newPerson p :<|>: allPeople p :<|>: H0)

getPerson :: Monad m => Pool SqlBackend -> PersonRequest -> m Person
getPerson pool (PersonRequest name) = liftIO $ flip runSqlPersistMPool pool $ do
  p <- getBy $ UniquePerson name
  case p of
    Just (Entity _ person) -> pure $ Just person
    Nothing                -> pure Nothing

newPerson :: Monad m => Pool SqlBackend -> Person -> m PersonRequest
newPerson pool p@(Person name _) = liftIO $ flip runSqlPersistMPool pool $ do
  _ <- insert p
  pure $ PersonRequest name

allPeople :: MonadServer m => Pool SqlBackend -> ConduitT Person Void m () -> m ()
allPeople pool = liftIO $ flip runSqlPersistMPool pool $ selectSource [] []
