{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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

server :: SqlBackend -> SingleServerT PersistentService ServerErrorIO _
server p
  = singleService ( method @"getPerson" $ getPerson p
                  , method @"newPerson" $ newPerson p
                  , method @"allPeople" $ allPeople p)

getPerson :: SqlBackend -> MPersonRequest -> ServerErrorIO (Entity Person)
getPerson conn (MPersonRequest idf) = do
  r <- runDb conn $ do
    let pId = PersonKey $ SqlBackendKey idf
    maybePerson <- get pId
    pure $ Entity pId <$> maybePerson
  case r of
    Just p  -> pure p
    Nothing -> serverError $ ServerError NotFound "unknown person"

newPerson :: SqlBackend -> MPerson -> ServerErrorIO MPersonRequest
newPerson conn (MPerson _ name age) = runDb conn $ do
  PersonKey (SqlBackendKey nId) <- insert (Person name age)
  pure $ MPersonRequest nId

allPeople :: SqlBackend -> ConduitT (Entity Person) Void ServerErrorIO () -> ServerErrorIO ()
allPeople conn sink = runDb conn $
  runConduit $ selectSource [] [] .| liftServerConduit sink
