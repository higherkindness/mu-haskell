{-# language EmptyDataDecls             #-}
{-# language FlexibleContexts           #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language NamedFieldPuns             #-}
{-# language OverloadedStrings          #-}
{-# language PartialTypeSignatures      #-}
{-# language QuasiQuotes                #-}
{-# language TemplateHaskell            #-}
{-# language TypeApplications           #-}
{-# language TypeFamilies               #-}

module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Conduit
import           Data.Maybe              (fromMaybe)
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Mu.GRpc.Server
import           Mu.Server
import           Prelude                 hiding (id)

import           Schema

main :: IO ()
main = do
  putStrLn "running application with persistent"
  return ()
  -- runGRpcApp 1234 server

-- Database

-- mkPersist sqlSettings [persistLowerCase|
-- PersonId
--   id Int
--   deriving Show
-- Person
--   personId PersonId
--   name     String
--   age      Int
--   deriving Show
-- |]

-- Server implementation

server :: MonadServer m => ServerT PersistentService m _
server = Server (getPerson :<|>: newPerson :<|>: allPeople :<|>: H0)

getPerson :: Monad m => PersonId -> m Person
getPerson PersonId{id = pid} = liftIO $ runSqlite @IO @SqlBackend "example.db" $ do
  person <- get $ PersonId (SqlBackend pid)
  pure $ fromMaybe Nothing person

newPerson :: Monad m => Person -> m PersonId
newPerson person@Person{personId} = liftIO $ runSqlite @IO @SqlBackend "example.db" $ do
  _ <- insert person
  return personId

allPeople :: MonadServer m => ConduitT Person Void m () -> m ()
allPeople = liftIO $ runSqlite @IO @SqlBackend "example.db" $ selectSource [] []
