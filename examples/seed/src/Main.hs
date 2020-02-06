{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Int
import           Data.Text                as T
import           GHC.Generics
import           Mu.GRpc.Server
import           Mu.Schema
import           Mu.Server

import           Schema

data Person = Person
  { name :: Maybe T.Text
  , age  :: Maybe Int32
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "Person"
             , FromSchema Maybe SeedSchema "Person" )

newtype PeopleRequest = PeopleRequest
  { name :: Maybe T.Text
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "PeopleRequest"
             , FromSchema Maybe SeedSchema "PeopleRequest" )

newtype PeopleResponse = PeopleResponse
  { person :: Maybe Person
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "PeopleResponse"
             , FromSchema Maybe SeedSchema "PeopleResponse" )

main :: IO ()
main = do
  putStrLn "running seed application"
  runGRpcAppTrans msgProtoBuf 8080 runStderrLoggingT server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => ServerT Maybe PeopleService m _
server = Server (getPerson :<|>: getPersonStream :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson (PeopleRequest n) = PeopleResponse $ Just $ Person n (Just 18)

getPerson :: Monad m => PeopleRequest -> m PeopleResponse
getPerson = pure . evolvePerson

getPersonStream :: (MonadServer m, MonadLogger m)
                => ConduitT () PeopleRequest m ()
                -> ConduitT PeopleResponse Void m ()
                -> m ()
getPersonStream source sink = runConduit $ source .| C.mapM reStream .| sink
  where
    reStream req = do
      liftIO $ threadDelay (2 * 1000 * 1000) -- 2 sec
      logDebugN $ T.pack $ "stream request: " ++ show req
      pure $ evolvePerson req
