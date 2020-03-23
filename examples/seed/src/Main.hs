{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Int
import           Data.Text                as T
import           GHC.Generics
import           Mu.GraphQL.Server
import           Mu.GRpc.Server
import           Mu.Schema
import           Mu.Server
import           Network.Wai

import           Schema

data Person = Person
  { name :: T.Text
  , age  :: Int32
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   SeedSchema "Person"
             , FromSchema SeedSchema "Person" )

newtype PeopleRequest = PeopleRequest
  { name :: T.Text
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   SeedSchema "PeopleRequest"
             , FromSchema SeedSchema "PeopleRequest" )

newtype PeopleResponse = PeopleResponse
  { person :: Maybe Person
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   SeedSchema "PeopleResponse"
             , FromSchema SeedSchema "PeopleResponse" )

main :: IO ()
main = do
  putStrLn "running seed application"
  runConcurrently $ (\_ _ _ -> ())
    <$> Concurrently (runGRpcAppTrans msgProtoBuf 8080 runStderrLoggingT server)
    <*> Concurrently (runGRpcAppTrans msgAvro     8081 runStderrLoggingT server)
    <*> Concurrently (runGraphQLAppTrans         50053 runStderrLoggingT server
                        (Proxy @('Just "PeopleService"))
                        (Proxy @'Nothing) (Proxy @'Nothing))


-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => SingleServerT PeopleService m _
server = Server (getPerson :<|>: getPersonStream :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson (PeopleRequest n) = PeopleResponse $ Just $ Person n 18

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
