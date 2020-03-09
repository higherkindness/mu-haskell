{-# language DataKinds             #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedLabels      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Text                as T
import           Mu.GRpc.Server
import           Mu.Schema
import           Mu.Schema.Optics
import           Mu.Server

import           Schema

type Person         = Term SeedSchema (SeedSchema :/: "Person")
type PeopleRequest  = Term SeedSchema (SeedSchema :/: "PeopleRequest")
type PeopleResponse = Term SeedSchema (SeedSchema :/: "PeopleResponse")

main :: IO ()
main = do
  putStrLn "running seed application"
  runGRpcAppTrans msgProtoBuf 8080 runStderrLoggingT server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => SingleServerT PeopleService m _
server = Server (getPerson :<|>: getPersonStream :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson req = record1 (Just $ record (req ^. #name, 18))

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
