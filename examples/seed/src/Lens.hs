{-# language DataKinds             #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedLabels      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger
import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Text                as T
import           Mu.GRpc.Server
import           Mu.Schema
import           Mu.Schema.Lens
import           Mu.Server

import           Schema

type Person          = Term SeedSchema (SeedSchema :/: "Person")
type PeopleRequest   = Term SeedSchema (SeedSchema :/: "PeopleRequest")
type PeopleResponse  = Term SeedSchema (SeedSchema :/: "PeopleResponse")
type Weather         = Term SeedSchema (SeedSchema :/: "Weather")
type WeatherRequest  = Term SeedSchema (SeedSchema :/: "WeatherRequest")
type WeatherResponse = Term SeedSchema (SeedSchema :/: "WeatherResponse")

main :: IO ()
main = do
  putStrLn "running seed application"
  runGRpcAppTrans msgProtoBuf 8080 runStderrLoggingT server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => SingleServerT info PeopleService m _
server = singleService
  ( method @"getPerson" getPerson
  , method @"getPersonStream" getPersonStream
  , method @"getWeather" getWeather )

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson req = record (Just $ record (req ^. #name, 18))

getPerson :: Monad m => PeopleRequest -> m PeopleResponse
getPerson = pure . evolvePerson

getPersonStream
  :: (MonadServer m, MonadLogger m)
  => ConduitT () PeopleRequest m ()
  -> ConduitT PeopleResponse Void m ()
  -> m ()
getPersonStream source sink = runConduit $ source .| C.mapM reStream .| sink
  where
    reStream req = do
      liftIO $ threadDelay (2 * 1000 * 1000) -- 2 sec
      logDebugN $ T.pack $ "stream request: " ++ show req
      pure $ evolvePerson req

getWeather :: (MonadServer m)
           => WeatherRequest
           -> m WeatherResponse
getWeather msg
 | Just w <- msg ^. #currentWeather
 = pure $ record (go w)
 | otherwise
 = pure $ record "who knows?"
 where go e | not $ e `isn't` #sunny  = "is sunny! 😄"
            | not $ e `isn't` #cloudy = "is cloudy 😟"
            | not $ e `isn't` #rainy  = "is rainy... 😭"
            | otherwise               = error "this should never happen"
