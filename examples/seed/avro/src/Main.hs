{-# language DataKinds             #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedLabels      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Monad.Logger
import           Data.Int
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
  runGRpcAppTrans msgAvro 8080 runStderrLoggingT server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => SingleServerT PeopleService m _
server = Server (getPerson :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson req = record1 (review _U0 $ record (req ^. #name, 18 :: Int32))

getPerson :: Monad m => PeopleRequest -> m PeopleResponse
getPerson = pure . evolvePerson
