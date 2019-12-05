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
import           Data.Text                as T
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running seed application"
  runGRpcAppTrans 8080 runStderrLoggingT server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: (MonadServer m, MonadLogger m) => ServerT PeopleService m _
server = Server (getPerson :<|>: getPersonStream :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson (PeopleRequest n) = PeopleResponse $ Person n 18

getPerson :: Monad m => PeopleRequest -> m PeopleResponse
getPerson = return . evolvePerson

getPersonStream :: (MonadServer m, MonadLogger m)
                => ConduitT () PeopleRequest m ()
                -> ConduitT PeopleResponse Void m ()
                -> m ()
getPersonStream source sink = runConduit $ source .| C.mapM reStream .| sink
  where
    reStream req = do
      liftIO $ threadDelay (2 * 1000 * 1000) -- 2 sec
      logDebugN $ T.pack ("stream request: " ++ show req)
      return $ evolvePerson req
