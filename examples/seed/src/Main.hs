{-# language PartialTypeSignatures #-}

module Main where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Mu.GRpc.Server
import           Mu.Server

import           Schema

main :: IO ()
main = do
  putStrLn "running seed application"
  runGRpcApp 8080 server

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/seed/server/modules/process/src/main/scala/example/seed/server/process/ProtoPeopleServiceHandler.scala

server :: ServerIO PeopleService _
server = Server (getPerson :<|>: getPersonStream :<|>: H0)

evolvePerson :: PeopleRequest -> PeopleResponse
evolvePerson (PeopleRequest n) = PeopleResponse $ Person n 18

getPerson :: PeopleRequest -> ServerErrorIO PeopleResponse
getPerson = return . evolvePerson

getPersonStream :: ConduitT () PeopleRequest ServerErrorIO ()
                -> ConduitT PeopleResponse Void ServerErrorIO ()
                -> ServerErrorIO ()
getPersonStream source sink = runConduit $ source .| C.mapM reStream .| sink
  where
    reStream req = do
      liftIO $ threadDelay (2 * 1000 * 1000) -- 2 sec
      return $ evolvePerson req
