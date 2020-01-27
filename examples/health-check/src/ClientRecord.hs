{-# language AllowAmbiguousTypes #-}
{-# language DataKinds           #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeOperators       #-}
module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           System.Environment

import           Mu.GRpc.Client.Record

import           Definition

data HealthCall = HealthCall
  { setStatus   :: HealthStatusMsg -> IO (GRpcReply ())
  , check       :: HealthCheckMsg -> IO (GRpcReply ServerStatusMsg)
  , clearStatus :: HealthCheckMsg -> IO (GRpcReply ())
  , checkAll    :: IO (GRpcReply AllStatusMsg)
  , cleanAll    :: IO (GRpcReply ())
  , watch       :: HealthCheckMsg -> IO (ConduitT () (GRpcReply ServerStatusMsg) IO ())
  } deriving (Generic)

buildHealthCall :: GrpcClient -> HealthCall
buildHealthCall = buildService @'MsgProtoBuf @HealthCheckService @""

main :: IO ()
main = do -- Setup the client
  let config = grpcClientConfigSimple "127.0.0.1" 8080 False
  Right grpcClient <- setupGrpcClient' config
  let client = buildHealthCall grpcClient
  -- Execute command
  args <- getArgs
  case args of
    ["watch" , who]            -> watching client who
    ["simple", who]            -> simple client who
    ["update", who]            -> update client who "SERVING"
    ["update", who, newstatus] -> update client who newstatus
    _                          -> putStrLn "unknown command"

simple :: HealthCall -> String -> IO ()
simple client who = do
  let hcm = HealthCheckMsg (Just $ T.pack who)
  putStrLn ("UNARY: Is there some server named " <> who <> "?")
  rknown <- check client hcm
  putStrLn ("UNARY: Actually the status is " <> show rknown)
  update client who "SERVING"
  r <- clearStatus client hcm
  putStrLn ("UNARY: Was clearing successful? " <> show r)
  runknown <- check client hcm
  putStrLn ("UNARY: Current status of " <> who <> ": " <> show runknown)

update :: HealthCall -> String -> String -> IO ()
update client who newstatus = do
  let hcm = HealthCheckMsg (Just $ T.pack who)
  putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
  r <- setStatus client (HealthStatusMsg (Just hcm) (Just $ ServerStatusMsg (Just $ T.pack newstatus)))
  putStrLn ("UNARY: Was setting successful? " <> show r)
  rstatus <- check client hcm
  putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)

watching :: HealthCall -> String -> IO ()
watching client who = do
  let hcm = HealthCheckMsg (Just $ T.pack who)
  stream <- watch client hcm
  runConduit $ stream .| C.mapM_ print
