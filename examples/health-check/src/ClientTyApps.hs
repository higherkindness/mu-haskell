{-# language AllowAmbiguousTypes #-}
{-# language DataKinds           #-}
{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           System.Environment

import           Mu.GRpc.Client.TyApps

import           Definition

main :: IO ()
main = do -- Setup the client
  let config = grpcClientConfigSimple "127.0.0.1" 50051 False
  Right client <- setupGrpcClient' config
  -- Execute command
  args <- getArgs
  case args of
    ["watch" , who]            -> watching client who
    ["simple", who]            -> simple client who
    ["update", who]            -> update client who "SERVING"
    ["update", who, newstatus] -> update client who newstatus
    _                          -> putStrLn "unknown command"

simple :: GrpcClient -> String -> IO ()
simple client who = do
  let hcm = HealthCheckMsg $ T.pack who
  putStrLn ("UNARY: Is there some server named " <> who <> "?")
  rknown :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"check" client hcm
  putStrLn ("UNARY: Actually the status is " <> show rknown)
  update client who "SERVING"
  r <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"clearStatus" client hcm
  putStrLn ("UNARY: Was clearing successful? " <> show r)
  runknown :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"check" client hcm
  putStrLn ("UNARY: Current status of " <> who <> ": " <> show runknown)

update :: GrpcClient -> String -> String -> IO ()
update client who newstatus = do
  let hcm = HealthCheckMsg $ T.pack who
  putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
  r <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"setStatus" client
                (HealthStatusMsg (Just hcm) (Just $ ServerStatusMsg (T.pack newstatus)))
  putStrLn ("UNARY: Was setting successful? " <> show r)
  rstatus :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"check" client hcm
  putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)

watching :: GrpcClient -> String -> IO ()
watching client who = do
  let hcm = HealthCheckMsg $ T.pack who
  replies <- gRpcCall @'MsgProtoBuf @HealthCheckService @"HealthCheckServiceFS2" @"watch" client hcm
  runConduit $ replies .| C.mapM_ (print :: GRpcReply ServerStatusMsg -> IO ())
