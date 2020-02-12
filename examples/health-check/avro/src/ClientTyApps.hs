{-# language AllowAmbiguousTypes #-}
{-# language DataKinds           #-}
{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeOperators       #-}
module Main where

import qualified Data.Text             as T
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
    ["simple", who]            -> simple client who
    ["update", who]            -> update client who "SERVING"
    ["update", who, newstatus] -> update client who newstatus
    _                          -> putStrLn "unknown command"

simple :: GrpcClient -> String -> IO ()
simple client who = do
  let hcm = HealthCheckMsg (T.pack who)
  putStrLn ("UNARY: Is there some server named " <> who <> "?")
  rknown :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgAvro @HealthCheckService @"check" client hcm
  putStrLn ("UNARY: Actually the status is " <> show rknown)
  update client who "SERVING"
  r <- gRpcCall @'MsgAvro @HealthCheckService @"clearStatus" client hcm
  putStrLn ("UNARY: Was clearing successful? " <> show r)
  runknown :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgAvro @HealthCheckService @"check" client hcm
  putStrLn ("UNARY: Current status of " <> who <> ": " <> show runknown)

update :: GrpcClient -> String -> String -> IO ()
update client who newstatus = do
  let hcm = HealthCheckMsg (T.pack who)
  putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
  r <- gRpcCall @'MsgAvro @HealthCheckService @"setStatus" client
                (HealthStatusMsg hcm (ServerStatusMsg (T.pack newstatus)))
  putStrLn ("UNARY: Was setting successful? " <> show r)
  rstatus :: GRpcReply ServerStatusMsg
    <- gRpcCall @'MsgAvro @HealthCheckService @"check" client hcm
  putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)
