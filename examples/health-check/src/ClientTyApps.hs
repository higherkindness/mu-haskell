{-# language DataKinds, ScopedTypeVariables,
             TypeApplications, TypeOperators,
             FlexibleContexts, AllowAmbiguousTypes,
             OverloadedStrings #-}
module Main where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import System.Environment

import Mu.Client.GRpc

import Common

main :: IO ()
main 
  = do -- Setup the client
       let config = grpcClientConfigSimple "127.0.0.1" 8080 False
       Right client <- setupGrpcClient' config
       -- Execute command
       args <- getArgs
       case args of
         ["watch" , who] -> watch client who
         ["simple", who] -> simple client who
         ["update", who] -> update client who "SERVING"
         ["update", who, newstatus] -> update client who newstatus
         _ -> putStrLn "unknown command"

simple :: GrpcClient -> String -> IO ()
simple client who
  = do let hc = HealthCheck (T.pack who)
       putStrLn ("UNARY: Is there some server named " <> who <> "?")
       rknown :: GRpcReply ServerStatus
         <- gRpcCall @HealthCheckService @"check" client hc
       putStrLn ("UNARY: Actually the status is " <> show rknown)
       update client who "SERVING"
       r :: GRpcReply ()
         <- gRpcCall @HealthCheckService @"clearStatus" client hc
       putStrLn ("UNARY: Was clearing successful? " <> show r)
       runknown :: GRpcReply ServerStatus
         <- gRpcCall @HealthCheckService @"check" client hc
       putStrLn ("UNARY: Current status of " <> who <> ": " <> show runknown)

update :: GrpcClient -> String -> String -> IO ()
update client who newstatus
  = do let hc = HealthCheck (T.pack who)
       putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
       r :: GRpcReply ()
         <- gRpcCall @HealthCheckService @"setStatus" client
                     (HealthStatus hc (ServerStatus (T.pack newstatus)))
       putStrLn ("UNARY: Was setting successful? " <> show r)
       rstatus :: GRpcReply ServerStatus
         <- gRpcCall @HealthCheckService @"check" client hc
       putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)

watch :: GrpcClient -> String -> IO ()
watch client who
  = do let hc = HealthCheck (T.pack who)
       replies :: ConduitT () (GRpcReply ServerStatus) IO ()
         <- gRpcCall @HealthCheckService @"watch" client hc
       runConduit $ replies .| C.mapM_ print