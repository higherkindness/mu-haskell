{-# language DataKinds, ScopedTypeVariables,
             TypeApplications, TypeOperators,
             FlexibleContexts, AllowAmbiguousTypes,
             OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import GHC.Generics
import System.Environment

import Mu.Client.GRpc
import Mu.Client.GRpc.Record

import Common

data HealthCall
  = HealthCall { setStatus :: HealthStatus -> IO (GRpcReply ()) 
               , check :: HealthCheck -> IO (GRpcReply ServerStatus) 
               , clearStatus :: HealthCheck -> IO (GRpcReply ()) 
               , checkAll :: IO (GRpcReply AllStatus) 
               , cleanAll :: IO (GRpcReply ()) 
               , watch :: HealthCheck -> IO (ConduitT () (GRpcReply ServerStatus) IO ()) }
  deriving (Generic)

main :: IO ()
main 
  = do -- Setup the client
       let config = grpcClientConfigSimple "127.0.0.1" 8080 False
       Right grpcClient <- setupGrpcClient' config
       let client = buildService @"healthcheck" @HealthCheckService grpcClient
       -- Execute command
       args <- getArgs
       case args of
         ["watch" , who] -> watching client who
         ["simple", who] -> simple client who
         ["update", who] -> update client who "SERVING"
         ["update", who, newstatus] -> update client who newstatus
         _ -> putStrLn "unknown command"

simple :: HealthCall -> String -> IO ()
simple client who
  = do let hc = HealthCheck (T.pack who)
       putStrLn ("UNARY: Is there some server named " <> who <> "?")
       rknown <- check client hc
       putStrLn ("UNARY: Actually the status is " <> show rknown)
       update client who "SERVING"
       r <- clearStatus client hc
       putStrLn ("UNARY: Was clearing successful? " <> show r)
       runknown <- check client hc
       putStrLn ("UNARY: Current status of " <> who <> ": " <> show runknown)

update :: HealthCall -> String -> String -> IO ()
update client who newstatus
  = do let hc = HealthCheck (T.pack who)
       putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
       r <- setStatus client (HealthStatus hc (ServerStatus (T.pack newstatus)))
       putStrLn ("UNARY: Was setting successful? " <> show r)
       rstatus <- check client hc
       putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)

watching :: HealthCall -> String -> IO ()
watching client who
  = do let hc = HealthCheck (T.pack who)
       stream <- watch client hc
       runConduit $ stream .| C.mapM_ print