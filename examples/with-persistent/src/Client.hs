{-# language AllowAmbiguousTypes #-}
{-# language DataKinds           #-}
{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeOperators       #-}

module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Int                 (Int64)
-- import qualified Data.Text                as T
import           Database.Persist.Types   (Entity)
import           System.Environment

import           Mu.GRpc.Client.TyApps

import           Schema

main :: IO ()
main = do -- Setup the client
  let config = grpcClientConfigSimple "127.0.0.1" 1234 False
  Right client <- setupGrpcClient' config
  -- Execute command
  args <- getArgs
  case args of
    ["watch"]    -> watching client
    ["get", idP] -> get client idP
    -- ["add", name, age] -> add client name age
    _            -> putStrLn "unknown command"

get :: GrpcClient -> String -> IO ()
get client idPerson = do
  let req = PersonRequest $ read idPerson
  putStrLn ("GET: Is there some person with id: " ++ idPerson ++ "?")
  rknown :: GRpcReply (Entity Person)
    <- gRpcCall @PersistentService @"getPerson" client req
  putStrLn ("GET: response was: " ++ show rknown)

-- add :: GrpcClient -> String -> String -> IO ()
-- add client who newstatus = do
--   let hcm = HealthCheckMsg (T.pack who)
--   -- putStrLn ("UNARY: Setting " <> who <> " service to " <> newstatus)
--   r <- gRpcCall @PersistentService @"setStatus" client
--                 (HealthStatusMsg hcm (PersonRequest (T.pack newstatus)))
--   -- putStrLn ("UNARY: Was setting successful? " <> show r)
--   rstatus :: GRpcReply PersonRequest
--     <- gRpcCall @PersistentService @"check" client hcm
--   putStrLn ("UNARY: Checked the status of " <> who <> ". Obtained: " <> show rstatus)

watching :: GrpcClient -> IO ()
watching client = do
  replies <- gRpcCall @PersistentService @"allPeople" client
  runConduit $ replies .| C.mapM_ (print :: GRpcReply (Entity Person) -> IO ())
