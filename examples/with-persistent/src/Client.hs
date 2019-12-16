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
import qualified Data.Text                as T
import           Database.Persist.Types   (Entity)
import           System.Environment

import           Mu.GRpc.Client.TyApps

import           Schema

main :: IO ()
main = do
  let config = grpcClientConfigSimple "127.0.0.1" 1234 False
  Right client <- setupGrpcClient' config
  args <- getArgs
  case args of
    ["watch"]          -> watching client
    ["get", idp]       -> get client idp
    ["add", name, age] -> add client name age
    _                  -> putStrLn "unknown command"

get :: GrpcClient -> String -> IO ()
get client idPerson = do
  let req = PersonRequest $ read idPerson
  putStrLn ("GET: Is there some person with id: " ++ idPerson ++ "?")
  rknown :: GRpcReply (Entity Person)
    <- gRpcCall @PersistentService @"getPerson" client req
  putStrLn ("GET: response was: " ++ show rknown)

add :: GrpcClient -> String -> String -> IO ()
add client name age = do
  let p = Person (T.pack name) (read age)
  putStrLn ("ADD: Creating new person " <> name <> " with age " <> age)
  response :: GRpcReply PersonRequest
    <- gRpcCall @PersistentService @"newPerson" client p
  putStrLn ("ADD: Was creating successful? " <> show response)

watching :: GrpcClient -> IO ()
watching client = do
  replies <- gRpcCall @PersistentService @"allPeople" client
  runConduit $ replies .| C.mapM_ (print :: GRpcReply (Entity Person) -> IO ())
