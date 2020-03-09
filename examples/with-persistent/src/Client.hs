{-# language DataKinds           #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}

module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           Mu.GRpc.Client.TyApps
import           System.Environment

import           Schema

main :: IO ()
main = do
  let config = grpcClientConfigSimple "127.0.0.1" 1234 False
  Right client <- setupGrpcClient' config
  args <- getArgs
  case args of
    ["watch"]       -> watching client
    ["get", idp]    -> get client idp
    ["add", nm, ag] -> add client nm ag
    _               -> putStrLn "unknown command"

get :: GrpcClient -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ read idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  response :: GRpcReply MPerson
    <- gRpcCall @'MsgProtoBuf @PersistentService @"PersistentService" @"getPerson" client req
  putStrLn $ "GET: response was: " ++ show response

add :: GrpcClient -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (T.pack nm) (read ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  response :: GRpcReply MPersonRequest
    <- gRpcCall @'MsgProtoBuf @PersistentService @"PersistentService" @"newPerson" client p
  putStrLn $ "ADD: was creating successful? " ++ show response

watching :: GrpcClient -> IO ()
watching client = do
  replies <- gRpcCall @'MsgProtoBuf @PersistentService @"PersistentService" @"allPeople" client
  runConduit $ replies .| C.mapM_ (print :: GRpcReply MPerson -> IO ())
