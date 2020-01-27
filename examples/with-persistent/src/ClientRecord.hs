{-# language DataKinds         #-}
{-# language DeriveGeneric     #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications  #-}

module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Mu.GRpc.Client.Record
import           System.Environment
import           Text.Read                (readMaybe)

import           Schema

data PersistentCall = PersistentCall
  { getPerson :: MPersonRequest -> IO (GRpcReply MPerson)
  , newPerson :: MPerson -> IO (GRpcReply MPersonRequest)
  , allPeople :: IO (ConduitT () (GRpcReply MPerson) IO ())
  } deriving Generic

main :: IO ()
main = do
  let config = grpcClientConfigSimple "127.0.0.1" 1234 False
  Right grpcClient <- setupGrpcClient' config
  let client = buildService @'MsgProtoBuf @PersistentService @"" grpcClient
  args <- getArgs
  case args of
    ["watch"]       -> watching client
    ["get", idp]    -> get client idp
    ["add", nm, ag] -> add client nm ag
    _               -> putStrLn "unknown command"

get :: PersistentCall -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ readMaybe idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  res <- getPerson client req
  putStrLn $ "GET: response was: " ++ show res

add :: PersistentCall -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (Just $ T.pack nm) (readMaybe ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  res <- newPerson client p
  putStrLn $ "ADD: was creating successful? " ++ show res

watching :: PersistentCall -> IO ()
watching client = do
  replies <- allPeople client
  runConduit $ replies .| C.mapM_ print
