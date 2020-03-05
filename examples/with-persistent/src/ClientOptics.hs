{-# language DataKinds        #-}
{-# language OverloadedLabels #-}

module Main where

import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           Mu.GRpc.Client.Optics
import           System.Environment
import           Text.Read                (readMaybe)

import           Schema

main :: IO ()
main = do
  Right client <- initGRpc (grpcClientConfigSimple "127.0.0.1" 1234 False) msgProtoBuf
  args <- getArgs
  case args of
    ["watch"]       -> watching client
    ["get", idp]    -> get client idp
    ["add", nm, ag] -> add client nm ag
    _               -> putStrLn "unknown command"

get :: GRpcConnection PersistentService 'MsgProtoBuf -> String -> IO ()
get client idPerson = do
  let req = readMaybe idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  response <- client ^. #getPerson $ record1 req
  putStrLn $ "GET: response was: " ++ show response

add :: GRpcConnection PersistentService 'MsgProtoBuf -> String -> String -> IO ()
add client nm ag = do
  let p = record (Nothing, Just (T.pack nm), readMaybe ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  response <- client ^. #newPerson $ p
  putStrLn $ "ADD: was creating successful? " ++ show response

watching :: GRpcConnection PersistentService 'MsgProtoBuf -> IO ()
watching client = do
  replies <- client ^. #allPeople
  runConduit $ replies .| C.mapM_ print
