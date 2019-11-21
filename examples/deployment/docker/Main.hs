{-# language OverloadedStrings #-}
module Main where

import           Mu.Rpc.Examples
import           Mu.Server.GRpc

main :: IO ()
main = do
  putStrLn "running quickstart application"
  runGRpcApp 8080 quickstartServer
