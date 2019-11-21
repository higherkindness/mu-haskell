{-# language OverloadedStrings #-}
module Main where

import           Mu.GRpc.Server
import           Mu.Rpc.Examples

main :: IO ()
main = do
  putStrLn "running quickstart application"
  runGRpcApp 8080 quickstartServer
