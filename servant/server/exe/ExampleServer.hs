{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson
import qualified Data.Text.IO as Text
import Mu.Rpc.Annotations
import Mu.Rpc.Examples
import Mu.Servant.Server
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = do
  putStrLn "running quickstart application"
  Text.putStrLn $ layout quickstartAPI
  run 8081 (serve quickstartAPI servantServer)

servantServer :: _
servantServer = servantServerHandlers toHandler quickstartServer

quickstartAPI :: Proxy _
quickstartAPI = packageAPI quickstartServer

instance FromJSON HelloRequest

instance FromJSON HiRequest

instance ToJSON HelloResponse

type instance
  AnnotatedPackage ServantRoute QuickStartService =
    '[ 'AnnService "Greeter" '["greet"],
       'AnnMethod "Greeter" "SayHello" '["say", "hello"],
       'AnnMethod "Greeter" "SayHi" '["say", "hi"],
       'AnnMethod "Greeter" "SayManyHellos" '["say", "many", "hellos"]
     ]

