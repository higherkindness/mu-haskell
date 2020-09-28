{-# language DataKinds             #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}

module Main where

import qualified Data.Text.IO             as Text
import           Mu.Rpc.Annotations
import           Mu.Rpc.Examples
import           Mu.Schema.Annotations
import           Mu.Servant.Server
import           Mu.Server
import           Network.Wai.Handler.Warp
import           Servant

main :: IO ()
main = do
  putStrLn "running quickstart application"
  Text.putStrLn $ layout quickstartAPI
  run 8081 (serve quickstartAPI servantServer)

servantServer :: _
servantServer = servantServerHandlers toHandler quickstartServer

quickstartAPI :: Proxy _
quickstartAPI = packageAPI (quickstartServer @ServerErrorIO)

type instance
  AnnotatedPackage ServantRoute QuickStartService =
    '[ 'AnnService "Greeter" ('ServantTopLevelRoute '["greet"]),
       'AnnMethod "Greeter" "SayHello"
                  ('ServantRoute '["say", "hello"] 'POST 200),
       'AnnMethod "Greeter" "SayHi"
                  ('ServantRoute '["say", "hi"] 'POST 200),
       'AnnMethod "Greeter" "SayManyHellos"
                  ('ServantRoute '["say", "many", "hellos"] 'POST 200)
     ]

type instance
  AnnotatedSchema ServantContentTypes QuickstartSchema =
    '[ 'AnnType "HelloRequest" DefaultServantContentTypes,
       'AnnType "HelloResponse" DefaultServantContentTypes,
       'AnnType "HiRequest" DefaultServantContentTypes
     ]
