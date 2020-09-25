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
    '[ 'AnnService "Greeter" ('ServantRoute '["greet"]),
       'AnnMethod "Greeter" "SayHello" ('ServantRoute '["say", "hello"]),
       'AnnMethod "Greeter" "SayHi" ('ServantRoute '["say", "hi"]),
       'AnnMethod "Greeter" "SayManyHellos" ('ServantRoute '["say", "many", "hellos"])
     ]

type instance
  AnnotatedPackage ServantMethod QuickStartService =
    '[]

type instance
  AnnotatedPackage ServantStatus QuickStartService =
    '[]

type instance
  AnnotatedSchema ServantUnaryContentTypes QuickstartSchema =
    '[ 'AnnType "HelloRequest" ('ServantUnaryContentTypes '[JSON]),
       'AnnType "HelloResponse" ('ServantUnaryContentTypes '[JSON]),
       'AnnType "HiRequest" ('ServantUnaryContentTypes '[JSON])
     ]

type instance
  AnnotatedSchema ServantStreamContentType QuickstartSchema =
    '[ 'AnnType "HelloRequest" ('ServantStreamContentType NewlineFraming JSON),
       'AnnType "HelloResponse" ('ServantStreamContentType NewlineFraming JSON),
       'AnnType "HiRequest" ('ServantStreamContentType NewlineFraming JSON)
     ]
