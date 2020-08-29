{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson
import Data.Conduit
import qualified Data.Text.IO as Text
import Mu.Rpc.Annotations
import Mu.Rpc.Examples
import Mu.Servant.Server
import Mu.Server
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = do
  putStrLn "running quickstart application"
  Text.putStrLn $ layout (Proxy @API)
  run 8081 (serve (Proxy @API) servantServer)

servantServer :: Server (PackageAPI QuickStartService (Handlers ServerErrorIO))
servantServer = servantServerHandlers toHandler quickstartServer

type API = PackageAPI QuickStartService (Handlers ServerErrorIO)

type Handlers m =
  '[  '[ HelloRequest -> m HelloResponse,
         HiRequest -> ConduitT HelloResponse Void m () -> m (),
         ConduitT () HelloRequest m () -> ConduitT HelloResponse Void m () -> m ()
       ]
   ]

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
