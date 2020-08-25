{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Aeson
import Data.Conduit
import Mu.Rpc.Examples
import Mu.Servant.Server
import Mu.Server
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = do
  putStrLn "running quickstart application"
  run 8081 (serve (Proxy @API) servantServer)

servantServer :: Server (PackageAPI QuickStartService (Handlers ServerErrorIO))
servantServer = servantServerHandlers toHandler quickstartServer

type API = PackageAPI QuickStartService (Handlers ServerErrorIO)

type Handlers m =
  '[  '[ HelloRequest -> m HelloResponse,
         HiRequest -> ConduitT HelloResponse Void m () -> m (),
         ConduitT () HelloRequest m () ->
         ConduitT HelloResponse Void m () ->
         m ()
       ]
   ]

instance FromJSON HelloRequest

instance FromJSON HiRequest

instance ToJSON HelloResponse
--deriving instance Generic HelloResponse
{-
    sayHello :: HelloRequest -> m HelloResponse
    sayHello (HelloRequest nm)
      = pure $ HelloResponse $ "hi, " <> nm
    sayHi :: HiRequest
          -> ConduitT HelloResponse Void m ()
          -> m ()
    sayHi (HiRequest n) sink
      = runConduit $ C.replicate n (HelloResponse "hi!") .| sink
    sayManyHellos :: ConduitT () HelloRequest m ()
                  -> ConduitT HelloResponse Void m ()
                  -> m ()
    sayManyHellos source sink
      = runConduit $ source .| C.mapM sayHello .| sink
-}
