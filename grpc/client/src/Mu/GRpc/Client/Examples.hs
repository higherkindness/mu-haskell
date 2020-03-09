{-# language DataKinds        #-}
{-# language TypeApplications #-}
{-# language TypeFamilies     #-}
{-|
Description : Examples for gRPC clients

Look at the source code of this module.
-}
module Mu.GRpc.Client.Examples where

import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Conduit.List        (consume)
import qualified Data.Text                as T
import           Network.HTTP2.Client     (HostName, PortNumber)

import           Mu.Adapter.ProtoBuf
import           Mu.GRpc.Client.TyApps
import           Mu.Rpc.Examples
import           Mu.Schema

type instance AnnotatedSchema ProtoBufAnnotation QuickstartSchema
  = '[ 'AnnField "HelloRequest" "name" ('ProtoBufId 1)
     , 'AnnField "HelloResponse" "message" ('ProtoBufId 1)
     , 'AnnField "HiRequest" "number" ('ProtoBufId 1) ]

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply T.Text)
sayHello' host port req
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       fmap (\(HelloResponse r) -> r) <$> sayHello c (HelloRequest req)

sayHello :: GrpcClient -> HelloRequest -> IO (GRpcReply HelloResponse)
sayHello = gRpcCall @'MsgProtoBuf @QuickStartService @"Greeter" @"SayHello"

sayHi' :: HostName -> PortNumber -> Int -> IO [GRpcReply T.Text]
sayHi' host port n
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       cndt <- sayHi c (HiRequest n)
       runConduit $ cndt .| C.map (fmap (\(HelloResponse r) -> r)) .| consume

sayHi :: GrpcClient -> HiRequest -> IO (ConduitT () (GRpcReply HelloResponse) IO ())
sayHi = gRpcCall @'MsgProtoBuf @QuickStartService @"Greeter" @"SayHi"
