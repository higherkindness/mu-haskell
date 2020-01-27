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

type M a = a Maybe

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply (Maybe T.Text))
sayHello' host port req
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       fmap (\(HelloResponse r) -> r) <$> sayHello c (HelloRequest (Just req))

sayHello :: GrpcClient -> M HelloRequest -> IO (GRpcReply (M HelloResponse))
sayHello = gRpcCall @'MsgProtoBuf @QuickStartService @"SayHello"

sayHi' :: HostName -> PortNumber -> Int -> IO [GRpcReply (Maybe T.Text)]
sayHi' host port n
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       cndt <- sayHi c (HiRequest (Just n))
       runConduit $ cndt .| C.map (fmap (\(HelloResponse r) -> r)) .| consume

sayHi :: GrpcClient -> M HiRequest -> IO (ConduitT () (GRpcReply (M HelloResponse)) IO ())
sayHi = gRpcCall @'MsgProtoBuf @QuickStartService @"SayHi"
