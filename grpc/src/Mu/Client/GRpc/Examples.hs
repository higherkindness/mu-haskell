{-# language DataKinds, TypeApplications #-}
module Mu.Client.GRpc.Examples where

import Data.Conduit
import Data.Conduit.Combinators as C
import Data.Conduit.List (consume)
import qualified Data.Text as T
import Network.HTTP2.Client (HostName, PortNumber)

import Mu.Client.GRpc.TyApps
import Mu.Rpc.Examples

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply T.Text)
sayHello' host port req
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       fmap (\(HelloResponse r) -> r) <$> sayHello c (HelloRequest req)

sayHello :: GrpcClient -> HelloRequest -> IO (GRpcReply HelloResponse)
sayHello = gRpcCall @QuickStartService @"SayHello"

sayHi' :: HostName -> PortNumber -> Int -> IO [GRpcReply T.Text]
sayHi' host port n
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       cndt <- sayHi c (HiRequest n)
       runConduit $ cndt .| C.map (fmap (\(HelloResponse r) -> r)) .| consume

sayHi :: GrpcClient -> HiRequest -> IO (ConduitT () (GRpcReply HelloResponse) IO ())
sayHi = gRpcCall @QuickStartService @"SayHi"