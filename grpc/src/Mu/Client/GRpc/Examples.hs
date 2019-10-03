{-# language DataKinds, TypeApplications #-}
module Mu.Client.GRpc.Examples where

import qualified Data.Text as T
import Network.HTTP2.Client (HostName, PortNumber)

import Mu.Client.GRpc
import Mu.Rpc.Examples

sayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply T.Text)
sayHello' host port req
  = do Right c <- setupGrpcClient' (grpcClientConfigSimple host port False)
       (fmap (\(HelloResponse r) -> r)) <$> sayHello c (HelloRequest req)

sayHello :: GrpcClient -> HelloRequest -> IO (GRpcReply HelloResponse)
sayHello = gRpcCall @_ @_ @"helloworld" @QuickStartService @"SayHello"