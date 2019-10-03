{-# language DataKinds, TypeApplications #-}
module Mu.Client.GRpc.Examples where

import Mu.Client.GRpc
import Mu.Rpc.Examples

sayHello :: GrpcClient -> HelloRequest -> IO (GRpcReply HelloResponse)
sayHello = gRpcCall @_ @_ @"helloworld" @QuickStartService @"SayHello"