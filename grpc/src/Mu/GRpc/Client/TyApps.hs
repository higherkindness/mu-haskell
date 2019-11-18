{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses, FlexibleContexts,
             ScopedTypeVariables, TypeApplications,
             TypeOperators, AllowAmbiguousTypes #-}
-- | Client for gRPC services defined using Mu 'Service'
--   using 'TypeApplications'
module Mu.GRpc.Client.TyApps (
  -- * Initialization of the gRPC client
  GrpcClient
, GrpcClientConfig
, grpcClientConfigSimple
, setupGrpcClient'
  -- * Call methods from the gRPC service
, gRpcCall
, CompressMode(..)
, GRpcReply(..)
) where

import Network.GRPC.Client (CompressMode(..))
import Network.GRPC.Client.Helpers

import Mu.Rpc
import Mu.Schema

import Mu.GRpc.Client.Internal

-- | Call a method from a Mu definition.
--   This method is thought to be used with @TypeApplications@:
--   > gRpcCall @"packageName" @ServiceDeclaration @"method" 
--  
--   The additional arguments you must provide to 'grpcCall'
--   depend on the signature of the method itself:
--   * The resulting value is always wrapped in 'GRpcReply'.
--   * A 'Single' input or output turns into a single value.
--   * A 'Stream' input or output turns into a 'ConduitT'
gRpcCall :: forall s methodName h.
            (GRpcServiceMethodCall s (s :-->: methodName) h)
         => GrpcClient -> h
gRpcCall = gRpcServiceMethodCall (Proxy @s) (Proxy @(s :-->: methodName))