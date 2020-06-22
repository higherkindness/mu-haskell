{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-|
Description : Client for gRPC services using @TypeApplications@

For further information over initialization of the connection,
consult the <http://hackage.haskell.org/package/http2-client-grpc http2-client-grpc docs>.
-}
module Mu.GRpc.Client.TyApps (
  -- * Initialization of the gRPC client
  GrpcClient
, GrpcClientConfig
, grpcClientConfigSimple
, setupGrpcClient'
, setupGrpcClientZipkin
  -- * Call methods from the gRPC service
, gRpcCall
, GRpcMessageProtocol(..)
, CompressMode(..)
, GRpcReply(..)
) where

import           GHC.TypeLits
import           Network.GRPC.Client         (CompressMode (..))
import           Network.GRPC.Client.Helpers

import           Mu.Rpc
import           Mu.Schema

import           Mu.GRpc.Bridge
import           Mu.GRpc.Client.Internal

-- | Call a method from a Mu definition.
--   This method is thought to be used with @TypeApplications@:
--
--   > gRpcCall @'MsgFormat @"packageName" @ServiceDeclaration @"method"
--
--   The additional arguments you must provide to 'gRpcCall'
--   depend on the signature of the method itself:
--   * The resulting value is always wrapped in 'GRpcReply'.
--   * A single input or output turns into a single value.
--   * A streaming input or output turns into a Conduit.
gRpcCall :: forall (pro :: GRpcMessageProtocol) (pkg :: Package')
                   (srvName :: Symbol) (methodName :: Symbol) h
                   pkgName services methods.
            ( pkg ~  'Package ('Just pkgName) services
            , LookupService services srvName ~ 'Service srvName methods
            , GRpcServiceMethodCall pro pkgName srvName (LookupMethod methods methodName) h)
         => GrpcClient -> h
gRpcCall
  = gRpcServiceMethodCall (Proxy @pro) (Proxy @pkgName) (Proxy @srvName)
                          (Proxy @(LookupMethod methods methodName))
