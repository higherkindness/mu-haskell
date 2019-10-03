{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, TypeApplications,
             TypeOperators, DeriveFunctor,
             AllowAmbiguousTypes #-}
module Mu.Client.GRpc (
  GrpcClient
, GrpcClientConfig
, grpcClientConfigSimple
, setupGrpcClient'
, gRpcCall
, GRpcReply(..)
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.TypeLits
import Network.HTTP2 (ErrorCode)
import Network.HTTP2.Client (ClientIO, TooMuchConcurrency, ClientError, runExceptT)
import Network.GRPC.Proto3Wire.Client (RPC(..), RawReply)
import Network.GRPC.Proto3Wire.Client.Helpers

import Mu.Rpc
import Mu.Schema
import Mu.Schema.Adapter.ProtoBuf

setupGrpcClient' :: GrpcClientConfig -> IO (Either ClientError GrpcClient)
setupGrpcClient' = runExceptT . setupGrpcClient

-- | Call a method from a `mu-rpc` definition.
--   This method is thought to be used with `TypeApplications`:
--   > gRpcCall @"packageName" @ServiceDeclaration @"method" 
gRpcCall :: forall (pkg :: Symbol) (s :: Service snm mnm) (methodName :: mnm) h.
            (KnownName pkg, GRpcServiceMethodCall s (s :-->: methodName) h)
         => GrpcClient -> h
gRpcCall = gRpcServiceMethodCall pkgName (Proxy @s) (Proxy @(s :-->: methodName))
  where pkgName = BS.pack (nameVal (Proxy @pkg))

class GRpcServiceMethodCall (s :: Service snm mnm) (m :: Method mnm) h where
  gRpcServiceMethodCall :: ByteString -> Proxy s -> Proxy m -> GrpcClient -> h
instance (KnownName serviceName, GRpcMethodCall m h)
         => GRpcServiceMethodCall ('Service serviceName methods) m h where
  gRpcServiceMethodCall pkgName _ = gRpcMethodCall pkgName svrName
    where svrName = BS.pack (nameVal (Proxy @serviceName))

data GRpcReply a
  = GRpcTooMuchConcurrency TooMuchConcurrency
  | GRpcErrorCode ErrorCode
  | GRpcErrorString String
  | GRpcClientError ClientError
  | GRpcOk a
  deriving (Show, Functor)

buildGRpcReply :: Either TooMuchConcurrency (RawReply a) -> GRpcReply a
buildGRpcReply (Left tmc) = GRpcTooMuchConcurrency tmc
buildGRpcReply (Right (Left ec)) = GRpcErrorCode ec
buildGRpcReply (Right (Right (_, _, Left es))) = GRpcErrorString es
buildGRpcReply (Right (Right (_, _, Right r))) = GRpcOk r

simplifyResponse :: ClientIO (GRpcReply a) -> IO (GRpcReply a)
simplifyResponse reply = do
  r <- runExceptT reply
  case r of
    Left e  -> return $ GRpcClientError e
    Right v -> return v

class GRpcMethodCall method h where
  gRpcMethodCall :: ByteString -> ByteString -> Proxy method -> GrpcClient -> h

instance (KnownName name, HasProtoSchema vsch vty v, HasProtoSchema rsch rty r)
         => GRpcMethodCall ('Method name '[ 'ArgSingle vsch vty ] ('RetSingle rsch rty))
                           (v -> IO (GRpcReply r)) where
  gRpcMethodCall pkgName srvName _ client x
    = simplifyResponse $ 
      buildGRpcReply <$>
      rawUnary (toProtoViaSchema @vsch, fromProtoViaSchema @rsch) rpc client x
    where methodName = BS.pack (nameVal (Proxy @name))
          rpc = RPC pkgName srvName methodName