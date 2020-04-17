{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TypeFamilies          #-}
{-# language UndecidableInstances  #-}
{-|
Description : Supported serialization formats for gRPC

Currently Protocol Buffers and Avro can be used as
serialization format for the messages in gRPC
requests and replies. This module provides types
and proxies used in both @mu-grpc-client@ and
@mu-grpc-server@ to drive this choice of serialization.
-}
module Mu.GRpc.Bridge where

import           Data.ByteString
import           Data.Kind
import           Data.Proxy
import           Network.GRPC.HTTP2.Proto3Wire

import           Mu.GRpc.Avro

-- | Serialization formats supported with gRPC.
data GRpcMessageProtocol
  = MsgProtoBuf  -- ^ Protocol Buffers.
  | MsgAvro      -- ^ Avro.
  deriving (Eq, Show)

-- | Choose Protocol Buffers as serialization format for gRPC.
--   This value is commonly used to create a client or server.
msgProtoBuf :: Proxy 'MsgProtoBuf
msgProtoBuf = Proxy
-- | Choose Avro as serialization format for gRPC.
--   This value is commonly used to create a client or server.
msgAvro :: Proxy 'MsgAvro
msgAvro = Proxy

-- | Defines how to build serialization-specific
--   RPC locators from a triple of (package, server, method).
class MkRPC (p :: GRpcMessageProtocol) where
  type RPCTy p :: Type
  mkRPC :: Proxy p -> ByteString -> ByteString -> ByteString -> RPCTy p
instance MkRPC 'MsgProtoBuf where
  type RPCTy 'MsgProtoBuf = RPC
  mkRPC _ = RPC
instance MkRPC 'MsgAvro where
  type RPCTy 'MsgAvro = AvroRPC
  mkRPC _ = AvroRPC
