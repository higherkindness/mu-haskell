{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TypeFamilies          #-}
{-# language UndecidableInstances  #-}
module Mu.GRpc.Bridge where

import           Data.ByteString
import           Data.Kind
import           Data.Proxy
import           Network.GRPC.HTTP2.Proto3Wire

import           Mu.GRpc.Avro

data GRpcMessageProtocol
  = MsgProtoBuf | MsgAvro
  deriving (Eq, Show)

msgProtoBuf :: Proxy 'MsgProtoBuf
msgProtoBuf = Proxy
msgAvro :: Proxy 'MsgAvro
msgAvro = Proxy

class MkRPC (p :: GRpcMessageProtocol) where
  type RPCTy p :: Type
  mkRPC :: Proxy p -> ByteString -> ByteString -> ByteString -> RPCTy p
instance MkRPC 'MsgProtoBuf where
  type RPCTy 'MsgProtoBuf = RPC
  mkRPC _ = RPC
instance MkRPC 'MsgAvro where
  type RPCTy 'MsgAvro = AvroRPC
  mkRPC _ = AvroRPC
