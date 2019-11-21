{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints -fno-warn-orphans #-}
module Mu.Adapter.ProtoBuf.Via where

import           Network.GRPC.HTTP2.Proto3Wire
import qualified Proto3.Wire.Decode            as PBDec
import qualified Proto3.Wire.Encode            as PBEnc

import           Mu.Adapter.ProtoBuf
import           Mu.Rpc
import           Mu.Schema

newtype ViaProtoBufTypeRef (ref :: TypeRef) t
  = ViaProtoBufTypeRef { unViaProtoBufTypeRef :: t }

instance ProtoBufTypeRef ref t
         => Proto3WireEncoder (ViaProtoBufTypeRef ref t) where
  proto3WireEncode = toProtoBufTypeRef (Proxy @ref) . unViaProtoBufTypeRef
  proto3WireDecode = ViaProtoBufTypeRef <$> fromProtoBufTypeRef (Proxy @ref)

instance Proto3WireEncoder () where
  proto3WireEncode _ = mempty
  proto3WireDecode = return ()

class ProtoBufTypeRef (ref :: TypeRef) t where
  fromProtoBufTypeRef :: Proxy ref -> PBDec.Parser PBDec.RawMessage t
  toProtoBufTypeRef   :: Proxy ref -> t -> PBEnc.MessageBuilder

instance (HasProtoSchema sch sty t)
         => ProtoBufTypeRef ('FromSchema sch sty) t where
  fromProtoBufTypeRef _ = fromProtoViaSchema @_ @_ @sch
  toProtoBufTypeRef   _ = toProtoViaSchema @_ @_ @sch

instance ( FromProtoBufRegistry r t
         , HasProtoSchema (MappingRight r last) sty t)
         => ProtoBufTypeRef ('FromRegistry r t last) t where
  fromProtoBufTypeRef _ = fromProtoBufWithRegistry @r
  toProtoBufTypeRef   _ = toProtoViaSchema @_ @_ @(MappingRight r last)
