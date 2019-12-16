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

newtype ViaToProtoBufTypeRef (ref :: TypeRef) t
  = ViaToProtoBufTypeRef { unViaToProtoBufTypeRef :: t }
newtype ViaFromProtoBufTypeRef (ref :: TypeRef) t
  = ViaFromProtoBufTypeRef { unViaFromProtoBufTypeRef :: t }

instance ToProtoBufTypeRef ref t
         => Proto3WireEncoder (ViaToProtoBufTypeRef ref t) where
  proto3WireEncode = toProtoBufTypeRef (Proxy @ref) . unViaToProtoBufTypeRef
  proto3WireDecode = error "this should never be called, use FromProtoBufTypeRef"
instance FromProtoBufTypeRef ref t
         => Proto3WireEncoder (ViaFromProtoBufTypeRef ref t) where
  proto3WireEncode = error "this should never be called, use ToProtoBufTypeRef"
  proto3WireDecode = ViaFromProtoBufTypeRef <$> fromProtoBufTypeRef (Proxy @ref)

instance Proto3WireEncoder () where
  proto3WireEncode _ = mempty
  proto3WireDecode = return ()

class FromProtoBufTypeRef (ref :: TypeRef) t where
  fromProtoBufTypeRef :: Proxy ref -> PBDec.Parser PBDec.RawMessage t
class ToProtoBufTypeRef (ref :: TypeRef) t where
  toProtoBufTypeRef   :: Proxy ref -> t -> PBEnc.MessageBuilder

instance (IsProtoSchema Maybe sch sty, FromSchema Maybe sch sty t)
         => FromProtoBufTypeRef ('ViaSchema sch sty) t where
  fromProtoBufTypeRef _ = fromProtoViaSchema @_ @_ @sch
instance (IsProtoSchema Maybe sch sty, ToSchema Maybe sch sty t)
         => ToProtoBufTypeRef ('ViaSchema sch sty) t where
  toProtoBufTypeRef   _ = toProtoViaSchema @_ @_ @sch

instance ( FromProtoBufRegistry r t
         , IsProtoSchema Maybe (MappingRight r last) sty
         , FromSchema Maybe (MappingRight r last) sty t )
         => FromProtoBufTypeRef ('ViaRegistry r t last) t where
  fromProtoBufTypeRef _ = fromProtoBufWithRegistry @r
instance ( FromProtoBufRegistry r t
         , IsProtoSchema Maybe (MappingRight r last) sty
         , ToSchema Maybe (MappingRight r last) sty t )
         => ToProtoBufTypeRef ('ViaRegistry r t last) t where
  toProtoBufTypeRef   _ = toProtoViaSchema @_ @_ @(MappingRight r last)
