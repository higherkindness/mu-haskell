{-# language PolyKinds, DataKinds, KindSignatures,
             MultiParamTypeClasses,
             ScopedTypeVariables, TypeApplications,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
module Mu.GRpc.Shared where

import qualified Proto3.Wire.Encode as PBEnc
import qualified Proto3.Wire.Decode as PBDec

import Mu.Rpc
import Mu.Schema
import Mu.Schema.Registry

import Mu.Schema.Adapter.ProtoBuf

class ProtoBufTypeRef (ref :: TypeRef) t where
  fromProtoBufTypeRef :: Proxy ref -> PBDec.Parser PBDec.RawMessage t
  toProtoBufTypeRef   :: Proxy ref -> t -> PBEnc.MessageBuilder

instance (HasProtoSchema sch sty t)
         => ProtoBufTypeRef ('FromSchema sch sty) t where
  fromProtoBufTypeRef _ = fromProtoViaSchema @sch
  toProtoBufTypeRef   _ = toProtoViaSchema @sch

instance ( FromProtoBufRegistry (Registry subject) t
         , HasProtoSchema (MappingRight (Registry subject) last) sty t)
         => ProtoBufTypeRef ('FromRegistry subject t last) t where
  fromProtoBufTypeRef _ = fromProtoBufWithRegistry @_ @subject
  toProtoBufTypeRef   _ = toProtoViaSchema @(MappingRight (Registry subject) last)