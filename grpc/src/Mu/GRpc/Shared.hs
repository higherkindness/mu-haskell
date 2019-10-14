{-# language PolyKinds, DataKinds,
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

import Mu.Schema.Adapter.ProtoBuf

class ProtoBufTypeRef (ref :: TypeRef) t where
  fromProtoBufTypeRef :: Proxy ref -> PBDec.Parser PBDec.RawMessage t
  toProtoBufTypeRef   :: Proxy ref -> t -> PBEnc.MessageBuilder

unitFromProtoBuf :: PBDec.Parser PBDec.RawMessage ()
unitFromProtoBuf = return ()
unitToProtoBuf :: () -> PBEnc.MessageBuilder
unitToProtoBuf _ = mempty

instance (HasProtoSchema sch sty t)
         => ProtoBufTypeRef ('FromSchema sch sty) t where
  fromProtoBufTypeRef _ = fromProtoViaSchema @sch
  toProtoBufTypeRef   _ = toProtoViaSchema @sch

instance ( FromProtoBufRegistry r t
         , HasProtoSchema (MappingRight r last) sty t)
         => ProtoBufTypeRef ('FromRegistry r t last) t where
  fromProtoBufTypeRef _ = fromProtoBufWithRegistry @r
  toProtoBufTypeRef   _ = toProtoViaSchema @(MappingRight r last)