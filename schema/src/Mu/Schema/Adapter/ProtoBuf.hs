{-# language PolyKinds, DataKinds, GADTs,
             TypeFamilies, TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, TypeApplications,
             UndecidableInstances,
             OverloadedStrings, ConstraintKinds,
             AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Schema.Adapter.ProtoBuf (
  -- * Custom annotations
  ProtoBufId
, ProtoBufOneOfIds
  -- * Conversion using schemas
, IsProtoSchema
, HasProtoSchema
, toProtoViaSchema
, fromProtoViaSchema
, parseProtoViaSchema
  -- * Conversion using registry
, FromProtoBufRegistry
, fromProtoBufWithRegistry
, parseProtoBufWithRegistry
) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Int
import Data.Kind
import Data.SOP (All)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits
import Proto3.Wire
import qualified Proto3.Wire.Encode as PBEnc
import qualified Proto3.Wire.Decode as PBDec

import Mu.Schema.Definition
import Mu.Schema.Interpretation
import Mu.Schema.Class
import qualified Mu.Schema.Registry as R

-- ANNOTATION FOR CONVERSION

data ProtoBufId (n :: Nat)
data ProtoBufOneOfIds (ns :: [Nat])

type family FindProtoBufId (f :: fn) (xs :: [Type]) :: Nat where
  FindProtoBufId f '[]
    = TypeError ('Text "protocol buffers id not available for field " ':<>: 'ShowType f)
  FindProtoBufId f (ProtoBufId n ': rest) = n
  FindProtoBufId f (other        ': rest) = FindProtoBufId f rest

type family FindProtoBufOneOfIds (f :: fn) (xs :: [Type]) :: [Nat] where
  FindProtoBufOneOfIds f '[]
    = TypeError ('Text "protocol buffers ids not available for oneof field " ':<>: 'ShowType f)
  FindProtoBufOneOfIds f (ProtoBufOneOfIds n ': rest) = n
  FindProtoBufOneOfIds f (other              ': rest) = FindProtoBufOneOfIds f rest 

-- CONVERSION USING SCHEMAS

class ProtoBridgeTerm sch (sch :/: sty) => IsProtoSchema sch sty
instance ProtoBridgeTerm sch (sch :/: sty) => IsProtoSchema sch sty

type HasProtoSchema sch sty a = (HasSchema sch sty a, IsProtoSchema sch sty)

toProtoViaSchema :: forall sch a sty.
                    (HasProtoSchema sch sty a)
                 => a -> PBEnc.MessageBuilder
toProtoViaSchema = termToProto . toSchema' @sch

fromProtoViaSchema :: forall sch a sty.
                      (HasProtoSchema sch sty a)
                   => PBDec.Parser PBDec.RawMessage a
fromProtoViaSchema = fromSchema' @sch <$> protoToTerm

parseProtoViaSchema :: forall sch a sty.
                       (HasProtoSchema sch sty a)
                    => BS.ByteString -> Either PBDec.ParseError a
parseProtoViaSchema = PBDec.parse (fromProtoViaSchema @sch)

-- CONVERSION USING REGISTRY

fromProtoBufWithRegistry
  :: forall (r :: R.Registry) t. 
     FromProtoBufRegistry r t
  => PBDec.Parser PBDec.RawMessage t
fromProtoBufWithRegistry = fromProtoBufRegistry' (Proxy @r)

parseProtoBufWithRegistry
  :: forall (r :: R.Registry) t. 
     FromProtoBufRegistry r t
  => BS.ByteString -> Either PBDec.ParseError t
parseProtoBufWithRegistry = PBDec.parse (fromProtoBufWithRegistry @r)

class FromProtoBufRegistry (ms :: Mappings Nat Schema') t where
  fromProtoBufRegistry' :: Proxy ms -> PBDec.Parser PBDec.RawMessage t

instance FromProtoBufRegistry '[] t where
  fromProtoBufRegistry' _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "no schema found in registry"))
instance (HasProtoSchema s sty t, FromProtoBufRegistry ms t)
         => FromProtoBufRegistry ( (n ':-> s) ': ms) t where
  fromProtoBufRegistry' _ = fromProtoViaSchema @s <|> fromProtoBufRegistry' (Proxy @ms)


-- =======================================
-- IMPLEMENTATION OF GENERIC SERIALIZATION
-- =======================================

instance Alternative (PBDec.Parser i) where
  empty = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "cannot parse"))
  PBDec.Parser x <|> PBDec.Parser y
    = PBDec.Parser $ \i -> case x i of
                             Left _      -> y i
                             r@(Right _) -> r

-- Top-level terms
class ProtoBridgeTerm (sch :: Schema tn fn) (t :: TypeDef tn fn) where
  termToProto :: Term sch t -> PBEnc.MessageBuilder
  protoToTerm :: PBDec.Parser PBDec.RawMessage (Term sch t)

-- Embedded terms
class ProtoBridgeEmbedTerm (sch :: Schema tn fn) (t :: TypeDef tn fn) where
  termToEmbedProto :: FieldNumber -> Term sch t -> PBEnc.MessageBuilder
  embedProtoToFieldValue :: PBDec.Parser PBDec.RawField (Term sch t)
  embedProtoToOneFieldValue :: PBDec.Parser PBDec.RawPrimitive (Term sch t)

class ProtoBridgeField (sch :: Schema tn fn) (f :: FieldDef tn fn) where
  fieldToProto :: Field sch f -> PBEnc.MessageBuilder
  protoToField :: PBDec.Parser PBDec.RawMessage (Field sch f)

class ProtoBridgeFieldValue (sch :: Schema tn fn) (t :: FieldType tn) where
  fieldValueToProto :: FieldNumber -> FieldValue sch t -> PBEnc.MessageBuilder
  protoToFieldValue :: PBDec.Parser PBDec.RawField (FieldValue sch t)

class ProtoBridgeOneFieldValue (sch :: Schema tn fn) (t :: FieldType tn) where
  protoToOneFieldValue :: PBDec.Parser PBDec.RawPrimitive (FieldValue sch t)

class ProtoBridgeUnionFieldValue (ids :: [Nat]) (sch :: Schema tn fn) (ts :: [FieldType tn]) where
  unionFieldValueToProto :: NS (FieldValue sch) ts -> PBEnc.MessageBuilder
  protoToUnionFieldValue :: PBDec.Parser PBDec.RawMessage (NS (FieldValue sch) ts)

-- --------
-- TERMS --
-- --------

-- RECORDS
-- -------

instance (All (ProtoBridgeField sch) args, ProtoBridgeFields sch args)
         => ProtoBridgeTerm sch ('DRecord name anns args) where
  termToProto (TRecord fields) = go fields
    where go :: forall fs. All (ProtoBridgeField sch) fs
             => NP (Field sch) fs -> PBEnc.MessageBuilder
          go Nil = mempty
          go (f :* fs) = fieldToProto f <> go fs
  protoToTerm = TRecord <$> protoToFields

class ProtoBridgeFields sch fields where
  protoToFields :: PBDec.Parser PBDec.RawMessage (NP (Field sch) fields)
instance ProtoBridgeFields sch '[] where
  protoToFields = pure Nil
instance (ProtoBridgeField sch f, ProtoBridgeFields sch fs)
         => ProtoBridgeFields sch (f ': fs) where
  protoToFields = (:*) <$> protoToField <*> protoToFields

instance ProtoBridgeTerm sch ('DRecord name anns args)
         => ProtoBridgeEmbedTerm sch ('DRecord name anns args) where
  termToEmbedProto fid v = PBEnc.embedded fid (termToProto v)
  embedProtoToFieldValue = do
    t <- PBDec.embedded (protoToTerm @_ @_ @sch @('DRecord name anns args))
    case t of
      Nothing -> PBDec.Parser (\_ -> Left (PBDec.WireTypeError "expected message"))
      Just v  -> return v
  embedProtoToOneFieldValue = PBDec.embedded' (protoToTerm @_ @_ @sch @('DRecord name anns args))

-- ENUMERATIONS
-- ------------

instance TypeError ('Text "protobuf requires wrapping enums in a message")
         => ProtoBridgeTerm sch ('DEnum name anns choices) where
  termToProto = error "protobuf requires wrapping enums in a message"
  protoToTerm = error "protobuf requires wrapping enums in a message"

instance ProtoBridgeEnum choices
         => ProtoBridgeEmbedTerm sch ('DEnum name anns choices) where
  termToEmbedProto fid (TEnum v) = enumToProto fid v
  embedProtoToFieldValue    = do n <- PBDec.one PBDec.int32 0
                                 TEnum <$> protoToEnum n
  embedProtoToOneFieldValue = do n <- PBDec.int32
                                 TEnum <$> protoToEnum n

class ProtoBridgeEnum (choices :: [ChoiceDef fn]) where
  enumToProto :: FieldNumber -> NS Proxy choices -> PBEnc.MessageBuilder
  protoToEnum :: Int32 -> PBDec.Parser a (NS Proxy choices)
instance ProtoBridgeEnum '[] where
  enumToProto = error "empty enum"
  protoToEnum _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "unknown enum type"))
instance (KnownNat (FindProtoBufId c anns), ProtoBridgeEnum cs)
         => ProtoBridgeEnum ('ChoiceDef c anns ': cs) where
  enumToProto fid (Z _) = PBEnc.int32 fid enumValue
    where enumValue = fromIntegral (natVal (Proxy @(FindProtoBufId c anns)))
  enumToProto fid (S v) = enumToProto fid v
  protoToEnum n
    | n == enumValue = return (Z Proxy)
    | otherwise      = S <$> protoToEnum n
    where enumValue = fromIntegral (natVal (Proxy @(FindProtoBufId c anns)))

-- SIMPLE
-- ------

instance TypeError ('Text "protobuf requires wrapping primitives in a message")
         => ProtoBridgeTerm sch ('DSimple t) where
  termToProto = error "protobuf requires wrapping primitives in a message"
  protoToTerm = error "protobuf requires wrapping primitives in a message"

-- ---------
-- FIELDS --
-- ---------

instance {-# OVERLAPPABLE #-}
         (ProtoBridgeFieldValue sch t, KnownNat (FindProtoBufId name anns))
         => ProtoBridgeField sch ('FieldDef name anns t) where
  fieldToProto (Field v) = fieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId name anns))
  protoToField = Field <$> protoToFieldValue `at` fieldId
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId name anns))

instance {-# OVERLAPS #-}
         (ProtoBridgeUnionFieldValue (FindProtoBufOneOfIds name anns) sch ts)
         => ProtoBridgeField sch ('FieldDef name anns ('TUnion ts)) where
  fieldToProto (Field (FUnion v)) = unionFieldValueToProto @_ @_ @(FindProtoBufOneOfIds name anns) v
  protoToField = Field . FUnion <$> protoToUnionFieldValue @_ @_ @(FindProtoBufOneOfIds name anns)

-- ------------------
-- TYPES OF FIELDS --
-- ------------------

-- SCHEMATIC
-- ---------

instance ProtoBridgeEmbedTerm sch (sch :/: t)
         => ProtoBridgeFieldValue sch ('TSchematic t) where
  fieldValueToProto fid (FSchematic v) = termToEmbedProto fid v
  protoToFieldValue = FSchematic <$> embedProtoToFieldValue
instance ProtoBridgeEmbedTerm sch (sch :/: t)
         => ProtoBridgeOneFieldValue sch ('TSchematic t) where
  protoToOneFieldValue = FSchematic <$> embedProtoToOneFieldValue

-- PRIMITIVE TYPES
-- ---------------

instance TypeError ('Text "null cannot be converted to protobuf")
         => ProtoBridgeFieldValue sch 'TNull where
  fieldValueToProto = error "null cannot be converted to protobuf"
  protoToFieldValue = error "null cannot be converted to protobuf"
instance TypeError ('Text "null cannot be converted to protobuf")
         => ProtoBridgeOneFieldValue sch 'TNull where
  protoToOneFieldValue = error "null cannot be converted to protobuf"

instance ProtoBridgeFieldValue sch ('TPrimitive Int) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.int32 fid (fromIntegral n)
  protoToFieldValue = FPrimitive . fromIntegral <$> PBDec.one PBDec.int32 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Int) where
  protoToOneFieldValue = FPrimitive . fromIntegral <$> PBDec.int32

instance ProtoBridgeFieldValue sch ('TPrimitive Int32) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.int32 fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.int32 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Int32) where
  protoToOneFieldValue = FPrimitive <$> PBDec.int32

instance ProtoBridgeFieldValue sch ('TPrimitive Int64) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.int64 fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.int64 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Int64) where
  protoToOneFieldValue = FPrimitive <$> PBDec.int64

-- WARNING! These instances may go out of bounds
instance ProtoBridgeFieldValue sch ('TPrimitive Integer) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.int64 fid (fromInteger n)
  protoToFieldValue = FPrimitive . fromIntegral <$> PBDec.one PBDec.int64 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Integer) where
  protoToOneFieldValue = FPrimitive . fromIntegral <$> PBDec.int64

instance ProtoBridgeFieldValue sch ('TPrimitive Float) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.float fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.float 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Float) where
  protoToOneFieldValue = FPrimitive <$> PBDec.float

instance ProtoBridgeFieldValue sch ('TPrimitive Double) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.double fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.double 0
instance ProtoBridgeOneFieldValue sch ('TPrimitive Double) where
  protoToOneFieldValue = FPrimitive <$> PBDec.double

instance ProtoBridgeFieldValue sch ('TPrimitive Bool) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.enum fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.bool False
instance ProtoBridgeOneFieldValue sch ('TPrimitive Bool) where
  protoToOneFieldValue = FPrimitive <$> PBDec.bool

instance ProtoBridgeFieldValue sch ('TPrimitive T.Text) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.text fid (LT.fromStrict n)
  protoToFieldValue = FPrimitive . LT.toStrict <$> PBDec.one PBDec.text ""
instance ProtoBridgeOneFieldValue sch ('TPrimitive T.Text) where
  protoToOneFieldValue = FPrimitive . LT.toStrict <$> PBDec.text

instance ProtoBridgeFieldValue sch ('TPrimitive LT.Text) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.text fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.text ""
instance ProtoBridgeOneFieldValue sch ('TPrimitive LT.Text) where
  protoToOneFieldValue = FPrimitive <$> PBDec.text

instance ProtoBridgeFieldValue sch ('TPrimitive BS.ByteString) where
  fieldValueToProto fid (FPrimitive n) = PBEnc.byteString fid n
  protoToFieldValue = FPrimitive <$> PBDec.one PBDec.byteString ""
instance ProtoBridgeOneFieldValue sch ('TPrimitive BS.ByteString) where
  protoToOneFieldValue = FPrimitive <$> PBDec.byteString

-- Note that Maybes and Lists require that we recur on the OneFieldValue class

instance (ProtoBridgeFieldValue sch t, ProtoBridgeOneFieldValue sch t)
         => ProtoBridgeFieldValue sch ('TOption t) where
  fieldValueToProto _   (FOption Nothing)  = mempty
  fieldValueToProto fid (FOption (Just v)) = fieldValueToProto fid v
  protoToFieldValue = FOption <$> PBDec.one (Just <$> protoToOneFieldValue) Nothing

instance TypeError ('Text "optionals cannot be nested in protobuf")
         => ProtoBridgeOneFieldValue sch ('TOption t) where
  protoToOneFieldValue = error "optionals cannot be nested in protobuf"

instance (ProtoBridgeFieldValue sch t, ProtoBridgeOneFieldValue sch t)
         => ProtoBridgeFieldValue sch ('TList t) where
  fieldValueToProto fid (FList xs) = foldMap (fieldValueToProto fid) xs
  protoToFieldValue = FList <$> PBDec.repeated protoToOneFieldValue

instance TypeError ('Text "lists cannot be nested in protobuf")
         => ProtoBridgeOneFieldValue sch ('TList t) where
  protoToOneFieldValue = error "lists cannot be nested in protobuf"

instance TypeError ('Text "maps are not currently supported")
         => ProtoBridgeFieldValue sch ('TMap k v) where
  fieldValueToProto = error "maps are not currently supported"
  protoToFieldValue = error "maps are not currently supported"

instance TypeError ('Text "nested unions are not currently supported")
         => ProtoBridgeFieldValue sch ('TUnion choices) where
  fieldValueToProto = error "nested unions are not currently supported"
  protoToFieldValue = error "nested unions are not currently supported"

-- UNIONS
-- ------

instance ProtoBridgeUnionFieldValue ids sch '[] where
  unionFieldValueToProto = error "empty list of unions"
  protoToUnionFieldValue = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "unknown type in an union"))

instance ( ProtoBridgeFieldValue sch t, KnownNat thisId
         , ProtoBridgeUnionFieldValue restIds sch ts )
         => ProtoBridgeUnionFieldValue (thisId ': restIds) sch (t ': ts) where
  unionFieldValueToProto (Z v) = fieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @thisId)
  unionFieldValueToProto (S v) = unionFieldValueToProto @_ @_ @restIds v
  protoToUnionFieldValue
    = Z <$> protoToFieldValue `at` fieldId <|> S <$> protoToUnionFieldValue @_ @_ @restIds
    where fieldId = fromInteger $ natVal (Proxy @thisId)