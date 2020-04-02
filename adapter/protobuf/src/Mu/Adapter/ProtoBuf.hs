{-# language AllowAmbiguousTypes   #-}
{-# language CPP                   #-}
{-# language ConstraintKinds       #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description : Adapter for Protocol Buffers serialization

Just import the module and you can turn any
value with a 'ToSchema' and 'FromSchema' from
and to Protocol Buffers. Since Protocol Buffers
need information about field identifiers, you
need to annotate your schema using 'ProtoBufAnnotation'.
-}
module Mu.Adapter.ProtoBuf (
  -- * Custom annotations
  ProtoBufAnnotation(..)
  -- * Conversion using schemas
, IsProtoSchema
, toProtoViaSchema
, fromProtoViaSchema
, parseProtoViaSchema
  -- * Conversion using registry
, FromProtoBufRegistry
, fromProtoBufWithRegistry
, parseProtoBufWithRegistry
) where

import           Control.Applicative
import qualified Data.ByteString          as BS
import           Data.Int
import           Data.SOP                 (All)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           GHC.TypeLits
import           Proto3.Wire
import qualified Proto3.Wire.Decode       as PBDec
import qualified Proto3.Wire.Encode       as PBEnc

import           Mu.Schema.Annotations
import           Mu.Schema.Class
import           Mu.Schema.Definition
import           Mu.Schema.Interpretation
import qualified Mu.Schema.Registry       as R

#if MIN_VERSION_proto3_wire(1,1,0)
instance ProtoEnum Bool
#endif

-- | Annotations for Protocol Buffers fields.
data ProtoBufAnnotation
  = -- | Numeric field identifier for normal fields
    --   and whether it should be packed (only used for lists of number-like values)
    ProtoBufId Nat Bool
    -- | List of identifiers for fields which contain a union
  | ProtoBufOneOfIds [Nat]

type family FindProtoBufId (sch :: Schema tn fn) (t :: tn) (f :: fn) where
  FindProtoBufId sch t f
    = FindProtoBufId' t f (GetFieldAnnotation (AnnotatedSchema ProtoBufAnnotation sch) t f)

type family FindProtoBufId' (t :: tn) (f :: fn) (p :: ProtoBufAnnotation) :: Nat where
  FindProtoBufId' t f ('ProtoBufId n b) = n
  FindProtoBufId' t f other
    = TypeError ('Text "protocol buffers id not available for field "
                 ':<>: 'ShowType t ':<>: 'Text "/" ':<>: 'ShowType f)

type family FindProtoBufPacked (sch :: Schema tn fn) (t :: tn) (f :: fn) where
  FindProtoBufPacked sch t f
    = FindProtoBufPacked' t f (GetFieldAnnotation (AnnotatedSchema ProtoBufAnnotation sch) t f)

type family FindProtoBufPacked' (t :: tn) (f :: fn) (p :: ProtoBufAnnotation) :: Bool where
  FindProtoBufPacked' t f ('ProtoBufId n b) = b
  FindProtoBufPacked' t f other
    = TypeError ('Text "protocol buffers id not available for field "
                 ':<>: 'ShowType t ':<>: 'Text "/" ':<>: 'ShowType f)

type family FindProtoBufOneOfIds (sch :: Schema tn fn) (t :: tn) (f :: fn) where
  FindProtoBufOneOfIds sch t f
    = FindProtoBufOneOfIds' t f (GetFieldAnnotation (AnnotatedSchema ProtoBufAnnotation sch) t f)

type family FindProtoBufOneOfIds' (t :: tn) (f :: fn) (p :: ProtoBufAnnotation) :: [Nat] where
  FindProtoBufOneOfIds' t f ('ProtoBufOneOfIds ns) = ns
  FindProtoBufOneOfIds' t f other
    = TypeError ('Text "protocol buffers id not available for oneof field "
                 ':<>: 'ShowType t ':<>: 'Text "/" ':<>: 'ShowType f)

-- CONVERSION USING SCHEMAS

-- | Represents those 'Schema's which are supported by Protocol Buffers.
--   Some values which can be represented as 'Term's cannot be so in
--   Protocol Buffers. For example, you cannot have a list within an option.
class ProtoBridgeTerm sch (sch :/: sty) => IsProtoSchema sch sty
instance ProtoBridgeTerm sch (sch :/: sty) => IsProtoSchema sch sty

-- type HasProtoSchema w sch sty a = (HasSchema w sch sty a, IsProtoSchema w sch sty)

-- | Conversion to Protocol Buffers mediated by a schema.
toProtoViaSchema :: forall t f (sch :: Schema t f) a sty.
                    (IsProtoSchema sch sty, ToSchema sch sty a)
                 => a -> PBEnc.MessageBuilder
toProtoViaSchema = termToProto . toSchema' @_ @_ @sch

-- | Conversion from Protocol Buffers mediated by a schema.
--   This function requires a 'PBDec.RawMessage', which means
--   that we already know that the Protocol Buffers message
--   is well-formed. Use 'parseProtoViaSchema' to parse directly
--   from a 'BS.ByteString'.
fromProtoViaSchema :: forall t f (sch :: Schema t f) a sty.
                      (IsProtoSchema sch sty, FromSchema sch sty a)
                   => PBDec.Parser PBDec.RawMessage a
fromProtoViaSchema = fromSchema' @_ @_ @sch <$> protoToTerm

-- | Conversion from Protocol Buffers mediated by a schema.
--   This function receives the 'BS.ByteString' directly,
--   and parses it as part of its duty.
parseProtoViaSchema :: forall sch a sty.
                       (IsProtoSchema sch sty, FromSchema sch sty a)
                    => BS.ByteString -> Either PBDec.ParseError a
parseProtoViaSchema = PBDec.parse (fromProtoViaSchema @_ @_ @sch)

-- CONVERSION USING REGISTRY

-- | Conversion from Protocol Buffers by checking
--   all the 'Schema's in a 'R.Registry'.
--
--   As 'fromProtoViaSchema', this version requires
--   an already well-formed Protocol Buffers message.
fromProtoBufWithRegistry
  :: forall (r :: R.Registry) t.
     FromProtoBufRegistry r t
  => PBDec.Parser PBDec.RawMessage t
fromProtoBufWithRegistry = fromProtoBufRegistry' (Proxy @r)

-- | Conversion from Protocol Buffers by checking
--   all the 'Schema's in a 'R.Registry'.
--
--   As 'parseProtoViaSchema', this version receives
--   a 'BS.ByteString' and parses it as part of its duty.
parseProtoBufWithRegistry
  :: forall (r :: R.Registry) t.
     FromProtoBufRegistry r t
  => BS.ByteString -> Either PBDec.ParseError t
parseProtoBufWithRegistry = PBDec.parse (fromProtoBufWithRegistry @r)

-- | Represents 'R.Registry's for which every 'Schema'
--   is supported by the Protocol Buffers format.
class FromProtoBufRegistry (ms :: Mappings Nat Schema') t where
  fromProtoBufRegistry' :: Proxy ms -> PBDec.Parser PBDec.RawMessage t

instance FromProtoBufRegistry '[] t where
  fromProtoBufRegistry' _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "no schema found in registry"))
instance (IsProtoSchema s sty, FromSchema s sty t, FromProtoBufRegistry ms t)
         => FromProtoBufRegistry ( (n ':-> s) ': ms) t where
  fromProtoBufRegistry' _ = fromProtoViaSchema @_ @_ @s <|> fromProtoBufRegistry' (Proxy @ms)


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
  embedProtoToOneFieldValue :: PBDec.Parser PBDec.RawPrimitive (Term sch t)
  -- support for packed encodings
  -- https://developers.google.com/protocol-buffers/docs/encoding#packed
  supportsPackingTerm :: Proxy (Term sch t) -> Bool
  termToPackedEmbedProto :: FieldNumber -> [Term sch t] -> PBEnc.MessageBuilder
  embedProtoToPackedFieldValue :: PBDec.Parser PBDec.RawPrimitive [Term sch t]

class ProtoBridgeField (sch :: Schema tn fn) (ty :: tn) (f :: FieldDef tn fn) where
  fieldToProto :: Field sch f -> PBEnc.MessageBuilder
  protoToField :: PBDec.Parser PBDec.RawMessage (Field sch f)

class ProtoBridgeOneFieldValue (sch :: Schema tn fn) (t :: FieldType tn) where
  defaultOneFieldValue :: Maybe (FieldValue sch t)
  oneFieldValueToProto :: FieldNumber -> FieldValue sch t -> PBEnc.MessageBuilder
  protoToOneFieldValue :: PBDec.Parser PBDec.RawPrimitive (FieldValue sch t)
  -- support for packed encodings
  -- https://developers.google.com/protocol-buffers/docs/encoding#packed
  supportsPacking         :: Proxy (FieldValue sch t) -> Bool
  packedFieldValueToProto :: FieldNumber -> [FieldValue sch t] -> PBEnc.MessageBuilder
  protoToPackedFieldValue :: PBDec.Parser PBDec.RawPrimitive [FieldValue sch t]

class ProtoBridgeUnionFieldValue (ids :: [Nat]) (sch :: Schema tn fn) (ts :: [FieldType tn]) where
  unionFieldValueToProto :: NS (FieldValue sch) ts -> PBEnc.MessageBuilder
  protoToUnionFieldValue :: PBDec.Parser PBDec.RawMessage (NS (FieldValue sch) ts)

-- --------
-- TERMS --
-- --------

-- RECORDS
-- -------

instance (All (ProtoBridgeField sch name) args, ProtoBridgeFields sch name args)
         => ProtoBridgeTerm sch ('DRecord name args) where
  termToProto (TRecord fields) = go fields
    where go :: forall fs. All (ProtoBridgeField sch name) fs
             => NP (Field sch) fs -> PBEnc.MessageBuilder
          go Nil       = mempty
          go (f :* fs) = fieldToProto @_ @_ @sch @name f <> go fs
  protoToTerm = TRecord <$> protoToFields @_ @_ @sch @name

class ProtoBridgeFields (sch :: Schema tn fn) (ty :: tn) (fields :: [FieldDef tn fn]) where
  protoToFields :: PBDec.Parser PBDec.RawMessage (NP (Field sch) fields)
instance ProtoBridgeFields sch ty '[] where
  protoToFields = pure Nil
instance (ProtoBridgeField sch ty f, ProtoBridgeFields sch ty fs)
         => ProtoBridgeFields sch ty (f ': fs) where
  protoToFields = (:*) <$> protoToField @_ @_ @sch @ty <*> protoToFields @_ @_ @sch @ty

instance ProtoBridgeTerm sch ('DRecord name args)
         => ProtoBridgeEmbedTerm sch ('DRecord name args) where
  termToEmbedProto fid v = PBEnc.embedded fid (termToProto v)
  embedProtoToOneFieldValue = PBDec.embedded' (protoToTerm @_ @_ @sch @('DRecord name args))
  supportsPackingTerm _ = False
  termToPackedEmbedProto = error "this is a bug, since we declare we do not support packed encoding"
  embedProtoToPackedFieldValue = error "this is a bug, since we declare we do not support packed encoding"

-- ENUMERATIONS
-- ------------

instance TypeError ('Text "protobuf requires wrapping enums in a message")
         => ProtoBridgeTerm sch ('DEnum name choices) where
  termToProto = error "protobuf requires wrapping enums in a message"
  protoToTerm = error "protobuf requires wrapping enums in a message"

instance ProtoBridgeEnum sch name choices
         => ProtoBridgeEmbedTerm sch ('DEnum name choices) where
  termToEmbedProto fid (TEnum v) = PBEnc.int32 fid (enumToProto @_ @_ @sch @name v)
  embedProtoToOneFieldValue = PBDec.int32 >>= fmap TEnum . protoToEnum @_ @_ @sch @name
  supportsPackingTerm _ = True
  termToPackedEmbedProto fid ts
    = PBEnc.packedVarints fid $ map (\(TEnum v) -> enumToProto @_ @_ @sch @name v) ts
  embedProtoToPackedFieldValue =
    PBDec.packedVarints >>= traverse (fmap TEnum . protoToEnum @_ @_ @sch @name)

class ProtoBridgeEnum (sch :: Schema tn fn) (ty :: tn) (choices :: [ChoiceDef fn]) where
  enumToProto :: Integral a => NS Proxy choices -> a
  protoToEnum :: Int32 -> PBDec.Parser a (NS Proxy choices)
instance ProtoBridgeEnum sch ty '[] where
  enumToProto = error "empty enum"
  protoToEnum _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "unknown enum type"))
instance (KnownNat (FindProtoBufId sch ty c), ProtoBridgeEnum sch ty cs)
         => ProtoBridgeEnum sch ty ('ChoiceDef c ': cs) where
  enumToProto (Z _) = fromIntegral (natVal (Proxy @(FindProtoBufId sch ty c)))
  enumToProto (S v) = enumToProto @_ @_ @sch @ty v
  protoToEnum n
    | n == enumValue = pure (Z Proxy)
    | otherwise      = S <$> protoToEnum @_ @_ @sch @ty n
    where enumValue = fromIntegral (natVal (Proxy @(FindProtoBufId sch ty c)))

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
         (ProtoBridgeOneFieldValue sch t, KnownNat (FindProtoBufId sch ty name))
         => ProtoBridgeField sch ty ('FieldDef name t) where
  fieldToProto (Field v) = oneFieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))
  protoToField
    = Field <$> case defaultOneFieldValue of
        Nothing -> do r <- one (Just <$> protoToOneFieldValue) Nothing `at` fieldId
                      maybe empty pure r
        Just d  -> one protoToOneFieldValue d `at` fieldId <|> pure d
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))

instance {-# OVERLAPS #-}
         (ProtoBridgeOneFieldValue sch t, KnownNat (FindProtoBufId sch ty name))
         => ProtoBridgeField sch ty ('FieldDef name ('TOption t)) where
  fieldToProto (Field (FOption Nothing))  = mempty
  fieldToProto (Field (FOption (Just v))) = oneFieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))
  protoToField = Field . FOption <$>
                   (PBDec.one (Just <$> protoToOneFieldValue) Nothing `at` fieldId <|> pure Nothing)
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))

class KnownBool (b :: Bool) where
  boolVal :: proxy b -> Bool
instance KnownBool 'True where
  boolVal _ = True
instance KnownBool 'False where
  boolVal _ = False

instance {-# OVERLAPS #-}
         (ProtoBridgeOneFieldValue sch t, KnownNat (FindProtoBufId sch ty name), KnownBool (FindProtoBufPacked sch ty name))
         => ProtoBridgeField sch ty ('FieldDef name ('TList t)) where
  fieldToProto (Field (FList xs))
    | boolVal (Proxy @(FindProtoBufPacked sch ty name)), supportsPacking (Proxy @(FieldValue sch t))
    = packedFieldValueToProto fieldId xs
    | otherwise
    = foldMap (oneFieldValueToProto fieldId) xs
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))
  protoToField = Field . FList <$> go
    where fieldId = fromInteger $ natVal (Proxy @(FindProtoBufId sch ty name))
          base = PBDec.repeated protoToOneFieldValue `at` fieldId <|> pure []
          go | supportsPacking (Proxy @(FieldValue sch t))
             = PBDec.one protoToPackedFieldValue [] `at` fieldId <|> base
             | otherwise
             = base

instance TypeError ('Text "maps are not currently supported")
         => ProtoBridgeField sch ty ('FieldDef name ('TMap k v)) where
  fieldToProto = error "maps are not currently supported"
  protoToField = error "maps are not currently supported"

instance {-# OVERLAPS #-}
         (ProtoBridgeUnionFieldValue (FindProtoBufOneOfIds sch ty name) sch ts)
         => ProtoBridgeField sch ty ('FieldDef name ('TUnion ts)) where
  fieldToProto (Field (FUnion v))
    = unionFieldValueToProto @_ @_ @(FindProtoBufOneOfIds sch ty name) v
  protoToField
    = Field . FUnion <$> protoToUnionFieldValue @_ @_ @(FindProtoBufOneOfIds sch ty name)

-- ------------------
-- TYPES OF FIELDS --
-- ------------------

-- SCHEMATIC
-- ---------

instance ProtoBridgeEmbedTerm sch (sch :/: t)
         => ProtoBridgeOneFieldValue sch ('TSchematic t) where
  defaultOneFieldValue = Nothing
  oneFieldValueToProto fid (FSchematic v) = termToEmbedProto fid v
  protoToOneFieldValue = FSchematic <$> embedProtoToOneFieldValue
  supportsPacking _ = supportsPackingTerm (Proxy @(Term sch (sch :/: t)))
  packedFieldValueToProto fid vs = termToPackedEmbedProto fid $ map (\(FSchematic t) -> t) vs
  protoToPackedFieldValue = map FSchematic <$> embedProtoToPackedFieldValue

-- PRIMITIVE TYPES
-- ---------------

instance TypeError ('Text "null cannot be converted to protobuf")
         => ProtoBridgeOneFieldValue sch 'TNull where
  defaultOneFieldValue = error "null cannot be converted to protobuf"
  oneFieldValueToProto = error "null cannot be converted to protobuf"
  protoToOneFieldValue = error "null cannot be converted to protobuf"
  supportsPacking _ = False
  packedFieldValueToProto = error "null cannot be converted to protobuf"
  protoToPackedFieldValue = error "null cannot be converted to protobuf"

instance ProtoBridgeOneFieldValue sch ('TPrimitive Int) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.int32 fid (fromIntegral n)
  protoToOneFieldValue = FPrimitive . fromIntegral <$> PBDec.int32
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedVarints fid $ map (\(FPrimitive i) -> fromIntegral i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedVarints

instance ProtoBridgeOneFieldValue sch ('TPrimitive Int32) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.int32 fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.int32
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedVarints fid $ map (\(FPrimitive i) -> fromIntegral i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedVarints

instance ProtoBridgeOneFieldValue sch ('TPrimitive Int64) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.int64 fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.int64
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedVarints fid $ map (\(FPrimitive i) -> fromIntegral i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedVarints

-- WARNING! These instances may go out of bounds
instance ProtoBridgeOneFieldValue sch ('TPrimitive Integer) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.int64 fid (fromInteger n)
  protoToOneFieldValue = FPrimitive . fromIntegral <$> PBDec.int64
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedVarints fid $ map (\(FPrimitive i) -> fromIntegral i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedVarints

instance ProtoBridgeOneFieldValue sch ('TPrimitive Float) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.float fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.float
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedFloats fid $ map (\(FPrimitive i) -> i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedFloats

instance ProtoBridgeOneFieldValue sch ('TPrimitive Double) where
  defaultOneFieldValue = Just $ FPrimitive 0
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.double fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.double
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedDoubles fid $ map (\(FPrimitive i) -> i) vs
  protoToPackedFieldValue = map FPrimitive <$> PBDec.packedDoubles

instance ProtoBridgeOneFieldValue sch ('TPrimitive Bool) where
  defaultOneFieldValue = Just $ FPrimitive False
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.enum fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.bool
  supportsPacking _ = True
  packedFieldValueToProto fid vs
    = PBEnc.packedVarints fid $ map (\(FPrimitive i) -> if i then 1 else 0) vs
  protoToPackedFieldValue = map (\(i :: Integer) -> FPrimitive (i /= 0)) <$> PBDec.packedVarints

instance ProtoBridgeOneFieldValue sch ('TPrimitive T.Text) where
  defaultOneFieldValue = Just $ FPrimitive ""
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.text fid (LT.fromStrict n)
  protoToOneFieldValue = FPrimitive . LT.toStrict <$> PBDec.text
  supportsPacking _ = False
  packedFieldValueToProto = error "this is a bug, since we declare we do not support packed encoding"
  protoToPackedFieldValue = error "this is a bug, since we declare we do not support packed encoding"

instance ProtoBridgeOneFieldValue sch ('TPrimitive LT.Text) where
  defaultOneFieldValue = Just $ FPrimitive ""
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.text fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.text
  supportsPacking _ = False
  packedFieldValueToProto = error "this is a bug, since we declare we do not support packed encoding"
  protoToPackedFieldValue = error "this is a bug, since we declare we do not support packed encoding"

instance ProtoBridgeOneFieldValue sch ('TPrimitive BS.ByteString) where
  defaultOneFieldValue = Just $ FPrimitive ""
  oneFieldValueToProto fid (FPrimitive n) = PBEnc.byteString fid n
  protoToOneFieldValue = FPrimitive <$> PBDec.byteString
  supportsPacking _ = False
  packedFieldValueToProto = error "this is a bug, since we declare we do not support packed encoding"
  protoToPackedFieldValue = error "this is a bug, since we declare we do not support packed encoding"

-- Note that Maybes and Lists require that we recur on the OneFieldValue class

instance TypeError ('Text "optionals cannot be nested in protobuf")
         => ProtoBridgeOneFieldValue sch ('TOption t) where
  defaultOneFieldValue = error "optionals cannot be nested in protobuf"
  oneFieldValueToProto = error "optionals cannot be nested in protobuf"
  protoToOneFieldValue = error "optionals cannot be nested in protobuf"
  supportsPacking      = error "optionals cannot be nested in protobuf"
  packedFieldValueToProto = error "optionals cannot be nested in protobuf"
  protoToPackedFieldValue = error "optionals cannot be nested in protobuf"

instance TypeError ('Text "lists cannot be nested in protobuf")
         => ProtoBridgeOneFieldValue sch ('TList t) where
  defaultOneFieldValue = error "lists cannot be nested in protobuf"
  oneFieldValueToProto = error "lists cannot be nested in protobuf"
  protoToOneFieldValue = error "lists cannot be nested in protobuf"
  supportsPacking      = error "lists cannot be nested in protobuf"
  packedFieldValueToProto = error "lists cannot be nested in protobuf"
  protoToPackedFieldValue = error "lists cannot be nested in protobuf"

instance TypeError ('Text "maps are not currently supported")
         => ProtoBridgeOneFieldValue sch ('TMap k v) where
  defaultOneFieldValue = error "maps are not currently supported"
  oneFieldValueToProto = error "maps are not currently supported"
  protoToOneFieldValue = error "maps are not currently supported"
  supportsPacking      = error "maps are not currently supported"
  packedFieldValueToProto = error "maps are not currently supported"
  protoToPackedFieldValue = error "maps are not currently supported"

instance TypeError ('Text "nested unions are not currently supported")
         => ProtoBridgeOneFieldValue sch ('TUnion choices) where
  defaultOneFieldValue = error "nested unions are not currently supported"
  oneFieldValueToProto = error "nested unions are not currently supported"
  protoToOneFieldValue = error "nested unions are not currently supported"
  supportsPacking      = error "nested unions are not currently supported"
  packedFieldValueToProto = error "nested unions are not currently supported"
  protoToPackedFieldValue = error "nested unions are not currently supported"

-- UNIONS
-- ------

instance ProtoBridgeUnionFieldValue ids sch '[] where
  unionFieldValueToProto = error "empty list of unions"
  protoToUnionFieldValue = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "unknown type in an union"))

instance ( ProtoBridgeOneFieldValue sch t, KnownNat thisId
         , ProtoBridgeUnionFieldValue restIds sch ts )
         => ProtoBridgeUnionFieldValue (thisId ': restIds) sch (t ': ts) where
  unionFieldValueToProto (Z v) = oneFieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @thisId)
  unionFieldValueToProto (S v) = unionFieldValueToProto @_ @_ @restIds v
  protoToUnionFieldValue
    = Z <$> p <|> S <$> protoToUnionFieldValue @_ @_ @restIds
    where fieldId = fromInteger $ natVal (Proxy @thisId)
          p = case defaultOneFieldValue of
            Nothing -> do r <- one (Just <$> protoToOneFieldValue) Nothing `at` fieldId
                          maybe empty pure r
            Just d  -> one protoToOneFieldValue d `at` fieldId <|> pure d
