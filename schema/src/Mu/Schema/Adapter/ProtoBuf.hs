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
  ProtoBufFieldIds
, IsProtoSchema
, HasProtoSchema
, toProtoViaSchema
, fromProtoViaSchema
, parseProtoViaSchema
, FromProtoBufRegistry
, fromProtoBufWithRegistry
, parseProtoBufWithRegistry
) where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Int
import Data.SOP (All)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits
import Proto3.Wire
import qualified Proto3.Wire.Encode as PBEnc
import qualified Proto3.Wire.Decode as PBDec

import Mu.Schema
import Mu.Schema.Registry as R

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

-- | Defines for each field in the schema the corresponding field ID.
type family ProtoBufFieldIds (sch :: Schema tn fn) (t :: tn) :: [Mapping fn Nat]

fromProtoBufWithRegistry
  :: forall (subject :: k) t. 
     FromProtoBufRegistry (R.Registry subject) t
  => PBDec.Parser PBDec.RawMessage t
fromProtoBufWithRegistry = fromProtoBufRegistry' (Proxy @(Registry subject))

parseProtoBufWithRegistry
  :: forall (subject :: k) t. 
     FromProtoBufRegistry (R.Registry subject) t
  => BS.ByteString -> Either PBDec.ParseError t
parseProtoBufWithRegistry = PBDec.parse (fromProtoBufWithRegistry @k @subject)

class FromProtoBufRegistry (ms :: Mappings Nat Schema') t where
  fromProtoBufRegistry' :: Proxy ms -> PBDec.Parser PBDec.RawMessage t

instance FromProtoBufRegistry '[] t where
  fromProtoBufRegistry' _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "no schema found in registry"))
instance (HasProtoSchema s sty t, FromProtoBufRegistry ms t)
         => FromProtoBufRegistry ( (n ':<->: s) ': ms) t where
  fromProtoBufRegistry' _ = fromProtoViaSchema @s <|> fromProtoBufRegistry' (Proxy @ms)

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

class ProtoBridgeField (sch :: Schema tn fn) (ms :: [Mapping fn Nat]) (f :: FieldDef tn fn) where
  fieldToProto :: Proxy ms -> Field sch f -> PBEnc.MessageBuilder
  protoToField :: Proxy ms -> PBDec.Parser PBDec.RawMessage (Field sch f)

class ProtoBridgeFieldValue (sch :: Schema tn fn) (t :: FieldType tn) where
  fieldValueToProto :: FieldNumber -> FieldValue sch t -> PBEnc.MessageBuilder
  protoToFieldValue :: PBDec.Parser PBDec.RawField (FieldValue sch t)

class ProtoBridgeOneFieldValue (sch :: Schema tn fn) (t :: FieldType tn) where
  protoToOneFieldValue :: PBDec.Parser PBDec.RawPrimitive (FieldValue sch t)

-- --------
-- TERMS --
-- --------

-- RECORDS
-- -------

instance forall sch name args.
         ( All (ProtoBridgeField sch (ProtoBufFieldIds sch name)) args
         , ProtoBridgeFields sch (ProtoBufFieldIds sch name) args )
         => ProtoBridgeTerm sch ('DRecord name args) where
  termToProto (TRecord fields) = go fields
    where go :: forall fs. All (ProtoBridgeField sch (ProtoBufFieldIds sch name)) fs
             => NP (Field sch) fs -> PBEnc.MessageBuilder
          go Nil = mempty
          go (f :* fs) = fieldToProto (Proxy @(ProtoBufFieldIds sch name)) f <> go fs
  protoToTerm = TRecord <$> protoToFields (Proxy @(ProtoBufFieldIds sch name))

class ProtoBridgeFields sch ms fields where
  protoToFields :: Proxy ms -> PBDec.Parser PBDec.RawMessage (NP (Field sch) fields)
instance ProtoBridgeFields sch ms '[] where
  protoToFields _ = pure Nil
instance (ProtoBridgeField sch ms f, ProtoBridgeFields sch ms fs)
         => ProtoBridgeFields sch ms (f ': fs) where
  protoToFields p = (:*) <$> protoToField p <*> protoToFields p

instance forall sch name args.
         ProtoBridgeTerm sch ('DRecord name args)
         => ProtoBridgeEmbedTerm sch ('DRecord name args) where
  termToEmbedProto fid v = PBEnc.embedded fid (termToProto v)
  embedProtoToFieldValue = do
    t <- PBDec.embedded (protoToTerm @_ @_ @sch @('DRecord name args))
    case t of
      Nothing -> PBDec.Parser (\_ -> Left (PBDec.WireTypeError "expected message"))
      Just v  -> return v
  embedProtoToOneFieldValue = PBDec.embedded' (protoToTerm @_ @_ @sch @('DRecord name args))

-- ENUMERATIONS
-- ------------

instance TypeError ('Text "protobuf requires wrapping enums in a message")
         => ProtoBridgeTerm sch ('DEnum name choices) where
  termToProto = error "protobuf requires wrapping enums in a message"
  protoToTerm = error "protobuf requires wrapping enums in a message"

instance forall sch name choices.
         ProtoBridgeEnum (ProtoBufFieldIds sch name) choices
         => ProtoBridgeEmbedTerm sch ('DEnum name choices) where
  termToEmbedProto fid (TEnum v) = enumToProto (Proxy @(ProtoBufFieldIds sch name)) fid v
  embedProtoToFieldValue    = do n <- PBDec.one PBDec.int32 0
                                 TEnum <$> protoToEnum (Proxy @(ProtoBufFieldIds sch name)) n
  embedProtoToOneFieldValue = do n <- PBDec.int32
                                 TEnum <$> protoToEnum (Proxy @(ProtoBufFieldIds sch name)) n

class ProtoBridgeEnum (ms :: [Mapping fn Nat]) (choices :: [fn]) where
  enumToProto :: Proxy ms -> FieldNumber -> NS Proxy choices -> PBEnc.MessageBuilder
  protoToEnum :: Proxy ms -> Int32 -> PBDec.Parser a (NS Proxy choices)
instance ProtoBridgeEnum ms '[] where
  enumToProto = error "empty enum"
  protoToEnum _ _ = PBDec.Parser (\_ -> Left (PBDec.WireTypeError "unknown enum type"))
instance (KnownNat (MappingRight ms c), ProtoBridgeEnum ms cs)
         => ProtoBridgeEnum ms (c ': cs) where
  enumToProto _ fid (Z _) = PBEnc.int32 fid enumValue
    where enumValue = fromIntegral (natVal (Proxy @(MappingRight ms c)))
  enumToProto p fid (S v) = enumToProto p fid v
  protoToEnum p n
    | n == enumValue = return (Z Proxy)
    | otherwise      = S <$> protoToEnum p n
    where enumValue = fromIntegral (natVal (Proxy @(MappingRight ms c)))

-- SIMPLE
-- ------

instance TypeError ('Text "protobuf requires wrapping primitives in a message")
         => ProtoBridgeTerm sch ('DSimple t) where
  termToProto = error "protobuf requires wrapping primitives in a message"
  protoToTerm = error "protobuf requires wrapping primitives in a message"

-- ---------
-- FIELDS --
-- ---------

instance forall sch ms name t.
         (ProtoBridgeFieldValue sch t, KnownNat (MappingRight ms name))
         => ProtoBridgeField sch ms ('FieldDef name t) where
  fieldToProto _ (Field v) = fieldValueToProto fieldId v
    where fieldId = fromInteger $ natVal (Proxy @(MappingRight ms name))
  protoToField _ = Field <$> protoToFieldValue `at` fieldId
    where fieldId = fromInteger $ natVal (Proxy @(MappingRight ms name))

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

-- TODO: Missing unions!!