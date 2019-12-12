{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Adapter.Json where

import           Control.Applicative                 ((<|>))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Functor.Contravariant
import           Data.Functor.Identity
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Text                           as T
import qualified Data.Vector                         as V

import           Mu.Schema
import qualified Mu.Schema.Interpretation.Schemaless as SLess

instance Applicative w => SLess.ToSchemalessTerm Value w where
  toSchemalessTerm (Object o)
    = SLess.TRecord $ map (\(k,v) -> SLess.Field k (pure $ SLess.toSchemalessValue v))
                    $ HM.toList o
  toSchemalessTerm v = SLess.TSimple (SLess.toSchemalessValue v)

instance Applicative w => SLess.ToSchemalessValue Value w where
  toSchemalessValue r@(Object _)
    = SLess.FSchematic (SLess.toSchemalessTerm r)
  toSchemalessValue Null       = SLess.FNull
  toSchemalessValue (String s) = SLess.FPrimitive s
  toSchemalessValue (Number n) = SLess.FPrimitive n
  toSchemalessValue (Bool   b) = SLess.FPrimitive b
  toSchemalessValue (Array xs)
    = SLess.FList $ map SLess.toSchemalessValue $ V.toList xs

instance (HasSchema w sch sty a, ToJSON (Term w sch (sch :/: sty)))
         => ToJSON (WithSchema w sch sty a) where
  toJSON (WithSchema x) = toJSON (toSchema' @_ @_ @sch @w x)
instance (HasSchema w sch sty a, FromJSON (Term w sch (sch :/: sty)))
         => FromJSON (WithSchema w sch sty a) where
  parseJSON v = WithSchema . fromSchema' @_ @_ @sch @w <$> parseJSON v

instance ToJSONFields sch args => ToJSON (Term Identity sch ('DRecord name args)) where
  toJSON (TRecord fields) = Object (toJSONFields fields)
instance FromJSONFields w sch args => FromJSON (Term w sch ('DRecord name args)) where
  parseJSON (Object v) = TRecord <$> parseJSONFields v
  parseJSON _          = fail "expected object"

class ToJSONFields sch fields where
  toJSONFields :: NP (Field Identity sch) fields -> Object
instance ToJSONFields sch '[] where
  toJSONFields _ = HM.empty
instance (KnownName name, ToJSON (FieldValue Identity sch t), ToJSONFields sch fs)
         => ToJSONFields sch ('FieldDef name t ': fs) where
  toJSONFields (Field (Identity v) :* rest) = HM.insert key value $ toJSONFields rest
    where key = T.pack (nameVal (Proxy @name))
          value = toJSON v

class FromJSONFields w sch fields where
  parseJSONFields :: Object -> Parser (NP (Field w sch) fields)
instance FromJSONFields w sch '[] where
  parseJSONFields _ = return Nil
instance (Applicative w, KnownName name, FromJSON (FieldValue w sch t), FromJSONFields w sch fs)
         => FromJSONFields w sch ('FieldDef name t ': fs) where
  parseJSONFields v = (:*) <$> (Field <$> (pure <$> v .: key)) <*> parseJSONFields v
    where key = T.pack (nameVal (Proxy @name))

instance ToJSONEnum choices => ToJSON (Term w sch ('DEnum name choices)) where
  toJSON (TEnum choice) = String (toJSONEnum choice)
instance FromJSONEnum choices => FromJSON (Term w sch ('DEnum name choices)) where
  parseJSON (String s) = TEnum <$> parseJSONEnum s
  parseJSON _          = fail "expected string"

class ToJSONEnum choices where
  toJSONEnum :: NS Proxy choices -> T.Text
instance ToJSONEnum '[] where
  toJSONEnum = error "empty enum"
instance (KnownName c, ToJSONEnum cs)
         => ToJSONEnum ('ChoiceDef c ': cs) where
  toJSONEnum (Z _) = T.pack (nameVal (Proxy @c))
  toJSONEnum (S v) = toJSONEnum v

class FromJSONEnum choices where
  parseJSONEnum :: T.Text -> Parser (NS Proxy choices)
instance FromJSONEnum '[] where
  parseJSONEnum _ = fail "unknown enum value"
instance (KnownName c, FromJSONEnum cs)
         => FromJSONEnum ('ChoiceDef c ': cs) where
  parseJSONEnum v
    | v == key  = return (Z Proxy)
    | otherwise = S <$> parseJSONEnum v
    where key = T.pack (nameVal (Proxy @c))

instance ToJSON (FieldValue w sch t) => ToJSON (Term w sch ('DSimple t)) where
  toJSON (TSimple x) = toJSON x
instance FromJSON (FieldValue w sch t) => FromJSON (Term w sch ('DSimple t)) where
  parseJSON v = TSimple <$> parseJSON v

instance ToJSON (FieldValue w sch 'TNull) where
  toJSON FNull = Null
instance ToJSON t => ToJSON (FieldValue w sch ('TPrimitive t)) where
  toJSON (FPrimitive v) = toJSON v
instance ToJSONKey t => ToJSONKey (FieldValue w sch ('TPrimitive t)) where
  toJSONKey = contramap FPrimitive toJSONKey
  toJSONKeyList = contramap (map FPrimitive) toJSONKeyList
instance ToJSON (Term w sch (sch :/: t))
         => ToJSON (FieldValue w sch ('TSchematic t)) where
  toJSON (FSchematic v) = toJSON v
instance ToJSON (FieldValue w sch t)
         => ToJSON (FieldValue w sch ('TOption t)) where
  toJSON (FOption v) = toJSON v
instance ToJSON (FieldValue w sch t)
         => ToJSON (FieldValue w sch ('TList t)) where
  toJSON (FList v) = toJSON v
instance (ToJSONKey (FieldValue w sch k), ToJSON (FieldValue w sch v))
         => ToJSON (FieldValue w sch ('TMap k v)) where
  toJSON (FMap v) = toJSON v
instance (ToJSONUnion w sch us)
         => ToJSON (FieldValue w sch ('TUnion us)) where
  toJSON (FUnion v) = unionToJSON v

class ToJSONUnion w sch us where
  unionToJSON :: NS (FieldValue w sch) us -> Value
instance ToJSONUnion w sch '[] where
  unionToJSON = error "this should never happen"
instance (ToJSON (FieldValue w sch u), ToJSONUnion w sch us)
         => ToJSONUnion w sch (u ': us) where
  unionToJSON (Z v) = toJSON v
  unionToJSON (S r) = unionToJSON r

instance FromJSON (FieldValue w sch 'TNull) where
  parseJSON Null = return FNull
  parseJSON _    = fail "expected null"
instance FromJSON t => FromJSON (FieldValue w sch ('TPrimitive t)) where
  parseJSON v = FPrimitive <$> parseJSON v
instance FromJSONKey t => FromJSONKey (FieldValue w sch ('TPrimitive t)) where
  fromJSONKey = fmap FPrimitive fromJSONKey
  fromJSONKeyList = fmap (map FPrimitive) fromJSONKeyList
instance FromJSON (Term w sch (sch :/: t))
         => FromJSON (FieldValue w sch ('TSchematic t)) where
  parseJSON v = FSchematic <$> parseJSON v
instance FromJSON (FieldValue w sch t)
         => FromJSON (FieldValue w sch ('TOption t)) where
  parseJSON v = FOption <$> parseJSON v
instance FromJSON (FieldValue w sch t)
         => FromJSON (FieldValue w sch ('TList t)) where
  parseJSON v = FList <$> parseJSON v
instance ( FromJSONKey (FieldValue w sch k), FromJSON (FieldValue w sch v)
         , Ord (FieldValue w sch k) )
         => FromJSON (FieldValue w sch ('TMap k v)) where
  parseJSON v = FMap <$> parseJSON v
instance (FromJSONUnion w sch us)
         => FromJSON (FieldValue w sch ('TUnion us)) where
  parseJSON v = FUnion <$> unionFromJSON v

class FromJSONUnion w sch us where
  unionFromJSON :: Value -> Parser (NS (FieldValue w sch) us)
instance FromJSONUnion w sch '[] where
  unionFromJSON _ = fail "value does not match any of the types of the union"
instance (FromJSON (FieldValue w sch u), FromJSONUnion w sch us)
         => FromJSONUnion w sch (u ': us) where
  unionFromJSON v = Z <$> parseJSON v <|> S <$> unionFromJSON v
