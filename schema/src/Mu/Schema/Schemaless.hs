{-# language PolyKinds, DataKinds, GADTs,
             KindSignatures, ScopedTypeVariables,
             TypeApplications, TypeOperators,
             FlexibleContexts, MultiParamTypeClasses,
             AllowAmbiguousTypes, StandaloneDeriving #-}
module Mu.Schema.Schemaless (
  -- * Terms without an associated schema
  Term(..), Field(..), FieldValue(..)
  -- * Checking and conversion against a schema
, CheckSchema, checkSchema, fromSchemalessTerm
  -- * For deserialization to schemaless terms
, ToSchemalessTerm(..), ToSchemalessValue(..)
) where

import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable

import Mu.Schema.Class
import Mu.Schema.Definition
import qualified Mu.Schema.Interpretation as S

-- | Interpretation of a type in a schema.
data Term where
  TRecord :: [Field]    -> Term
  TEnum   :: Int        -> Term
  TSimple :: FieldValue -> Term
  deriving (Eq, Ord, Show)

-- | Interpretation of a field.
data Field where
  Field :: T.Text -> FieldValue -> Field
  deriving (Eq, Ord, Show)

-- | Interpretation of a field type, by giving a value of that type.
data FieldValue where
  FNull      :: FieldValue
  FPrimitive :: (Typeable t, Eq t, Ord t, Show t) => t -> FieldValue
  FSchematic :: Term -> FieldValue
  FOption    :: Maybe FieldValue -> FieldValue
  FList      :: [FieldValue] -> FieldValue
  FMap       :: M.Map FieldValue FieldValue -> FieldValue

checkSchema
  :: forall (s :: Schema tn fn) (t :: tn).
     CheckSchema s (s :/: t)
  => Proxy t -> Term -> Maybe (S.Term s (s :/: t))
checkSchema _ = checkSchema'

fromSchemalessTerm
  :: forall sch t sty.
     (HasSchema sch sty t, CheckSchema sch (sch :/: sty))
  => Term -> Maybe t
fromSchemalessTerm t = fromSchema @_ @_ @sch <$> checkSchema (Proxy @sty) t

class CheckSchema (s :: Schema tn fn) (t :: TypeDef tn fn) where
  checkSchema' :: Term -> Maybe (S.Term s t)

-- TODO: write instances of CheckSchema

class ToSchemalessTerm t where
  toSchemalessTerm  :: t -> Term
class ToSchemalessValue t where
  toSchemalessValue :: t -> FieldValue

-- Boring instances
deriving instance Show FieldValue
instance Eq FieldValue where
  FNull == FNull = True
  FPrimitive (x :: a) == FPrimitive (y :: b)
    = case eqT @a @b of
        Nothing   -> False
        Just Refl -> x == y
  FSchematic x == FSchematic y = x == y
  FOption    x == FOption    y = x == y
  FList      x == FList      y = x == y
  FMap       x == FMap       y = x == y
  _            == _            = False
instance Ord FieldValue where
  FNull <= _ = True
  FPrimitive _ <= FNull = False
  FPrimitive (x :: a) <= FPrimitive (y :: b)
    = case eqT @a @b of
        Nothing   -> typeOf x <= typeOf y
        Just Refl -> x <= y
  FPrimitive _ <= _            = True
  FSchematic _ <= FNull        = False
  FSchematic _ <= FPrimitive _ = False
  FSchematic x <= FSchematic y = x <= y
  FSchematic _ <= _            = True
  FOption    _ <= FNull        = False
  FOption    _ <= FPrimitive _ = False
  FOption    _ <= FSchematic _ = False
  FOption    x <= FOption    y = x <= y
  FOption    _ <= _            = True
  FList      _ <= FNull        = False
  FList      _ <= FPrimitive _ = False
  FList      _ <= FSchematic _ = False
  FList      _ <= FOption    _ = False
  FList      x <= FList      y = x <= y
  FList      _ <= _            = True
  FMap       _ <= FNull        = False
  FMap       _ <= FPrimitive _ = False
  FMap       _ <= FSchematic _ = False
  FMap       _ <= FOption    _ = False
  FMap       _ <= FList      _ = False
  FMap       x <= FMap       y = x <= y
  -- FMap       _ <= _            = True