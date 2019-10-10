{-# language PolyKinds, DataKinds, GADTs,
             ScopedTypeVariables,
             TypeApplications, TypeOperators,
             FlexibleContexts, MultiParamTypeClasses,
             AllowAmbiguousTypes, StandaloneDeriving,
             FlexibleInstances, UndecidableInstances #-}
module Mu.Schema.Interpretation.Schemaless (
  -- * Terms without an associated schema
  Term(..), Field(..), FieldValue(..)
  -- * Checking and conversion against a schema
, CheckSchema, checkSchema, fromSchemalessTerm
  -- * For deserialization to schemaless terms
, ToSchemalessTerm(..), ToSchemalessValue(..)
) where

import Control.Applicative ((<|>))
import Data.List (find)
import qualified Data.Map as M
import Data.Proxy
import Data.SOP
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

class ToSchemalessTerm t where
  toSchemalessTerm  :: t -> Term
class ToSchemalessValue t where
  toSchemalessValue :: t -> FieldValue

class CheckSchema (s :: Schema tn fn) (t :: TypeDef tn fn) where
  checkSchema' :: Term -> Maybe (S.Term s t)
class CheckSchemaFields (s :: Schema tn fn) (fields :: [FieldDef tn fn]) where
  checkSchemaFields :: [Field] -> Maybe (NP (S.Field s) fields)
class CheckSchemaEnum (choices :: [ChoiceDef fn]) where
  checkSchemaEnumInt  :: Int -> Maybe (NS Proxy choices)
  checkSchemaEnumText :: T.Text -> Maybe (NS Proxy choices)
class CheckSchemaValue (s :: Schema tn fn) (field :: FieldType tn) where
  checkSchemaValue :: FieldValue -> Maybe (S.FieldValue s field)
class CheckSchemaUnion (s :: Schema tn fn) (ts :: [FieldType tn]) where
  checkSchemaUnion :: FieldValue -> Maybe (NS (S.FieldValue s) ts)

instance CheckSchemaFields s fields => CheckSchema s ('DRecord nm anns fields) where
  checkSchema' (TRecord fields) = S.TRecord <$> checkSchemaFields fields
  checkSchema' _ = Nothing
instance CheckSchemaFields s '[] where
  checkSchemaFields _ = pure Nil
instance (KnownName nm, CheckSchemaValue s ty, CheckSchemaFields s rest)
         => CheckSchemaFields s ('FieldDef nm anns ty ': rest) where
  checkSchemaFields fs
    = do let name = T.pack (nameVal (Proxy @nm))
         Field _ v <- find (\(Field fieldName _) -> fieldName == name) fs
         v' <- checkSchemaValue v
         r' <- checkSchemaFields @_ @_ @s @rest fs
         return (S.Field v' :* r')

instance CheckSchemaEnum choices => CheckSchema s ('DEnum nm anns choices) where
  checkSchema' (TEnum n) = S.TEnum <$> checkSchemaEnumInt n
  checkSchema' (TSimple (FPrimitive (n :: a)))
    = case (eqT @a @Int, eqT @a @T.Text, eqT @a @String) of
        (Just Refl, _, _) -> S.TEnum <$> checkSchemaEnumInt n
        (_, Just Refl, _) -> S.TEnum <$> checkSchemaEnumText n
        (_, _, Just Refl) -> S.TEnum <$> checkSchemaEnumText (T.pack n)
        _ -> Nothing
  checkSchema' _ = Nothing
instance CheckSchemaEnum '[] where
  checkSchemaEnumInt  _ = Nothing
  checkSchemaEnumText _ = Nothing
instance (KnownName c, CheckSchemaEnum cs)
         => CheckSchemaEnum ('ChoiceDef c anns ': cs) where
  checkSchemaEnumInt 0 = Just (Z Proxy)
  checkSchemaEnumInt n = S <$> checkSchemaEnumInt (n-1)
  checkSchemaEnumText t
    | t == T.pack (nameVal (Proxy @c)) = Just (Z Proxy)
    | otherwise                        = S <$> checkSchemaEnumText t

instance CheckSchemaValue s f => CheckSchema s ('DSimple f) where
  checkSchema' (TSimple t) = S.TSimple <$> checkSchemaValue t
  checkSchema' _           = Nothing
instance CheckSchemaValue s 'TNull where
  checkSchemaValue FNull = Just S.FNull
  checkSchemaValue _     = Nothing
instance Typeable t => CheckSchemaValue s ('TPrimitive t) where
  checkSchemaValue (FPrimitive (t :: a))
    = case eqT @a @t of
        Just Refl -> Just (S.FPrimitive t)
        Nothing   -> Nothing
  checkSchemaValue _              = Nothing
-- TODO: handle enums better by an if with typedef
instance (CheckSchema s (s :/: t))
         => CheckSchemaValue s ('TSchematic t) where
  checkSchemaValue (FSchematic t) = S.FSchematic <$> checkSchema' t
  checkSchemaValue _ = Nothing
instance CheckSchemaValue s t => CheckSchemaValue s ('TOption t) where
  checkSchemaValue (FOption x) = S.FOption <$> traverse checkSchemaValue x
  checkSchemaValue _           = Nothing
instance CheckSchemaValue s t => CheckSchemaValue s ('TList t) where
  checkSchemaValue (FList xs) = S.FList <$> traverse checkSchemaValue xs
  checkSchemaValue _          = Nothing
-- TODO: how to deal with maps??
instance CheckSchemaUnion s ts => CheckSchemaValue s ('TUnion ts) where
  checkSchemaValue x = S.FUnion <$> checkSchemaUnion x

instance CheckSchemaUnion s '[] where
  checkSchemaUnion _ = Nothing
instance (CheckSchemaValue s t, CheckSchemaUnion s ts)
         => CheckSchemaUnion s (t ': ts) where
  checkSchemaUnion x = Z <$> checkSchemaValue @_ @_ @s @t x <|> S <$> checkSchemaUnion x

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