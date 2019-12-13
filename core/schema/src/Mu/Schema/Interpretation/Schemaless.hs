{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
module Mu.Schema.Interpretation.Schemaless (
  -- * Terms without an associated schema
  Term(..), Field(..), FieldValue(..)
  -- * Checking and conversion against a schema
, CheckSchema, checkSchema, fromSchemalessTerm
  -- * For deserialization to schemaless terms
, ToSchemalessTerm(..), ToSchemalessValue(..)
) where

import           Control.Applicative      ((<|>))
import           Data.List                (find)
import qualified Data.Map                 as M
import           Data.Proxy
import           Data.SOP
import qualified Data.Text                as T
import           Data.Typeable

import           Mu.Schema.Class
import           Mu.Schema.Definition
import qualified Mu.Schema.Interpretation as S

-- | Interpretation of a type in a schema.
data Term (w :: * -> *) where
  TRecord :: [Field w]    -> Term w
  TEnum   :: Int          -> Term w
  TSimple :: FieldValue w -> Term w

deriving instance Eq   (w (FieldValue w)) => Eq   (Term w)
deriving instance Ord  (w (FieldValue w)) => Ord  (Term w)
deriving instance Show (w (FieldValue w)) => Show (Term w)

-- | Interpretation of a field.
data Field (w :: * -> *) where
  Field :: T.Text -> w (FieldValue w) -> Field w

deriving instance Eq   (w (FieldValue w)) => Eq   (Field w)
deriving instance Ord  (w (FieldValue w)) => Ord  (Field w)
deriving instance Show (w (FieldValue w)) => Show (Field w)

-- | Interpretation of a field type, by giving a value of that type.
data FieldValue (w :: * -> *) where
  FNull      :: FieldValue w
  FPrimitive :: (Typeable t, Eq t, Ord t, Show t) => t -> FieldValue w
  FSchematic :: Term w -> FieldValue w
  FOption    :: Maybe (FieldValue w) -> FieldValue w
  FList      :: [FieldValue w] -> FieldValue w
  FMap       :: M.Map (FieldValue w) (FieldValue w) -> FieldValue w

checkSchema
  :: forall (s :: Schema tn fn) (t :: tn) (w :: * -> *).
     (Traversable w, CheckSchema s (s :/: t))
  => Proxy t -> Term w -> Maybe (S.Term w s (s :/: t))
checkSchema _ = checkSchema'

fromSchemalessTerm
  :: forall sch w t sty.
     (Traversable w, FromSchema w sch sty t, CheckSchema sch (sch :/: sty))
  => Term w -> Maybe t
fromSchemalessTerm t = fromSchema @_ @_ @w @sch <$> checkSchema (Proxy @sty) t

class ToSchemalessTerm t w where
  toSchemalessTerm  :: t -> Term w
class ToSchemalessValue t w where
  toSchemalessValue :: t -> FieldValue w

class CheckSchema (s :: Schema tn fn) (t :: TypeDef tn fn) where
  checkSchema' :: Traversable w => Term w -> Maybe (S.Term w s t)
class CheckSchemaFields (s :: Schema tn fn) (fields :: [FieldDef tn fn]) where
  checkSchemaFields :: Traversable w => [Field w] -> Maybe (NP (S.Field w s) fields)
class CheckSchemaEnum (choices :: [ChoiceDef fn]) where
  checkSchemaEnumInt  :: Int -> Maybe (NS Proxy choices)
  checkSchemaEnumText :: T.Text -> Maybe (NS Proxy choices)
class CheckSchemaValue (s :: Schema tn fn) (field :: FieldType tn) where
  checkSchemaValue :: Traversable w => FieldValue w -> Maybe (S.FieldValue w s field)
class CheckSchemaUnion (s :: Schema tn fn) (ts :: [FieldType tn]) where
  checkSchemaUnion :: Traversable w => FieldValue w -> Maybe (NS (S.FieldValue w s) ts)

instance CheckSchemaFields s fields => CheckSchema s ('DRecord nm fields) where
  checkSchema' (TRecord fields) = S.TRecord <$> checkSchemaFields fields
  checkSchema' _                = Nothing
instance CheckSchemaFields s '[] where
  checkSchemaFields _ = pure Nil
instance (KnownName nm, CheckSchemaValue s ty, CheckSchemaFields s rest)
         => CheckSchemaFields s ('FieldDef nm ty ': rest) where
  checkSchemaFields fs
    = do let name = T.pack (nameVal (Proxy @nm))
         Field _ v <- find (\(Field fieldName _) -> fieldName == name) fs
         v' <- traverse checkSchemaValue v
         r' <- checkSchemaFields @_ @_ @s @rest fs
         return (S.Field v' :* r')

instance CheckSchemaEnum choices => CheckSchema s ('DEnum nm choices) where
  checkSchema' (TEnum n) = S.TEnum <$> checkSchemaEnumInt n
  checkSchema' (TSimple (FPrimitive (n :: a)))
    = case (eqT @a @Int, eqT @a @T.Text, eqT @a @String) of
        (Just Refl, _, _) -> S.TEnum <$> checkSchemaEnumInt n
        (_, Just Refl, _) -> S.TEnum <$> checkSchemaEnumText n
        (_, _, Just Refl) -> S.TEnum <$> checkSchemaEnumText (T.pack n)
        _                 -> Nothing
  checkSchema' _ = Nothing
instance CheckSchemaEnum '[] where
  checkSchemaEnumInt  _ = Nothing
  checkSchemaEnumText _ = Nothing
instance (KnownName c, CheckSchemaEnum cs)
         => CheckSchemaEnum ('ChoiceDef c ': cs) where
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
  checkSchemaValue _              = Nothing
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
deriving instance (Show (w (FieldValue w))) => Show (FieldValue w)
instance (Eq (w (FieldValue w))) => Eq (FieldValue w) where
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
instance (Ord (w (FieldValue w))) => Ord (FieldValue w) where
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
