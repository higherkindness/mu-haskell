{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Terms without an associated schema

In the edges of your application it's useful to
consider terms for which a type-level schema has
not yet been applied. Think of receiving a JSON
document: you can parse it but checking the schema
is an additional step.
-}
module Mu.Schema.Interpretation.Schemaless (
  -- * Terms without an associated schema
  Term(..), Field(..), FieldValue(..)
  -- * Checking and conversion against a schema
, checkSchema, fromSchemalessTerm
  -- * For deserialization to schemaless terms
, ToSchemalessTerm(..), ToSchemalessValue(..)
  -- * For implementors
, CheckSchema
) where

import           Control.Applicative      ((<|>))
import           Data.List                (find)
import qualified Data.Map                 as M
import           Data.Profunctor
import           Data.Proxy
import           Data.SOP
import qualified Data.Text                as T
import           Data.Typeable

import           Mu.Schema.Class
import           Mu.Schema.Definition
import qualified Mu.Schema.Interpretation as S

-- | Interpretation of a type in a schema.
data Term (w :: * -> * -> *) where
  -- | A record given by the value of its fields.
  TRecord :: [Field w]    -> Term w
  -- | An enumeration given by one choice.
  TEnum   :: Int          -> Term w
  -- | A primitive value.
  TSimple :: FieldValue w -> Term w

deriving instance Eq   (w [FieldValue w] (FieldValue w)) => Eq   (Term w)
deriving instance Ord  (w [FieldValue w] (FieldValue w)) => Ord  (Term w)
deriving instance Show (w [FieldValue w] (FieldValue w)) => Show (Term w)

-- | Interpretation of a field.
data Field (w :: * -> * -> *) where
  -- | A single field given by its name and its value.
  --   Note that the contents are wrapped in a @w@ type constructor.
  Field :: T.Text -> w [FieldValue w] (FieldValue w) -> Field w

deriving instance Eq   (w [FieldValue w] (FieldValue w)) => Eq   (Field w)
deriving instance Ord  (w [FieldValue w] (FieldValue w)) => Ord  (Field w)
deriving instance Show (w [FieldValue w] (FieldValue w)) => Show (Field w)

-- | Interpretation of a field type, by giving a value of that type.
data FieldValue (w :: * -> * -> *) where
  FNull      :: FieldValue w
  FPrimitive :: (Typeable t, Eq t, Ord t, Show t) => t -> FieldValue w
  FSchematic :: Term w -> FieldValue w
  FOption    :: Maybe (FieldValue w) -> FieldValue w
  FList      :: [FieldValue w] -> FieldValue w
  FMap       :: M.Map (FieldValue w) (FieldValue w) -> FieldValue w

-- | Checks that a schemaless 'Term' obbeys the
--   restrictions for tyoe @t@ of schema @s@.
--   If successful, returns a 'S.Term' indexed
--   by the corresponding schema and type.
--
--   Use this function to check a schemaless terms
--   at the "borders" of your application.
checkSchema
  :: forall (s :: Schema tn fn) (t :: tn) (w :: * -> * -> *).
     (Profunctor w, forall x. Traversable (w x), CheckSchema s (s :/: t))
  => Proxy t -> Term w -> Maybe (S.Term w s (s :/: t))
checkSchema _ = checkSchema'

-- | Converts a schemaless term to a Haskell type
--   by going through the corresponding schema type.
fromSchemalessTerm
  :: forall sch w t sty.
     (Profunctor w, forall x. Traversable (w x), FromSchema w sch sty t, CheckSchema sch (sch :/: sty))
  => Term w -> Maybe t
fromSchemalessTerm t = fromSchema @_ @_ @w @sch <$> checkSchema (Proxy @sty) t

-- | Deserialization to schemaless terms.
class ToSchemalessTerm t w where
  -- | Turns a document (such as JSON) into a schemaless term.
  --   This function should handle the "compound" types in that format,
  --   such as records and enumerations.
  toSchemalessTerm  :: t -> Term w
-- | Deserialization to schemaless values.
class ToSchemalessValue t w where
  -- | Turns a document (such as JSON) into a schemaless term.
  --   This function should handle the "primitive" types in that format.
  toSchemalessValue :: t -> FieldValue w

-- | Type class used to define the generic 'checkSchema'.
--
--   Exposed for usage in other modules,
--   in particular 'Mu.Schema.Registry'.
class CheckSchema (s :: Schema tn fn) (t :: TypeDef tn fn) where
  checkSchema' :: (Profunctor w, forall x. Traversable (w x))
               => Term w -> Maybe (S.Term w s t)
class CheckSchemaFields (s :: Schema tn fn) (fields :: [FieldDef tn fn]) where
  checkSchemaFields :: (Profunctor w, forall x. Traversable (w x))
                    => [Field w] -> Maybe (NP (S.Field w s) fields)
class CheckSchemaEnum (choices :: [ChoiceDef fn]) where
  checkSchemaEnumInt  :: Int -> Maybe (NS Proxy choices)
  checkSchemaEnumText :: T.Text -> Maybe (NS Proxy choices)
class CheckSchemaValue (s :: Schema tn fn) (field :: FieldType tn) where
  checkSchemaValue :: (Profunctor w, forall x. Traversable (w x))
                   => FieldValue w -> Maybe (S.FieldValue w s field)
class CheckSchemaUnion (s :: Schema tn fn) (ts :: [FieldType tn]) where
  checkSchemaUnion :: (Profunctor w, forall x. Traversable (w x))
                   => FieldValue w -> Maybe (NS (S.FieldValue w s) ts)

instance CheckSchemaFields s fields => CheckSchema s ('DRecord nm fields) where
  checkSchema' (TRecord fields) = S.TRecord <$> checkSchemaFields fields
  checkSchema' _                = Nothing
instance CheckSchemaFields s '[] where
  checkSchemaFields _ = pure Nil
instance (KnownName nm, CheckSchemaValue s ty, CheckSchemaFields s rest)
         => CheckSchemaFields s ('FieldDef nm '[] ty ': rest) where
  checkSchemaFields fs
    = do let name = T.pack (nameVal (Proxy @nm))
         Field _ v <- find (\(Field fieldName _) -> fieldName == name) fs
         v' <- traverse checkSchemaValue v
         r' <- checkSchemaFields @_ @_ @s @rest fs
         return (S.Field (lmap (const []) v') :* r')

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
deriving instance (Show (w [FieldValue w] (FieldValue w))) => Show (FieldValue w)
instance (Eq (w [FieldValue w] (FieldValue w))) => Eq (FieldValue w) where
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
instance (Ord (w [FieldValue w] (FieldValue w))) => Ord (FieldValue w) where
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
