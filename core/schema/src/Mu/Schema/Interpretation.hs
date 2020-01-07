{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language PolyKinds             #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Interpretation of schemas

This module defines 'Term's which comply with
a given 'Schema'. These 'Term's are the main
form of values used internally by @mu-haskell@.

This module follows the ideas of
<https://reasonablypolymorphic.com/blog/higher-kinded-data/ higher-kinded data>.
In particular, each interpretation of a 'Field'
wraps its contents into a "wrapper" type @w@,
which may add additional behavior to it.
For example, in Protocol Buffers every field is
optional, and this is expressed by setting
@w@ to 'Maybe'.

In this module we make use of 'NP' and 'NS'
as defined by <https://hackage.haskell.org/package/sop-core sop-core>.
These are the n-ary versions of a pair and
'Either', respectively. In other words, 'NP'
puts together a bunch of values of different
types, 'NS' allows you to choose from a bunch
of types.
-}
module Mu.Schema.Interpretation (
  -- * Interpretation
  Term(..), Field(..), FieldValue(..)
, NS(..), NP(..), Proxy(..)
  -- * Transforming the wrapper type
, transWrap, transWrapNoMaps
  -- ** For internal use only
, transFields, transFieldsNoMaps
, transValue, transValueNoMaps
) where

import           Data.Map
import           Data.Proxy
import           Data.SOP

import           Mu.Schema.Definition

-- | Interpretation of a type in a schema.
data Term w (sch :: Schema typeName fieldName) (t :: TypeDef typeName fieldName) where
  -- | A record given by the value of its fields.
  TRecord :: NP (Field w sch) args -> Term w sch ('DRecord name args)
  -- | An enumeration given by one choice.
  TEnum   :: NS Proxy choices      -> Term w sch ('DEnum name choices)
  -- | A primitive value.
  TSimple :: FieldValue w sch t    -> Term w sch ('DSimple t)

-- | Interpretation of a field.
data Field w (sch :: Schema typeName fieldName) (f :: FieldDef typeName fieldName) where
  -- | A single field. Note that the contents are wrapped in a @w@ type constructor.
  Field :: w (FieldValue w sch t) -> Field w sch ('FieldDef name t)

-- | Interpretation of a field type, by giving a value of that type.
data FieldValue w (sch :: Schema typeName fieldName) (t :: FieldType typeName) where
  -- | Null value, as found in Avro and JSON.
  FNull      :: FieldValue w sch 'TNull
  -- | Value of a primitive type.
  FPrimitive :: t -> FieldValue w sch ('TPrimitive t)
  -- | Term of another type in the schema.
  FSchematic :: Term w sch (sch :/: t)
             -> FieldValue w sch ('TSchematic t)
  -- | Optional value.
  FOption    :: Maybe (FieldValue w sch t)
             -> FieldValue w sch ('TOption t)
  -- | List of values.
  FList      :: [FieldValue w sch t]
             -> FieldValue w sch ('TList   t)
  -- | Dictionary (key-value map) of values.
  FMap       :: Ord (FieldValue w sch k)
             => Map (FieldValue w sch k) (FieldValue w sch v)
             -> FieldValue w sch ('TMap k v)
  -- | One single value of one of the specified types.
  FUnion     :: NS (FieldValue w sch) choices
             -> FieldValue w sch ('TUnion choices)

-- | Change the underlying wrapper of a term.
transWrap
  :: forall tn fn (sch :: Schema tn fn) t u v.
     (Functor u, forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k))
  => (forall a. u a -> v a)
  -> Term u sch t -> Term v sch t
transWrap n x = case x of
  TRecord f -> TRecord (transFields n f)
  TEnum   c -> TEnum c
  TSimple v -> TSimple (transValue n v)

-- | Change the underlying wrapper of a term.
--   This version assumes that no field is a map,
--   which allows for a more general type.
--   If a map is found, an exception is raised.
transWrapNoMaps
  :: forall tn fn (sch :: Schema tn fn) t u v.
     (Functor u)
  => (forall a. u a -> v a)
  -> Term u sch t -> Term v sch t
transWrapNoMaps n x = case x of
  TRecord f -> TRecord (transFieldsNoMaps n f)
  TEnum   c -> TEnum c
  TSimple v -> TSimple (transValueNoMaps n v)

-- | Change the underlying wrapper of a list of fields.
transFields
  :: forall tn fn (sch :: Schema tn fn) args u v.
     (Functor u, forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k))
  => (forall a. u a -> v a)
  -> NP (Field u sch) args -> NP (Field v sch) args
transFields _ Nil = Nil
transFields n (Field v :* rest)
  = Field (n (fmap (transValue n) v)) :* transFields n rest

-- | Change the underlying wrapper of a list of fields.
--   This version assumes no maps are present as fields.
transFieldsNoMaps
  :: forall tn fn (sch :: Schema tn fn) args u v.
     (Functor u)
  => (forall a. u a -> v a)
  -> NP (Field u sch) args -> NP (Field v sch) args
transFieldsNoMaps _ Nil = Nil
transFieldsNoMaps n (Field v :* rest)
  = Field (n (fmap (transValueNoMaps n) v)) :* transFieldsNoMaps n rest

-- | Change the underlying wrapper of a value.
transValue
  :: forall tn fn (sch :: Schema tn fn) l u v.
     (Functor u, forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k))
  => (forall a. u a -> v a)
  -> FieldValue u sch l -> FieldValue v sch l
transValue _ FNull          = FNull
transValue _ (FPrimitive y) = FPrimitive y
transValue n (FSchematic t) = FSchematic (transWrap n t)
transValue n (FOption    o) = FOption (transValue n <$> o)
transValue n (FList      l) = FList (transValue n <$> l)
transValue n (FMap       m) = FMap (mapKeys (transValue n) (transValue n <$> m))
transValue n (FUnion     u) = FUnion (transUnion u)
  where
    transUnion :: NS (FieldValue u sch) us -> NS (FieldValue v sch) us
    transUnion (Z z) = Z (transValue n z)
    transUnion (S s) = S (transUnion s)

-- | Change the underlying wrapper of a value.
--   This version assumes that the value is not a map.
transValueNoMaps
  :: forall tn fn (sch :: Schema tn fn) l u v.
     (Functor u)
  => (forall a. u a -> v a)
  -> FieldValue u sch l -> FieldValue v sch l
transValueNoMaps _ FNull          = FNull
transValueNoMaps _ (FPrimitive y) = FPrimitive y
transValueNoMaps n (FSchematic t) = FSchematic (transWrapNoMaps n t)
transValueNoMaps n (FOption    o) = FOption (transValueNoMaps n <$> o)
transValueNoMaps n (FList      l) = FList (transValueNoMaps n <$> l)
transValueNoMaps _ (FMap       _) = error "this should never happen"
transValueNoMaps n (FUnion     u) = FUnion (transUnion u)
  where
    transUnion :: NS (FieldValue u sch) us -> NS (FieldValue v sch) us
    transUnion (Z z) = Z (transValueNoMaps n z)
    transUnion (S s) = S (transUnion s)

-- ===========================
-- CRAZY EQ AND SHOW INSTANCES
-- ===========================

instance All (Eq `Compose` Field w sch) args
         => Eq (Term w sch ('DRecord name args)) where
  TRecord xs == TRecord ys = xs == ys
instance (KnownName name, All (Show `Compose` Field w sch) args)
         => Show (Term w sch ('DRecord name args)) where
  show (TRecord xs) = "record " ++ nameVal (Proxy @name) ++ " { " ++ printFields xs ++ " }"
    where printFields :: forall fs. All (Show `Compose` Field w sch) fs
                      => NP (Field w sch) fs -> String
          printFields Nil         = ""
          printFields (x :* Nil)  = show x
          printFields (x :* rest) = show x ++ ", " ++ printFields rest
instance All (Eq `Compose` Proxy) choices => Eq (Term w sch ('DEnum name choices)) where
  TEnum x == TEnum y = x == y
instance (KnownName name, All KnownName choices, All (Show `Compose` Proxy) choices)
         => Show (Term w sch ('DEnum name choices)) where
  show (TEnum choice) = "enum " ++ nameVal (Proxy @name) ++ " { " ++ printChoice choice ++ " }"
    where printChoice :: forall cs. All KnownName cs => NS Proxy cs -> String
          printChoice (Z p) = nameVal p
          printChoice (S n) = printChoice n
instance Eq (FieldValue w sch t) => Eq (Term w sch ('DSimple t)) where
  TSimple x == TSimple y = x == y
instance Show (FieldValue w sch t) => Show (Term w sch ('DSimple t)) where
  show (TSimple x) = show x

instance (Eq (w (FieldValue w sch t))) => Eq (Field w sch ('FieldDef name t)) where
  Field x == Field y = x == y
instance (KnownName name, Show (w (FieldValue w sch t)))
         => Show (Field w sch ('FieldDef name t)) where
  show (Field x) = nameVal (Proxy @name) ++ ": " ++ show x

instance Eq (FieldValue w sch 'TNull) where
  _ == _ = True
instance Eq t => Eq (FieldValue w sch ('TPrimitive t)) where
  FPrimitive x == FPrimitive y = x == y
instance Eq (Term w sch (sch :/: t)) => Eq (FieldValue w sch ('TSchematic t)) where
  FSchematic x == FSchematic y = x == y
instance Eq (FieldValue w sch t) => Eq (FieldValue w sch ('TOption t)) where
  FOption x == FOption y = x == y
instance Eq (FieldValue w sch t) => Eq (FieldValue w sch ('TList t)) where
  FList x == FList y = x == y
instance (Eq (FieldValue w sch k), Eq (FieldValue w sch v))
         => Eq (FieldValue w sch ('TMap k v)) where
  FMap x == FMap y = x == y
instance All (Eq `Compose` FieldValue w sch) choices
         => Eq (FieldValue w sch ('TUnion choices)) where
  FUnion x == FUnion y = x == y

instance Ord (FieldValue w sch 'TNull) where
  compare _ _ = EQ
instance Ord t => Ord (FieldValue w sch ('TPrimitive t)) where
  compare (FPrimitive x) (FPrimitive y) = compare x y
instance Ord (Term w sch (sch :/: t)) => Ord (FieldValue w sch ('TSchematic t)) where
  compare (FSchematic x) (FSchematic y) = compare x y
instance Ord (FieldValue w sch t) => Ord (FieldValue w sch ('TOption t)) where
  compare (FOption x) (FOption y) = compare x y
instance Ord (FieldValue w sch t) => Ord (FieldValue w sch ('TList t)) where
  compare (FList x) (FList y) = compare x y
instance (Ord (FieldValue w sch k), Ord (FieldValue w sch v))
         => Ord (FieldValue w sch ('TMap k v)) where
  compare (FMap x) (FMap y) = compare x y
instance ( All (Ord `Compose` FieldValue w sch) choices
         , All (Eq  `Compose` FieldValue w sch) choices )
         => Ord (FieldValue w sch ('TUnion choices)) where
  compare (FUnion x) (FUnion y) = compare x y

instance Show (FieldValue w sch 'TNull) where
  show _ = "null"
instance Show t => Show (FieldValue w sch ('TPrimitive t)) where
  show (FPrimitive x) = show x
instance Show (Term w sch (sch :/: t)) => Show (FieldValue w sch ('TSchematic t)) where
  show (FSchematic x) = show x
instance Show (FieldValue w sch t) => Show (FieldValue w sch ('TOption t)) where
  show (FOption Nothing)  = "none"
  show (FOption (Just x)) = "some(" ++ show x ++ ")"
instance Show (FieldValue w sch t) => Show (FieldValue w sch ('TList t)) where
  show (FList xs) = show xs
instance (Show (FieldValue w sch k), Show (FieldValue w sch v))
         => Show (FieldValue w sch ('TMap k v)) where
  show (FMap x) = show x
instance All (Show `Compose` FieldValue w sch) choices
         => Show (FieldValue w sch ('TUnion choices)) where
  show (FUnion x) = show x
