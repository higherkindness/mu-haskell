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
import           Data.Profunctor
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
  Field :: w (NP (FieldValue w sch) args) (FieldValue w sch t)
        -> Field w sch ('FieldDef name args t)

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
     ( Profunctor u, Profunctor v
     , forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k)
     , forall k. Ord (FieldValue v sch k) => Ord (FieldValue u sch k) )
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> Term u sch t -> Term v sch t
transWrap n b x = case x of
  TRecord f -> TRecord (transFields n b f)
  TEnum   c -> TEnum c
  TSimple v -> TSimple (transValue n b v)

-- | Change the underlying wrapper of a term.
--   This version assumes that no field is a map,
--   which allows for a more general type.
--   If a map is found, an exception is raised.
transWrapNoMaps
  :: forall tn fn (sch :: Schema tn fn) t u v.
     (Profunctor u, Profunctor v)
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> Term u sch t -> Term v sch t
transWrapNoMaps n b x = case x of
  TRecord f -> TRecord (transFieldsNoMaps n b f)
  TEnum   c -> TEnum c
  TSimple v -> TSimple (transValueNoMaps n b v)

-- | Change the underlying wrapper of a list of fields.
transFields
  :: forall tn fn (sch :: Schema tn fn) args u v.
     ( Profunctor u, Profunctor v
     , forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k)
     , forall k. Ord (FieldValue v sch k) => Ord (FieldValue u sch k) )
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> NP (Field u sch) args -> NP (Field v sch) args
transFields _ _ Nil = Nil
transFields n b (Field v :* rest)
  = Field (n (dimap (transValues b n) (transValue n b) v)) :* transFields n b rest

-- | Change the underlying wrapper of a list of fields.
--   This version assumes no maps are present as fields.
transFieldsNoMaps
  :: forall tn fn (sch :: Schema tn fn) args u v.
     (Profunctor u, Profunctor v)
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> NP (Field u sch) args -> NP (Field v sch) args
transFieldsNoMaps _ _ Nil = Nil
transFieldsNoMaps n b (Field v :* rest)
  = Field (n (dimap (transValuesNoMaps b n) (transValueNoMaps n b) v)) :* transFieldsNoMaps n b rest

transValues
  :: forall tn fn (sch :: Schema tn fn) l u v.
     ( Profunctor u, Profunctor v
     , forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k)
     , forall k. Ord (FieldValue v sch k) => Ord (FieldValue u sch k) )
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> NP (FieldValue u sch) l -> NP (FieldValue v sch) l
transValues _ _ Nil = Nil
transValues n b (x :* xs) = transValue n b x :* transValues n b xs

transValuesNoMaps
  :: forall tn fn (sch :: Schema tn fn) l u v.
     (Profunctor u, Profunctor v)
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> NP (FieldValue u sch) l -> NP (FieldValue v sch) l
transValuesNoMaps _ _ Nil = Nil
transValuesNoMaps n b (x :* xs) = transValueNoMaps n b x :* transValuesNoMaps n b xs

-- | Change the underlying wrapper of a value.
transValue
  :: forall tn fn (sch :: Schema tn fn) l u v.
     ( Profunctor u, Profunctor v
     , forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k)
     , forall k. Ord (FieldValue v sch k) => Ord (FieldValue u sch k) )
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> FieldValue u sch l -> FieldValue v sch l
transValue _ _ FNull          = FNull
transValue _ _ (FPrimitive y) = FPrimitive y
transValue n b (FSchematic t) = FSchematic (transWrap n b t)
transValue n b (FOption    o) = FOption (transValue n b <$> o)
transValue n b (FList      l) = FList (transValue n b <$> l)
transValue n b (FMap       m) = FMap (mapKeys (transValue n b) (transValue n b <$> m))
transValue n b (FUnion     u) = FUnion (transUnion u)
  where
    transUnion :: NS (FieldValue u sch) us -> NS (FieldValue v sch) us
    transUnion (Z z) = Z (transValue n b z)
    transUnion (S s) = S (transUnion s)

-- | Change the underlying wrapper of a value.
--   This version assumes that the value is not a map.
transValueNoMaps
  :: forall tn fn (sch :: Schema tn fn) l u v.
     (Profunctor u, Profunctor v)
  => (forall x a. u x a -> v x a)
  -> (forall x a. v x a -> u x a)
  -> FieldValue u sch l -> FieldValue v sch l
transValueNoMaps _ _ FNull          = FNull
transValueNoMaps _ _ (FPrimitive y) = FPrimitive y
transValueNoMaps n b (FSchematic t) = FSchematic (transWrapNoMaps n b t)
transValueNoMaps n b (FOption    o) = FOption (transValueNoMaps n b <$> o)
transValueNoMaps n b (FList      l) = FList (transValueNoMaps n b <$> l)
transValueNoMaps _ _ (FMap       _) = error "this should never happen"
transValueNoMaps n b (FUnion     u) = FUnion (transUnion u)
  where
    transUnion :: NS (FieldValue u sch) us -> NS (FieldValue v sch) us
    transUnion (Z z) = Z (transValueNoMaps n b z)
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

instance (Eq (w (NP (FieldValue w sch) args) (FieldValue w sch t)))
         => Eq (Field w sch ('FieldDef name args t)) where
  Field x == Field y = x == y
instance (KnownName name, Show (w (NP (FieldValue w sch) args) (FieldValue w sch t)))
         => Show (Field w sch ('FieldDef name args t)) where
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
