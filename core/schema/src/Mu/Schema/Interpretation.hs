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
-- | Interpretation of schemas
module Mu.Schema.Interpretation (
  Term(..), Field(..), FieldValue(..)
, NS(..), NP(..), Proxy(..)
, transWrap
) where

import           Data.Map
import           Data.Proxy
import           Data.SOP

import           Mu.Schema.Definition

-- | Interpretation of a type in a schema.
data Term w (sch :: Schema typeName fieldName) (t :: TypeDef typeName fieldName) where
  TRecord :: NP (Field w sch) args -> Term w sch ('DRecord name args)
  TEnum   :: NS Proxy choices      -> Term w sch ('DEnum name choices)
  TSimple :: FieldValue w sch t    -> Term w sch ('DSimple t)

-- | Interpretation of a field.
data Field w (sch :: Schema typeName fieldName) (f :: FieldDef typeName fieldName) where
  Field :: w (FieldValue w sch t) -> Field w sch ('FieldDef name t)

-- | Interpretation of a field type, by giving a value of that type.
data FieldValue w (sch :: Schema typeName fieldName) (t :: FieldType typeName) where
  FNull      :: FieldValue w sch 'TNull
  FPrimitive :: t -> FieldValue w sch ('TPrimitive t)
  FSchematic :: Term w sch (sch :/: t)
             -> FieldValue w sch ('TSchematic t)
  FOption    :: Maybe (FieldValue w sch t)
             -> FieldValue w sch ('TOption t)
  FList      :: [FieldValue w sch t]
             -> FieldValue w sch ('TList   t)
  FMap       :: Ord (FieldValue w sch k)
             => Map (FieldValue w sch k) (FieldValue w sch v)
             -> FieldValue w sch ('TMap k v)
  FUnion     :: NS (FieldValue w sch) choices
             -> FieldValue w sch ('TUnion choices)

transWrap
  :: forall tn fn (sch :: Schema tn fn) t u v.
     (Functor u, forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k))
  => (forall a. u a -> v a)
  -> Term u sch t -> Term v sch t
transWrap n x = case x of
  TRecord f -> TRecord (transFields f)
  TEnum   c -> TEnum c
  TSimple v -> TSimple (transValue v)
  where
    transFields :: NP (Field u sch) args -> NP (Field v sch) args
    transFields Nil               = Nil
    transFields (Field v :* rest) = Field (n (fmap transValue v)) :* transFields rest
    transValue :: FieldValue u sch l -> FieldValue v sch l
    transValue FNull          = FNull
    transValue (FPrimitive y) = FPrimitive y
    transValue (FSchematic t) = FSchematic (transWrap n t)
    transValue (FOption    o) = FOption (transValue <$> o)
    transValue (FList      l) = FList (transValue <$> l)
    transValue (FMap       m) = FMap (mapKeys transValue (transValue <$> m))
    transValue (FUnion     u) = FUnion (transUnion u)
    transUnion :: NS (FieldValue u sch) us -> NS (FieldValue v sch) us
    transUnion (Z z) = Z (transValue z)
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
