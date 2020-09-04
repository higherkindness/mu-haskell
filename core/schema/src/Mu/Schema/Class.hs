{-# language DataKinds              #-}
{-# language DefaultSignatures      #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language PolyKinds              #-}
{-# language QuantifiedConstraints  #-}
{-# language RankNTypes             #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeFamilies           #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-|
Description : Conversion from types to schemas

This module defines a couple of type classes
'ToSchema' and 'FromSchema' to turn Haskell
types back and forth @mu-haskell@ 'Term's.

In most cases, the instances can be automatically
derived. If you enable the extensions
@DeriveGeneric@ and @DeriveAnyClass@, you can do:

> data MyHaskellType = ...
>   deriving ( ToSchema   MySchema "MySchemaType" MyHaskellType
>            , FromSchema MySchema "MySchemaType" MyHaskellType)

If the default mapping which required identical
names for fields in the Haskell and schema types
does not suit you, use 'CustomFieldMapping'.
-}
module Mu.Schema.Class (
  WithSchema(..), unWithSchema
, FromSchema(..), fromSchema'
, ToSchema(..), toSchema'
, CustomFieldMapping(..)
, Mapping(..), Mappings, MappingRight, MappingLeft
  -- * Internal use only
, GToSchemaRecord(..)
) where

import           Data.Kind
import           Data.Map                 as M
import           Data.SOP
import           GHC.Generics
import           GHC.TypeLits

import           Mu.Schema.Definition
import           Mu.Schema.Interpretation

-- | Tags a value with its schema.
--   For usage with @deriving via@.
newtype WithSchema (sch :: Schema tn fn) (sty :: tn) a where
  WithSchema :: forall tn fn (sch :: Schema tn fn) (sty :: tn) a.
                a -> WithSchema sch sty a

-- | Accessor for 'WithSchema'.
--   Intended for usage with @TypeApplications@.
unWithSchema :: forall tn fn (sch :: Schema tn fn) (sty :: tn) a.
                WithSchema sch sty a -> a
unWithSchema (WithSchema x) = x

-- | Defines the conversion of a type @t@ into a 'Term'
--   which follows the schema @sch@.
--   You can give an optional mapping between the
--   field names of @t@ and that of @sty@
--   by means of 'CustomFieldMapping'.
class ToSchema (sch :: Schema typeName fieldName) (sty :: typeName) (t :: Type)
      | sch t -> sty where
  -- | Conversion from Haskell type to schema term.
  toSchema   :: t -> Term sch (sch :/: sty)

  default
    toSchema :: (Generic t, GToSchemaTypeDef sch '[] (sch :/: sty) (Rep t))
              => t -> Term sch (sch :/: sty)
  toSchema x = toSchemaTypeDef (Proxy @'[]) (from x)

-- | Defines the conversion from a 'Term'
--   which follows the schema @sch@ into a type @t@.
--   You can give an optional mapping between the
--   field names of @t@ and that of @sty@
--   by means of 'CustomFieldMapping'.
class FromSchema (sch :: Schema typeName fieldName) (sty :: typeName) (t :: Type)
      | sch t -> sty where
  -- | Conversion from schema term to Haskell type.
  fromSchema :: Term sch (sch :/: sty) -> t

  default
    fromSchema :: (Generic t, GFromSchemaTypeDef sch '[] (sch :/: sty) (Rep t) )
               => Term sch (sch :/: sty) -> t
  fromSchema x = to (fromSchemaTypeDef (Proxy @'[]) x)

instance (sch :/: sty ~ 'DRecord sty fields)
         => ToSchema sch sty (Term sch ('DRecord sty fields)) where
  toSchema = id
instance (sch :/: sty ~ 'DEnum sty choices)
         => ToSchema sch sty (Term sch ('DEnum sty choices)) where
  toSchema = id
instance (sch :/: sty ~ 'DRecord sty fields)
         => FromSchema sch sty (Term sch ('DRecord sty fields)) where
  fromSchema = id
instance (sch :/: sty ~ 'DEnum sty choices)
         => FromSchema sch sty (Term sch ('DEnum sty choices)) where
  fromSchema = id

-- | Conversion from Haskell type to schema term.
--   This version is intended for usage with @TypeApplications@:
--   > toSchema' @MySchema myValue
toSchema' :: forall fn tn (sch :: Schema tn fn) t sty.
             ToSchema sch sty t => t -> Term sch (sch :/: sty)
toSchema' = toSchema
-- | Conversion from schema term to Haskell type.
--   This version is intended for usage with @TypeApplications@:
--   > fromSchema' @MySchema mySchemaTerm
fromSchema' :: forall fn tn (sch :: Schema tn fn) t sty.
               FromSchema sch sty t => Term sch (sch :/: sty) -> t
fromSchema' = fromSchema

-- | By default, the names of the fields in the Haskell type
--   and those of the schema types must coincide. By using
--   this wrapper you can override this default setting.
--
--   This type should be used with @DerivingVia@, as follows:
--
--   > type MyCustomFieldMapping = '[ "A" ':-> "a", ...]
--   > data MyHaskellType = ...
--   >   deriving ( ToSchema   f MySchema "MySchemaType" MyHaskellType
--   >            , FromSchema f MySchema "MySchemaType" MyHaskellType)
--   >     via (CustomFieldMapping "MySchemaType" MyCustomFieldMapping MyHaskellType)
newtype CustomFieldMapping (sty :: typeName) (fmap :: [Mapping Symbol fieldName])  a
  = CustomFieldMapping a

instance (Generic t, GToSchemaTypeDef sch fmap (sch :/: sty) (Rep t))
         => ToSchema sch sty (CustomFieldMapping sty fmap t) where
  toSchema (CustomFieldMapping x) = toSchemaTypeDef (Proxy @fmap) (from x)

instance (Generic t, GFromSchemaTypeDef sch fmap (sch :/: sty) (Rep t))
         => FromSchema sch sty (CustomFieldMapping sty fmap t) where
  fromSchema x = CustomFieldMapping $ to (fromSchemaTypeDef (Proxy @fmap) x)

-- ======================
-- CRAZY GENERICS SECTION
-- ======================

-- Auxiliary type families to find elements in lists
-- They return an indication of where the thing was found
--
-- Note: it turns out that GHC.Generics generates some weird
-- instances for records in the form (x :*: y) :*: z
-- and we cover them with the special HereLeft and HereRight
data Where = Here | HereLeft | HereRight | HereRightThenLeft | HereTwoRights | There Where

type family Find (xs :: [k]) (x :: k) :: Where where
  Find '[]       y = TypeError ('Text "Could not find " ':<>: 'ShowType y)
  Find (y ': xs) y = 'Here
  Find (x ': xs) y = 'There (Find xs y)

type family FindCon (xs :: * -> *) (x :: Symbol) :: Where where
  FindCon (C1 ('MetaCons x p s) f) x = 'Here
  FindCon (C1 ('MetaCons x p s) f :+: rest) x = 'Here
  FindCon (other :+: rest) x = 'There (FindCon rest x)
  FindCon nothing          x = TypeError ('Text "Could not find constructor " ':<>: 'ShowType x)

type family FindSel (xs :: * -> *) (x :: Symbol) :: Where where
  FindSel (S1 ('MetaSel ('Just x) u ss ds) f) x = 'Here
  FindSel (S1 ('MetaSel ('Just x) u ss ds) f :*: rest) x = 'Here
  FindSel ((S1 ('MetaSel ('Just x) u ss ds) f :*: other) :*: rest) x = 'HereLeft
  FindSel ((other :*: S1 ('MetaSel ('Just x) u ss ds) f) :*: rest) x = 'HereRight
  FindSel ((other1 :*: (S1 ('MetaSel ('Just x) u ss ds) f :*: other2)) :*: rest) x = 'HereRightThenLeft
  FindSel ((other1 :*: (other2 :*: S1 ('MetaSel ('Just x) u ss ds) f)) :*: rest) x = 'HereTwoRights
  FindSel (other :*: rest) x = 'There (FindSel rest x)
  FindSel nothing          x = TypeError ('Text "Could not find selector " ':<>: 'ShowType x)

type family FindEnumChoice (xs :: [ChoiceDef fs]) (x :: fs) :: Where where
  FindEnumChoice '[] x = TypeError ('Text "Could not find enum choice " ':<>: 'ShowType x)
  FindEnumChoice ('ChoiceDef name ': xs) name = 'Here
  FindEnumChoice (other           ': xs) name = 'There (FindEnumChoice xs name)

type family FindField (xs :: [FieldDef ts fs]) (x :: fs) :: Where where
  FindField '[] x = TypeError ('Text "Could not find field " ':<>: 'ShowType x)
  FindField ('FieldDef name t ': xs) name = 'Here
  FindField (other            ': xs) name = 'There (FindField xs name)

-- Generic type definitions
class GToSchemaTypeDef
        (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
        (t :: TypeDef ts fs) (f :: * -> *) where
  toSchemaTypeDef   :: Proxy fmap -> f a -> Term sch t
class GFromSchemaTypeDef
        (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
        (t :: TypeDef ts fs) (f :: * -> *) where
  fromSchemaTypeDef :: Proxy fmap -> Term sch t -> f a

-- ------------------
-- TYPES OF FIELDS --
-- ------------------

instance GToSchemaFieldTypeWrap sch t f
         => GToSchemaTypeDef sch fmap ('DSimple t) f where
  toSchemaTypeDef _ x = TSimple (toSchemaFieldTypeW x)
instance GFromSchemaFieldTypeWrap sch t f
         => GFromSchemaTypeDef sch fmap ('DSimple t) f where
  fromSchemaTypeDef _ (TSimple x) = fromSchemaFieldTypeW x

class GToSchemaFieldTypeWrap
        (sch :: Schema ts fs) (t :: FieldType ts) (f :: * -> *) where
  toSchemaFieldTypeW   :: f a -> FieldValue sch t
class GFromSchemaFieldTypeWrap
        (sch :: Schema ts fs) (t :: FieldType ts) (f :: * -> *) where
  fromSchemaFieldTypeW :: FieldValue sch t -> f a

instance GToSchemaFieldType sch t f
         => GToSchemaFieldTypeWrap sch t (K1 i f) where
  toSchemaFieldTypeW (K1 x) = toSchemaFieldType x
instance GFromSchemaFieldType sch t f
         => GFromSchemaFieldTypeWrap sch t (K1 i f) where
  fromSchemaFieldTypeW x = K1 (fromSchemaFieldType x)
instance GToSchemaFieldTypeWrap sch t f
         => GToSchemaFieldTypeWrap sch t (M1 s m f) where
  toSchemaFieldTypeW (M1 x) = toSchemaFieldTypeW x
instance GFromSchemaFieldTypeWrap sch t f
         => GFromSchemaFieldTypeWrap sch t (M1 s m f) where
  fromSchemaFieldTypeW x = M1 (fromSchemaFieldTypeW x)

class GToSchemaFieldType
        (sch :: Schema ts fs) (t :: FieldType ts) (f :: *) where
  toSchemaFieldType   :: f -> FieldValue sch t
class GFromSchemaFieldType
        (sch :: Schema ts fs) (t :: FieldType ts) (f :: *) where
  fromSchemaFieldType :: FieldValue sch t -> f

class GToSchemaFieldTypeUnion
        (sch :: Schema ts fs) (t :: [FieldType ts]) (f :: * -> *) where
  toSchemaFieldTypeUnion   :: f a -> NS (FieldValue sch) t
class GFromSchemaFieldTypeUnion
        (sch :: Schema ts fs) (t :: [FieldType ts]) (f :: * -> *) where
  fromSchemaFieldTypeUnion :: NS (FieldValue sch) t -> f a

-- These instances are straightforward,
-- just turn the "real types" into their
-- schema correspondants.
instance GToSchemaFieldType sch 'TNull () where
  toSchemaFieldType _   = FNull
instance GFromSchemaFieldType sch 'TNull () where
  fromSchemaFieldType _ = ()
instance GToSchemaFieldType sch ('TPrimitive t) t where
  toSchemaFieldType = FPrimitive
instance GFromSchemaFieldType sch ('TPrimitive t) t where
  fromSchemaFieldType (FPrimitive x) = x
-- These instances "tie the loop" with the whole schema,
-- and they are the reason why we need to thread the @sch@
-- type throghout the whole implementation.
instance ToSchema sch t v
         => GToSchemaFieldType sch ('TSchematic t) v where
  toSchemaFieldType x = FSchematic $ toSchema x
instance FromSchema sch t v
         => GFromSchemaFieldType sch ('TSchematic t) v where
  fromSchemaFieldType (FSchematic x) = fromSchema x
instance GToSchemaFieldType sch t v
         => GToSchemaFieldType sch ('TOption t) (Maybe v) where
  toSchemaFieldType x = FOption (toSchemaFieldType <$> x)
instance GFromSchemaFieldType sch t v
         => GFromSchemaFieldType sch ('TOption t) (Maybe v) where
  fromSchemaFieldType (FOption x) = fromSchemaFieldType <$> x
instance GToSchemaFieldType sch t v
         => GToSchemaFieldType sch ('TList t) [v] where
  toSchemaFieldType x = FList (toSchemaFieldType <$> x)
instance GFromSchemaFieldType sch t v
         => GFromSchemaFieldType sch ('TList t) [v] where
  fromSchemaFieldType (FList x) = fromSchemaFieldType <$> x
instance (GToSchemaFieldType sch sk hk, GToSchemaFieldType sch sv hv,
          Ord (FieldValue sch sk))  -- Ord is required to build a map
         => GToSchemaFieldType sch ('TMap sk sv) (M.Map hk hv) where
  toSchemaFieldType x = FMap (M.mapKeys toSchemaFieldType (M.map toSchemaFieldType x))
instance (GFromSchemaFieldType sch sk hk, GFromSchemaFieldType sch sv hv, Ord hk)
         => GFromSchemaFieldType sch ('TMap sk sv) (M.Map hk hv) where
  fromSchemaFieldType (FMap x) = M.mapKeys fromSchemaFieldType (M.map fromSchemaFieldType x)
-- This assumes that a union is represented by
-- a value of type 'NS', where types are in
-- the same order.
instance {-# OVERLAPS #-}
         AllZip (GToSchemaFieldType sch) ts vs
         => GToSchemaFieldType sch ('TUnion ts) (NS I vs) where
  toSchemaFieldType t = FUnion (go t)
    where go :: AllZip (GToSchemaFieldType sch) tss vss
             => NS I vss -> NS (FieldValue sch) tss
          go (Z (I x)) = Z (toSchemaFieldType x)
          go (S n)     = S (go n)
instance {-# OVERLAPS #-}
         AllZip (GFromSchemaFieldType sch) ts vs
         => GFromSchemaFieldType sch ('TUnion ts) (NS I vs) where
  fromSchemaFieldType (FUnion t) = go t
    where go :: AllZip (GFromSchemaFieldType sch) tss vss
             => NS (FieldValue sch) tss -> NS I vss
          go (Z x) = Z (I (fromSchemaFieldType x))
          go (S n) = S (go n)
-- But we can also use any other if it has
-- the right structure
instance {-# OVERLAPPABLE #-}
         (Generic f, GToSchemaFieldTypeUnion sch ts (Rep f))
         => GToSchemaFieldType sch ('TUnion ts) f where
  toSchemaFieldType x = FUnion (toSchemaFieldTypeUnion (from x))
instance {-# OVERLAPPABLE #-}
         (Generic f, GFromSchemaFieldTypeUnion sch ts (Rep f))
         => GFromSchemaFieldType sch ('TUnion ts) f where
  fromSchemaFieldType (FUnion x) = to (fromSchemaFieldTypeUnion x)

instance {-# OVERLAPS #-} GToSchemaFieldTypeUnion sch '[] U1 where
  toSchemaFieldTypeUnion U1 = error "this should never happen"
instance {-# OVERLAPS #-} GFromSchemaFieldTypeUnion sch '[] U1 where
  fromSchemaFieldTypeUnion _ = U1
instance {-# OVERLAPS #-} GToSchemaFieldTypeUnion sch '[] (M1 i t U1) where
  toSchemaFieldTypeUnion (M1 U1) = error "this should never happen"
instance {-# OVERLAPS #-} GFromSchemaFieldTypeUnion sch '[] (M1 i t U1) where
  fromSchemaFieldTypeUnion _ = M1 U1
instance {-# OVERLAPPABLE #-}
         TypeError ('Text "the type does not match the union")
         => GToSchemaFieldTypeUnion sch '[] f where
  toSchemaFieldTypeUnion = error "this should never happen"
instance {-# OVERLAPPABLE #-}
         TypeError ('Text "the type does not match the union")
         => GFromSchemaFieldTypeUnion sch '[] f where
  fromSchemaFieldTypeUnion = error "this should never happen"

instance (GToSchemaFieldTypeWrap sch t v)
         => GToSchemaFieldTypeUnion sch '[t] v where
  toSchemaFieldTypeUnion   x     = Z (toSchemaFieldTypeW x)
instance (GFromSchemaFieldTypeWrap sch t v)
         => GFromSchemaFieldTypeUnion sch '[t] v where
  fromSchemaFieldTypeUnion (Z x) = fromSchemaFieldTypeW x
  fromSchemaFieldTypeUnion (S _) = error "this should never happen"

-- remove M1 from thing with more than one element
instance {-# OVERLAPS #-} (GToSchemaFieldTypeUnion sch (a ': b ': rest) v)
         => GToSchemaFieldTypeUnion sch (a ': b ': rest) (M1 i t v) where
  toSchemaFieldTypeUnion (M1 x) = toSchemaFieldTypeUnion x
instance {-# OVERLAPS #-} (GFromSchemaFieldTypeUnion sch (a ': b ': rest) v)
         => GFromSchemaFieldTypeUnion sch (a ': b ': rest) (M1 i t v) where
  fromSchemaFieldTypeUnion x = M1 (fromSchemaFieldTypeUnion x)

instance (GToSchemaFieldTypeWrap sch t v, GToSchemaFieldTypeUnion sch ts vs)
         => GToSchemaFieldTypeUnion sch (t ': ts) (v :+: vs) where
  toSchemaFieldTypeUnion (L1 x) = Z (toSchemaFieldTypeW x)
  toSchemaFieldTypeUnion (R1 r) = S (toSchemaFieldTypeUnion r)
instance (GFromSchemaFieldTypeWrap sch t v, GFromSchemaFieldTypeUnion sch ts vs)
         => GFromSchemaFieldTypeUnion sch (t ': ts) (v :+: vs) where
  fromSchemaFieldTypeUnion (Z x) = L1 (fromSchemaFieldTypeW x)
  fromSchemaFieldTypeUnion (S r) = R1 (fromSchemaFieldTypeUnion r)
-- Weird nested instance produced by GHC
instance ( GToSchemaFieldTypeWrap sch t1 v1
         , GToSchemaFieldTypeWrap sch t2 v2
         , GToSchemaFieldTypeUnion sch ts vs )
         => GToSchemaFieldTypeUnion sch (t1 ': t2 ': ts) ((v1 :+: v2) :+: vs) where
  toSchemaFieldTypeUnion (L1 (L1 x)) = Z (toSchemaFieldTypeW x)
  toSchemaFieldTypeUnion (L1 (R1 x)) = S (Z (toSchemaFieldTypeW x))
  toSchemaFieldTypeUnion (R1 r)      = S (S (toSchemaFieldTypeUnion r))
instance ( GFromSchemaFieldTypeWrap sch t1 v1
         , GFromSchemaFieldTypeWrap sch t2 v2
         , GFromSchemaFieldTypeUnion sch ts vs )
         => GFromSchemaFieldTypeUnion sch (t1 ': t2 ': ts) ((v1 :+: v2) :+: vs) where
  fromSchemaFieldTypeUnion (Z x)     = L1 (L1 (fromSchemaFieldTypeW x))
  fromSchemaFieldTypeUnion (S (Z x)) = L1 (R1 (fromSchemaFieldTypeW x))
  fromSchemaFieldTypeUnion (S (S r)) = R1 (fromSchemaFieldTypeUnion r)


-- ---------------
-- ENUMERATIONS --
------------------

instance {-# OVERLAPPABLE #-}
         (GToSchemaEnumDecompose fmap choices f)
         => GToSchemaTypeDef sch fmap ('DEnum name choices) f where
  toSchemaTypeDef p x = TEnum (toSchemaEnumDecomp p x)
instance {-# OVERLAPPABLE #-}
         (GFromSchemaEnumDecompose fmap choices f)
         => GFromSchemaTypeDef sch fmap ('DEnum name choices) f where
  fromSchemaTypeDef p (TEnum x) = fromSchemaEnumDecomp p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GToSchemaTypeDef sch fmap ('DEnum name choices) f
         => GToSchemaTypeDef sch fmap ('DEnum name choices) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef sch fmap ('DEnum name choices) f
         => GFromSchemaTypeDef sch fmap ('DEnum name choices) (D1 meta f) where
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)

-- 'toSchema' for enumerations:
-- 1. recursively decompose the (:+:)s into their atomic components
--    this is done by 'GToSchemaEnumSymbol'
-- 2. for each atomic component, figure out which is the element
--    in the schema's enumeration that it corresponds to
--    this is done by 'MappingRight' and 'Find'
-- 3. from that location, build a 'Proxy' value
--    this is done by 'GToSchemaEnumProxy'
class GToSchemaEnumDecompose (fmap :: Mappings Symbol fs)
                             (choices :: [ChoiceDef fs]) (f :: * -> *) where
  toSchemaEnumDecomp :: Proxy fmap -> f a -> NS Proxy choices
instance (GToSchemaEnumDecompose fmap choices oneway, GToSchemaEnumDecompose fmap choices oranother)
         => GToSchemaEnumDecompose fmap choices (oneway :+: oranother) where
  toSchemaEnumDecomp p (L1 x) = toSchemaEnumDecomp p x
  toSchemaEnumDecomp p (R1 x) = toSchemaEnumDecomp p x
instance GToSchemaEnumProxy choices (FindEnumChoice choices (MappingRight fmap c))
         => GToSchemaEnumDecompose fmap choices (C1 ('MetaCons c p s) f) where
  toSchemaEnumDecomp _ _
    = toSchemaEnumProxy (Proxy @choices) (Proxy @(FindEnumChoice choices (MappingRight fmap c)))
-- Types which have no constructor information cannot be used here

class GToSchemaEnumProxy (choices :: [k]) (w :: Where) where
  toSchemaEnumProxy :: Proxy choices -> Proxy w -> NS Proxy choices
instance GToSchemaEnumProxy (c ': cs) 'Here where
  toSchemaEnumProxy _ _ = Z Proxy
instance forall c cs w. GToSchemaEnumProxy cs w
         => GToSchemaEnumProxy (c ': cs) ('There w) where
  toSchemaEnumProxy _ _ = S (toSchemaEnumProxy (Proxy @cs) (Proxy @w))

-- 'fromSchema' for enumerations:
-- 1. for each element in the list of choices
--    (this iteration is done by 'GFromSchemaEnumDecomp')
--    figure out the constructor it corresponds to
--    this is done by 'MappingLeft' and 'FindCon'
-- 2. from that location, build a 'U1' value wrapped
--    in as many 'L1' and 'R1' required.
--    this is done by 'GFromSchemaEnumU1'
class GFromSchemaEnumDecompose (fmap :: Mappings Symbol fs) (choices :: [ChoiceDef fs]) (f :: * -> *) where
  fromSchemaEnumDecomp :: Proxy fmap -> NS Proxy choices -> f a
instance GFromSchemaEnumDecompose fmap '[] f where
  fromSchemaEnumDecomp _ _ = error "This should never happen"
instance (GFromSchemaEnumU1 f (FindCon f (MappingLeft fmap c)), GFromSchemaEnumDecompose fmap cs f)
         => GFromSchemaEnumDecompose fmap ('ChoiceDef c ': cs) f where
  fromSchemaEnumDecomp _ (Z _) = fromSchemaEnumU1 (Proxy @f) (Proxy @(FindCon f (MappingLeft fmap c)))
  fromSchemaEnumDecomp p (S x) = fromSchemaEnumDecomp p x

class GFromSchemaEnumU1 (f :: * -> *) (w :: Where) where
  fromSchemaEnumU1 :: Proxy f -> Proxy w -> f a
instance GFromSchemaEnumU1 (C1 m U1 :+: rest) 'Here where
  fromSchemaEnumU1 _ _ = L1 (M1 U1)
instance GFromSchemaEnumU1 (C1 m U1) 'Here where
  fromSchemaEnumU1 _ _ = M1 U1
instance forall other rest w. GFromSchemaEnumU1 rest w
         => GFromSchemaEnumU1 (other :+: rest) ('There w) where
  fromSchemaEnumU1 _ _ = R1 (fromSchemaEnumU1 (Proxy @rest) (Proxy @w))

-- ----------
-- RECORDS --
-------------

instance {-# OVERLAPPABLE #-}
         (GToSchemaRecord sch fmap args f)
         => GToSchemaTypeDef sch fmap ('DRecord name args) f where
  toSchemaTypeDef p x = TRecord (toSchemaRecord p x)
instance {-# OVERLAPPABLE #-}
         (GFromSchemaRecord sch fmap args f)
         => GFromSchemaTypeDef sch fmap ('DRecord name args) f where
  fromSchemaTypeDef p (TRecord x) = fromSchemaRecord p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GToSchemaTypeDef sch fmap ('DRecord name args) f
         => GToSchemaTypeDef sch fmap ('DRecord name args) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef sch fmap ('DRecord name args) f
         => GFromSchemaTypeDef sch fmap ('DRecord name args) (D1 meta f) where
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)
instance {-# OVERLAPS #-}
         GToSchemaTypeDef sch fmap ('DRecord name args) f
         => GToSchemaTypeDef sch fmap ('DRecord name args) (C1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef sch fmap ('DRecord name args) f
         => GFromSchemaTypeDef sch fmap ('DRecord name args) (C1 meta f) where
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)

-- 'toSchema' for records:
-- 1. iterate over each field in the schema of the record
--    this is done by 'GToSchemaRecord'
-- 2. figure out the selector (field) in the Haskell type
--    to which that record corresponds to
--    this is done by 'MappingLeft' and 'FindSel'
-- 3. using that location, obtain the value of the field
--    this is done by 'GToSchemaRecordSearch'
--
-- Due to some glitch in 'GHC.Generics', sometimes products
-- are not represented by a linear sequence of ':*:',
-- so we need to handle some cases in a special way
-- (see 'HereLeft' and 'HereRight' instances)

-- | For internal use only: generic conversion of a list of fields.
class GToSchemaRecord (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                      (args :: [FieldDef ts fs]) (f :: * -> *) where
  toSchemaRecord :: Proxy fmap -> f a -> NP (Field sch) args
instance GToSchemaRecord sch fmap '[] f where
  toSchemaRecord _ _ = Nil
instance ( GToSchemaRecord sch fmap cs f
         , GToSchemaRecordSearch sch t f (FindSel f (MappingLeft fmap name)) )
         => GToSchemaRecord sch fmap ('FieldDef name t ': cs) f where
  toSchemaRecord p x = this :* toSchemaRecord p x
    where this = Field (toSchemaRecordSearch (Proxy @(FindSel f (MappingLeft fmap name))) x)

class GToSchemaRecordSearch (sch :: Schema ts fs)
                            (t :: FieldType ts) (f :: * -> *) (wh :: Where) where
  toSchemaRecordSearch :: Proxy wh -> f a -> FieldValue sch t
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t (S1 m (K1 i v)) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x)) = toSchemaFieldType x
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t (S1 m (K1 i v) :*: rest) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x) :*: _) = toSchemaFieldType x
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((S1 m (K1 i v) :*: other) :*: rest) 'HereLeft where
  toSchemaRecordSearch _ ((M1 (K1 x) :*: _) :*: _) = toSchemaFieldType x
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((other :*: S1 m (K1 i v)) :*: rest) 'HereRight where
  toSchemaRecordSearch _ ((_ :*: M1 (K1 x)) :*: _) = toSchemaFieldType x
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((other1 :*: (S1 m (K1 i v) :*: other2)) :*: rest) 'HereRightThenLeft where
  toSchemaRecordSearch _ ((_ :*: (M1 (K1 x) :*: _)) :*: _) = toSchemaFieldType x
instance GToSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((other1 :*: (other2 :*: S1 m (K1 i v))) :*: rest) 'HereTwoRights where
  toSchemaRecordSearch _ ((_ :*: (_ :*: M1 (K1 x))) :*: _) = toSchemaFieldType x
instance forall sch t other rest n.
         GToSchemaRecordSearch sch t rest n
         => GToSchemaRecordSearch sch t (other :*: rest) ('There n) where
  toSchemaRecordSearch _ (_ :*: xs) = toSchemaRecordSearch (Proxy @n) xs

-- 'fromSchema' for records
-- 1. decompose the sequence of products into atomic components
--    until we arrive to the selector metadata 'S1'
--    this is done by 'GFromSchemaRecord'
-- 2. figure out the field in the schema it corresponds to
--    this is done by 'MappingRight' and 'FindField'
-- 3. using that location, obtain the value of the field
--    this is done by 'GFromSchemaRecordSearch'
class GFromSchemaRecord (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                        (args :: [FieldDef ts fs]) (f :: * -> *) where
  fromSchemaRecord :: Proxy fmap -> NP (Field sch) args -> f a
instance (GFromSchemaRecordSearch sch v args (FindField args (MappingRight fmap name)))
         => GFromSchemaRecord sch fmap args (S1 ('MetaSel ('Just name) u ss ds) (K1 i v)) where
  fromSchemaRecord _ x
    = M1 $ K1 $ fromSchemaRecordSearch (Proxy @(FindField args (MappingRight fmap name))) x
instance ( GFromSchemaRecord sch fmap args oneway
         , GFromSchemaRecord sch fmap args oranother )
         => GFromSchemaRecord sch fmap args (oneway :*: oranother) where
  fromSchemaRecord p x =  fromSchemaRecord p x :*: fromSchemaRecord p x
instance GFromSchemaRecord sch fmap args U1 where
  fromSchemaRecord _ _ = U1

class GFromSchemaRecordSearch (sch :: Schema ts fs)
                              (v :: *) (args :: [FieldDef ts fs]) (wh :: Where) where
  fromSchemaRecordSearch :: Proxy wh -> NP (Field sch) args -> v
instance (GFromSchemaFieldType sch t v)
         => GFromSchemaRecordSearch sch v ('FieldDef name t ': rest) 'Here where
  fromSchemaRecordSearch _ (Field x :* _) = fromSchemaFieldType x
instance forall sch v other rest n.
         GFromSchemaRecordSearch sch v rest n
         => GFromSchemaRecordSearch sch v (other ': rest) ('There n) where
  fromSchemaRecordSearch _ (_ :* xs) = fromSchemaRecordSearch (Proxy @n) xs
