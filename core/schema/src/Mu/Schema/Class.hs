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
-- | Conversion from types to schemas
module Mu.Schema.Class (
  WithSchema(..)
, FromSchema(..), fromSchema'
, ToSchema(..), toSchema'
, CustomFieldMapping(..)
, Mapping(..), Mappings, MappingRight, MappingLeft
, transSchema
) where

import           Data.Functor.Identity
import           Data.Kind
import           Data.Map                 as M
import           Data.SOP
import           GHC.Generics
import           GHC.TypeLits

import           Mu.Schema.Definition
import           Mu.Schema.Interpretation

-- | Tags a value with its schema.
--   For usage with @deriving via@.
newtype WithSchema (w :: Type -> Type) (sch :: Schema tn fn) (sty :: tn) a = WithSchema a

-- | Defines the conversion of a type @t@ into a 'Term'
--   which follows the schema @sch@.
--   You can give an optional mapping between the
--   field names of @t@ and that of 'SchemaType'
--   by means of 'FieldMapping'.
class ToSchema (w :: Type -> Type) (sch :: Schema typeName fieldName) (sty :: typeName) (t :: Type)
      | sch t -> sty where
  -- | Conversion from Haskell type to schema term.
  toSchema   :: t -> Term w sch (sch :/: sty)

  default
    toSchema :: (Generic t, GToSchemaTypeDef w sch '[] (sch :/: sty) (Rep t))
              => t -> Term w sch (sch :/: sty)
  toSchema x = toSchemaTypeDef (Proxy @'[]) (from x)

-- | Defines the conversion from a 'Term'
--   which follows the schema @sch@ into a type @t@.
--   You can give an optional mapping between the
--   field names of @t@ and that of 'SchemaType'
--   by means of 'FieldMapping'.
class FromSchema (w :: Type -> Type) (sch :: Schema typeName fieldName) (sty :: typeName) (t :: Type)
      | sch t -> sty where
  -- | Conversion from schema term to Haskell type.
  fromSchema :: Term w sch (sch :/: sty) -> t

  default
    fromSchema :: (Generic t, GFromSchemaTypeDef w sch '[] (sch :/: sty) (Rep t) )
               => Term w sch (sch :/: sty) -> t
  fromSchema x = to (fromSchemaTypeDef (Proxy @'[]) x)

-- | Conversion from Haskell type to schema term.
--   This version is intended for usage with @TypeApplications@:
--   > toSchema' @MySchema myValue
toSchema' :: forall fn tn (sch :: Schema tn fn) w t sty.
             ToSchema w sch sty t => t -> Term w sch (sch :/: sty)
toSchema' = toSchema
-- | Conversion from schema term to Haskell type.
--   This version is intended for usage with @TypeApplications@:
--   > fromSchema' @MySchema mySchemaTerm
fromSchema' :: forall fn tn (sch :: Schema tn fn) w t sty.
               FromSchema w sch sty t => Term w sch (sch :/: sty) -> t
fromSchema' = fromSchema

newtype CustomFieldMapping (sty :: typeName) (fmap :: [Mapping Symbol fieldName])  a
  = CustomFieldMapping a

instance (Generic t, GToSchemaTypeDef w sch fmap (sch :/: sty) (Rep t))
         => ToSchema w sch sty (CustomFieldMapping sty fmap t) where
  toSchema (CustomFieldMapping x) = toSchemaTypeDef (Proxy @fmap) (from x)

instance (Generic t, GFromSchemaTypeDef w sch fmap (sch :/: sty) (Rep t))
         => FromSchema w sch sty (CustomFieldMapping sty fmap t) where
  fromSchema x = CustomFieldMapping $ to (fromSchemaTypeDef (Proxy @fmap) x)

transSchema
  :: forall fn tn (sch :: Schema tn fn) sty u v a b.
     ( ToSchema u sch sty a, FromSchema v sch sty b
     , Functor u, forall k. Ord (FieldValue u sch k) => Ord (FieldValue v sch k) )
  => (forall x. u x -> v x) -> Proxy sch -> a -> b
transSchema f _ = fromSchema @_ @_ @v @sch @sty . transWrap f . toSchema @_ @_ @u @sch @sty

-- ======================
-- CRAZY GENERICS SECTION
-- ======================

-- Auxiliary type families to find elements in lists
-- They return an indication of where the thing was found
--
-- Note: it turns out that GHC.Generics generates some weird
-- instances for records in the form (x :*: y) :*: z
-- and we cover them with the special HereLeft and HereRight
data Where = Here | HereLeft | HereRight | There Where

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
        (w :: * -> *) (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
        (t :: TypeDef ts fs) (f :: * -> *) where
  toSchemaTypeDef   :: Proxy fmap -> f a -> Term w sch t
class GFromSchemaTypeDef
        (w :: * -> *) (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
        (t :: TypeDef ts fs) (f :: * -> *) where
  fromSchemaTypeDef :: Proxy fmap -> Term w sch t -> f a

-- ------------------
-- TYPES OF FIELDS --
-- ------------------

instance GToSchemaFieldTypeWrap w sch t f
         => GToSchemaTypeDef w sch fmap ('DSimple t) f where
  toSchemaTypeDef _ x = TSimple (toSchemaFieldTypeW x)
instance GFromSchemaFieldTypeWrap w sch t f
         => GFromSchemaTypeDef w sch fmap ('DSimple t) f where
  fromSchemaTypeDef _ (TSimple x) = fromSchemaFieldTypeW x

class GToSchemaFieldTypeWrap
        (w :: * -> *) (sch :: Schema ts fs) (t :: FieldType ts) (f :: * -> *) where
  toSchemaFieldTypeW   :: f a -> FieldValue w sch t
class GFromSchemaFieldTypeWrap
        (w :: * -> *) (sch :: Schema ts fs) (t :: FieldType ts) (f :: * -> *) where
  fromSchemaFieldTypeW :: FieldValue w sch t -> f a

instance GToSchemaFieldType w sch t f
         => GToSchemaFieldTypeWrap w sch t (K1 i f) where
  toSchemaFieldTypeW (K1 x) = toSchemaFieldType x
instance GFromSchemaFieldType w sch t f
         => GFromSchemaFieldTypeWrap w sch t (K1 i f) where
  fromSchemaFieldTypeW x = K1 (fromSchemaFieldType x)
instance GToSchemaFieldTypeWrap w sch t f
         => GToSchemaFieldTypeWrap w sch t (M1 s m f) where
  toSchemaFieldTypeW (M1 x) = toSchemaFieldTypeW x
instance GFromSchemaFieldTypeWrap w sch t f
         => GFromSchemaFieldTypeWrap w sch t (M1 s m f) where
  fromSchemaFieldTypeW x = M1 (fromSchemaFieldTypeW x)

class GToSchemaFieldType
        (w :: * -> *) (sch :: Schema ts fs) (t :: FieldType ts) (f :: *) where
  toSchemaFieldType   :: f -> FieldValue w sch t
class GFromSchemaFieldType
        (w :: * -> *) (sch :: Schema ts fs) (t :: FieldType ts) (f :: *) where
  fromSchemaFieldType :: FieldValue w sch t -> f

class GToSchemaFieldTypeUnion
        (w :: * -> *) (sch :: Schema ts fs) (t :: [FieldType ts]) (f :: * -> *) where
  toSchemaFieldTypeUnion   :: f a -> NS (FieldValue w sch) t
class GFromSchemaFieldTypeUnion
        (w :: * -> *) (sch :: Schema ts fs) (t :: [FieldType ts]) (f :: * -> *) where
  fromSchemaFieldTypeUnion :: NS (FieldValue w sch) t -> f a

-- These instances are straightforward,
-- just turn the "real types" into their
-- schema correspondants.
instance GToSchemaFieldType w sch 'TNull () where
  toSchemaFieldType _   = FNull
instance GFromSchemaFieldType w sch 'TNull () where
  fromSchemaFieldType _ = ()
instance GToSchemaFieldType w sch ('TPrimitive t) t where
  toSchemaFieldType = FPrimitive
instance GFromSchemaFieldType w sch ('TPrimitive t) t where
  fromSchemaFieldType (FPrimitive x) = x
-- These instances "tie the loop" with the whole schema,
-- and they are the reason why we need to thread the @sch@
-- type throghout the whole implementation.
instance ToSchema w sch t v
         => GToSchemaFieldType w sch ('TSchematic t) v where
  toSchemaFieldType x = FSchematic $ toSchema x
instance FromSchema w sch t v
         => GFromSchemaFieldType w sch ('TSchematic t) v where
  fromSchemaFieldType (FSchematic x) = fromSchema x
instance GToSchemaFieldType w sch t v
         => GToSchemaFieldType w sch ('TOption t) (Maybe v) where
  toSchemaFieldType x = FOption (toSchemaFieldType <$> x)
instance GFromSchemaFieldType w sch t v
         => GFromSchemaFieldType w sch ('TOption t) (Maybe v) where
  fromSchemaFieldType (FOption x) = fromSchemaFieldType <$> x
instance GToSchemaFieldType w sch t v
         => GToSchemaFieldType w sch ('TList t) [v] where
  toSchemaFieldType x = FList (toSchemaFieldType <$> x)
instance GFromSchemaFieldType w sch t v
         => GFromSchemaFieldType w sch ('TList t) [v] where
  fromSchemaFieldType (FList x) = fromSchemaFieldType <$> x
instance (GToSchemaFieldType w sch sk hk, GToSchemaFieldType w sch sv hv,
          Ord (FieldValue w sch sk))  -- Ord is required to build a map
         => GToSchemaFieldType w sch ('TMap sk sv) (M.Map hk hv) where
  toSchemaFieldType x = FMap (M.mapKeys toSchemaFieldType (M.map toSchemaFieldType x))
instance (GFromSchemaFieldType w sch sk hk, GFromSchemaFieldType w sch sv hv, Ord hk)
         => GFromSchemaFieldType w sch ('TMap sk sv) (M.Map hk hv) where
  fromSchemaFieldType (FMap x) = M.mapKeys fromSchemaFieldType (M.map fromSchemaFieldType x)
-- This assumes that a union is represented by
-- a value of type 'NS', where types are in
-- the same order.
instance {-# OVERLAPS #-}
         AllZip (GToSchemaFieldType w sch) ts vs
         => GToSchemaFieldType w sch ('TUnion ts) (NS I vs) where
  toSchemaFieldType t = FUnion (go t)
    where go :: AllZip (GToSchemaFieldType w sch) tss vss
             => NS I vss -> NS (FieldValue w sch) tss
          go (Z (I x)) = Z (toSchemaFieldType x)
          go (S n)     = S (go n)
instance {-# OVERLAPS #-}
         AllZip (GFromSchemaFieldType w sch) ts vs
         => GFromSchemaFieldType w sch ('TUnion ts) (NS I vs) where
  fromSchemaFieldType (FUnion t) = go t
    where go :: AllZip (GFromSchemaFieldType w sch) tss vss
             => NS (FieldValue w sch) tss -> NS I vss
          go (Z x) = Z (I (fromSchemaFieldType x))
          go (S n) = S (go n)
-- But we can also use any other if it has
-- the right structure
instance {-# OVERLAPPABLE #-}
         (Generic f, GToSchemaFieldTypeUnion w sch ts (Rep f))
         => GToSchemaFieldType w sch ('TUnion ts) f where
  toSchemaFieldType x = FUnion (toSchemaFieldTypeUnion (from x))
instance {-# OVERLAPPABLE #-}
         (Generic f, GFromSchemaFieldTypeUnion w sch ts (Rep f))
         => GFromSchemaFieldType w sch ('TUnion ts) f where
  fromSchemaFieldType (FUnion x) = to (fromSchemaFieldTypeUnion x)

instance {-# OVERLAPS #-} GToSchemaFieldTypeUnion w sch '[] U1 where
  toSchemaFieldTypeUnion U1 = error "this should never happen"
instance {-# OVERLAPS #-} GFromSchemaFieldTypeUnion w sch '[] U1 where
  fromSchemaFieldTypeUnion _ = U1
instance {-# OVERLAPPABLE #-}
         TypeError ('Text "the type does not match the union")
         => GToSchemaFieldTypeUnion w sch '[] f where
  toSchemaFieldTypeUnion = error "this should never happen"
instance {-# OVERLAPPABLE #-}
         TypeError ('Text "the type does not match the union")
         => GFromSchemaFieldTypeUnion w sch '[] f where
  fromSchemaFieldTypeUnion = error "this should never happen"

instance (GToSchemaFieldTypeWrap w sch t v)
         => GToSchemaFieldTypeUnion w sch '[t] v where
  toSchemaFieldTypeUnion   x     = Z (toSchemaFieldTypeW x)
instance (GFromSchemaFieldTypeWrap w sch t v)
         => GFromSchemaFieldTypeUnion w sch '[t] v where
  fromSchemaFieldTypeUnion (Z x) = fromSchemaFieldTypeW x
  fromSchemaFieldTypeUnion (S _) = error "this should never happen"
instance (GToSchemaFieldTypeWrap w sch t v, GToSchemaFieldTypeUnion w sch ts vs)
         => GToSchemaFieldTypeUnion w sch (t ': ts) (v :+: vs) where
  toSchemaFieldTypeUnion (L1 x) = Z (toSchemaFieldTypeW x)
  toSchemaFieldTypeUnion (R1 r) = S (toSchemaFieldTypeUnion r)
instance (GFromSchemaFieldTypeWrap w sch t v, GFromSchemaFieldTypeUnion w sch ts vs)
         => GFromSchemaFieldTypeUnion w sch (t ': ts) (v :+: vs) where
  fromSchemaFieldTypeUnion (Z x) = L1 (fromSchemaFieldTypeW x)
  fromSchemaFieldTypeUnion (S r) = R1 (fromSchemaFieldTypeUnion r)
-- Weird nested instance produced by GHC
instance ( GToSchemaFieldTypeWrap w sch t1 v1
         , GToSchemaFieldTypeWrap w sch t2 v2
         , GToSchemaFieldTypeUnion w sch ts vs )
         => GToSchemaFieldTypeUnion w sch (t1 ': t2 ': ts) ((v1 :+: v2) :+: vs) where
  toSchemaFieldTypeUnion (L1 (L1 x)) = Z (toSchemaFieldTypeW x)
  toSchemaFieldTypeUnion (L1 (R1 x)) = S (Z (toSchemaFieldTypeW x))
  toSchemaFieldTypeUnion (R1 r)      = S (S (toSchemaFieldTypeUnion r))
instance ( GFromSchemaFieldTypeWrap w sch t1 v1
         , GFromSchemaFieldTypeWrap w sch t2 v2
         , GFromSchemaFieldTypeUnion w sch ts vs )
         => GFromSchemaFieldTypeUnion w sch (t1 ': t2 ': ts) ((v1 :+: v2) :+: vs) where
  fromSchemaFieldTypeUnion (Z x)     = L1 (L1 (fromSchemaFieldTypeW x))
  fromSchemaFieldTypeUnion (S (Z x)) = L1 (R1 (fromSchemaFieldTypeW x))
  fromSchemaFieldTypeUnion (S (S r)) = R1 (fromSchemaFieldTypeUnion r)


-- ---------------
-- ENUMERATIONS --
------------------

instance {-# OVERLAPPABLE #-}
         (GToSchemaEnumDecompose fmap choices f)
         => GToSchemaTypeDef w sch fmap ('DEnum name choices) f where
  toSchemaTypeDef p x = TEnum (toSchemaEnumDecomp p x)
instance {-# OVERLAPPABLE #-}
         (GFromSchemaEnumDecompose fmap choices f)
         => GFromSchemaTypeDef w sch fmap ('DEnum name choices) f where
  fromSchemaTypeDef p (TEnum x) = fromSchemaEnumDecomp p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GToSchemaTypeDef w sch fmap ('DEnum name choices) f
         => GToSchemaTypeDef w sch fmap ('DEnum name choices) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef w sch fmap ('DEnum name choices) f
         => GFromSchemaTypeDef w sch fmap ('DEnum name choices) (D1 meta f) where
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
         (GToSchemaRecord w sch fmap args f)
         => GToSchemaTypeDef w sch fmap ('DRecord name args) f where
  toSchemaTypeDef p x = TRecord (toSchemaRecord p x)
instance {-# OVERLAPPABLE #-}
         (GFromSchemaRecord w sch fmap args f)
         => GFromSchemaTypeDef w sch fmap ('DRecord name args) f where
  fromSchemaTypeDef p (TRecord x) = fromSchemaRecord p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GToSchemaTypeDef w sch fmap ('DRecord name args) f
         => GToSchemaTypeDef w sch fmap ('DRecord name args) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef w sch fmap ('DRecord name args) f
         => GFromSchemaTypeDef w sch fmap ('DRecord name args) (D1 meta f) where
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)
instance {-# OVERLAPS #-}
         GToSchemaTypeDef w sch fmap ('DRecord name args) f
         => GToSchemaTypeDef w sch fmap ('DRecord name args) (C1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
instance {-# OVERLAPS #-}
         GFromSchemaTypeDef w sch fmap ('DRecord name args) f
         => GFromSchemaTypeDef w sch fmap ('DRecord name args) (C1 meta f) where
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
-- (see 'HereLeft'  and 'HereRight' instances)
class GToSchemaRecord (w :: * -> *) (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                      (args :: [FieldDef ts fs]) (f :: * -> *) where
  toSchemaRecord :: Proxy fmap -> f a -> NP (Field w sch) args
instance GToSchemaRecord w sch fmap '[] f where
  toSchemaRecord _ _ = Nil
instance ( GToSchemaRecord w sch fmap cs f
         , GToSchemaRecordSearch w sch t f (FindSel f (MappingLeft fmap name)) )
         => GToSchemaRecord w sch fmap ('FieldDef name t ': cs) f where
  toSchemaRecord p x = this  :* toSchemaRecord p x
    where this = Field (toSchemaRecordSearch (Proxy @(FindSel f (MappingLeft fmap name))) x)

class GToSchemaRecordSearch (w :: * -> *) (sch :: Schema ts fs)
                            (t :: FieldType ts) (f :: * -> *) (wh :: Where) where
  toSchemaRecordSearch :: Proxy wh -> f a -> w (FieldValue w sch t)
instance {-# OVERLAPS #-} GToSchemaFieldType Identity sch t v
         => GToSchemaRecordSearch Identity sch t (S1 m (K1 i v)) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x)) = Identity (toSchemaFieldType x)
instance {-# OVERLAPPABLE #-} (Functor w, GToSchemaFieldType w sch t v)
         => GToSchemaRecordSearch w sch t (S1 m (K1 i (w v))) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x)) = toSchemaFieldType <$> x
instance {-# OVERLAPS #-} GToSchemaFieldType Identity sch t v
         => GToSchemaRecordSearch Identity sch t (S1 m (K1 i v) :*: rest) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x) :*: _) = Identity (toSchemaFieldType x)
instance {-# OVERLAPPABLE #-} (Functor w, GToSchemaFieldType w sch t v)
         => GToSchemaRecordSearch w sch t (S1 m (K1 i (w v)) :*: rest) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x) :*: _) = toSchemaFieldType <$> x
instance {-# OVERLAPS #-} GToSchemaFieldType Identity sch t v
         => GToSchemaRecordSearch Identity sch t ((S1 m (K1 i v) :*: other) :*: rest) 'HereLeft where
  toSchemaRecordSearch _ ((M1 (K1 x) :*: _) :*: _) = Identity (toSchemaFieldType x)
instance {-# OVERLAPPABLE #-} (Functor w, GToSchemaFieldType w sch t v)
         => GToSchemaRecordSearch w sch t ((S1 m (K1 i (w v)) :*: other) :*: rest) 'HereLeft where
  toSchemaRecordSearch _ ((M1 (K1 x) :*: _) :*: _) = toSchemaFieldType <$> x
instance {-# OVERLAPS #-} GToSchemaFieldType Identity sch t v
         => GToSchemaRecordSearch Identity sch t ((other :*: S1 m (K1 i v)) :*: rest) 'HereRight where
  toSchemaRecordSearch _ ((_ :*: M1 (K1 x)) :*: _) = Identity (toSchemaFieldType x)
instance {-# OVERLAPPABLE #-} (Functor w, GToSchemaFieldType w sch t v)
         => GToSchemaRecordSearch w sch t ((other :*: S1 m (K1 i (w v))) :*: rest) 'HereRight where
  toSchemaRecordSearch _ ((_ :*: M1 (K1 x)) :*: _) = toSchemaFieldType <$> x
instance forall sch t other rest n w.
         GToSchemaRecordSearch w sch t rest n
         => GToSchemaRecordSearch w sch t (other :*: rest) ('There n) where
  toSchemaRecordSearch _ (_ :*: xs) = toSchemaRecordSearch (Proxy @n) xs

-- 'fromSchema' for records
-- 1. decompose the sequence of products into atomic components
--    until we arrive to the selector metadata 'S1'
--    this is done by 'GFromSchemaRecord'
-- 2. figure out the field in the schema it corresponds to
--    this is done by 'MappingRight' and 'FindField'
-- 3. using that location, obtain the value of the field
--    this is done by 'GFromSchemaRecordSearch'
class GFromSchemaRecord (w :: * -> *) (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                        (args :: [FieldDef ts fs]) (f :: * -> *) where
  fromSchemaRecord :: Proxy fmap -> NP (Field w sch) args -> f a
instance {-# OVERLAPS #-}
         (GFromSchemaRecordSearch Identity sch v args (FindField args (MappingRight fmap name)))
         => GFromSchemaRecord Identity sch fmap args (S1 ('MetaSel ('Just name) u ss ds) (K1 i v)) where
  fromSchemaRecord _ x = M1 $ K1 $ runIdentity $ fromSchemaRecordSearch (Proxy @(FindField args (MappingRight fmap name))) x
instance {-# OVERLAPPABLE #-}
         (GFromSchemaRecordSearch w sch v args (FindField args (MappingRight fmap name)))
         => GFromSchemaRecord w sch fmap args (S1 ('MetaSel ('Just name) u ss ds) (K1 i (w v))) where
  fromSchemaRecord _ x = M1 $ K1 $ fromSchemaRecordSearch (Proxy @(FindField args (MappingRight fmap name))) x
instance ( GFromSchemaRecord w sch fmap args oneway
         , GFromSchemaRecord w sch fmap args oranother )
         => GFromSchemaRecord w sch fmap args (oneway :*: oranother) where
  fromSchemaRecord p x =  fromSchemaRecord p x :*: fromSchemaRecord p x
instance GFromSchemaRecord w sch fmap args U1 where
  fromSchemaRecord _ _ = U1

class GFromSchemaRecordSearch (w :: * -> *) (sch :: Schema ts fs)
                              (v :: *) (args :: [FieldDef ts fs]) (wh :: Where) where
  fromSchemaRecordSearch :: Proxy wh -> NP (Field w sch) args -> w v
instance (Functor w, GFromSchemaFieldType w sch t v)
         => GFromSchemaRecordSearch w sch v ('FieldDef name t ': rest) 'Here where
  fromSchemaRecordSearch _ (Field x :* _) = fromSchemaFieldType <$> x
instance forall sch v other rest n w.
         GFromSchemaRecordSearch w sch v rest n
         => GFromSchemaRecordSearch w sch v (other ': rest) ('There n) where
  fromSchemaRecordSearch _ (_ :* xs) = fromSchemaRecordSearch (Proxy @n) xs
