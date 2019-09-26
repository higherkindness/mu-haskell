{-# language PolyKinds, DataKinds, GADTs,
             TypeFamilies, TypeOperators,
             MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, FlexibleContexts,
             TypeApplications, ScopedTypeVariables,
             UndecidableInstances,
             DefaultSignatures #-}
-- | Conversion from types to schemas
module Mu.Schema.Class (
  WithSchema(..), HasSchema(..), fromSchema', toSchema'
, Mapping(..), Mappings, MappingRight, MappingLeft
) where

import Data.Kind
import Data.Map as M
import Data.SOP
import GHC.Generics
import GHC.TypeLits

import Mu.Schema.Definition
import Mu.Schema.Interpretation

-- | Tags a value with its schema.
--   For usage with @deriving via@.
newtype WithSchema (sch :: Schema tn fn) (sty :: tn) a = WithSchema a

-- | Defines the conversion of a type @t@ into a 'Term'
--   which follows the schema @sch@.
--   The corresponding type is given by 'SchemaType',
--   and you can give an optional mapping between the
--   field names of @t@ and that of 'SchemaType'
--   by means of 'FieldMapping'.
class HasSchema (sch :: Schema typeName fieldName) (sty :: typeName) (t :: Type)
      | sch t -> sty where
  -- | Specifies the type of the schema to map.
  -- type SchemaType sch t :: typeName
  -- | Defines custom mapping between field names in
  --   the Haskell type and the schema. Otherwise,
  --   these names must coincide.
  type FieldMapping sch sty t :: [Mapping Symbol fieldName]
  type FieldMapping sch sty t = '[]
  -- | Conversion from Haskell type to schema term.
  toSchema   :: t -> Term sch (sch :/: sty)
  -- | Conversion from schema term to Haskell type.
  fromSchema :: Term sch (sch :/: sty) -> t

  default
    toSchema :: ( Generic t
                , GSchemaTypeDef sch (FieldMapping sch sty t) (sch :/: sty) (Rep t) )
              => t -> Term sch (sch :/: sty)
  toSchema x = toSchemaTypeDef (Proxy @(FieldMapping sch sty t)) (from x)

  default
    fromSchema :: ( Generic t
                  , GSchemaTypeDef sch (FieldMapping sch sty t) (sch :/: sty) (Rep t) )
               => Term sch (sch :/: sty) -> t
  fromSchema x = to (fromSchemaTypeDef (Proxy @(FieldMapping sch sty t)) x)

-- | Conversion from Haskell type to schema term.
--   This version is intended for usage with @TypeApplications@:
--   > toSchema' @MySchema myValue
toSchema' :: forall sch t sty. HasSchema sch sty t => t -> Term sch (sch :/: sty)
toSchema' = toSchema
-- | Conversion from schema term to Haskell type.
--   This version is intended for usage with @TypeApplications@:
--   > fromSchema' @MySchema mySchemaTerm
fromSchema' :: forall sch t sty. HasSchema sch sty t => Term sch (sch :/: sty) -> t
fromSchema' = fromSchema

-- | Defines a mapping between two elements.
data Mapping  a b = a :<->: b
-- | Defines a set of mappings between elements of @a@ and @b@.
type Mappings a b = [Mapping a b]

-- | Finds the corresponding right value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
type family MappingRight (ms :: Mappings a b) (v :: a) :: b where
  MappingRight '[] (v :: Symbol) = v
  MappingRight '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingRight ((x ':<->: y) ': rest) x = y
  MappingRight (other        ': rest) x = MappingRight rest x

-- | Finds the corresponding left value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
type family MappingLeft (ms :: Mappings a b) (v :: b) :: a where
  MappingLeft '[] (v :: Symbol) = v
  MappingLeft '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingLeft ((x ':<->: y) ': rest) y = x
  MappingLeft (other        ': rest) y = MappingLeft rest y

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
  FindCon ((C1 ('MetaCons x p s) f) :+: rest) x = 'Here
  FindCon (other :+: rest) x = 'There (FindCon rest x)
  FindCon nothing          x = TypeError ('Text "Could not find constructor " ':<>: 'ShowType x)

type family FindSel (xs :: * -> *) (x :: Symbol) :: Where where
  FindSel (S1 ('MetaSel ('Just x) u ss ds) f) x = 'Here
  FindSel (S1 ('MetaSel ('Just x) u ss ds) f :*: rest) x = 'Here
  FindSel ((S1 ('MetaSel ('Just x) u ss ds) f :*: other) :*: rest) x = 'HereLeft
  FindSel ((other :*: S1 ('MetaSel ('Just x) u ss ds) f) :*: rest) x = 'HereRight
  FindSel (other :*: rest) x = 'There (FindSel rest x)
  FindSel nothing          x = TypeError ('Text "Could not find selector " ':<>: 'ShowType x)

type family FindField (xs :: [FieldDef ts fs]) (x :: ts) :: Where where
  FindField '[] x = TypeError ('Text "Could not find field " ':<>: 'ShowType x)
  FindField ('FieldDef name t ': xs) name = 'Here
  FindField (other            ': xs) name = 'There (FindField xs name)

-- Generic type definitions
class GSchemaTypeDef (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                     (t :: TypeDef ts fs) (f :: * -> *) where
  toSchemaTypeDef   :: Proxy fmap -> f a -> Term sch t
  fromSchemaTypeDef :: Proxy fmap -> Term sch t -> f a

-- ------------------
-- TYPES OF FIELDS --
-- ------------------

instance {-# OVERLAPPABLE #-}
         GSchemaFieldType sch t f
         => GSchemaTypeDef sch fmap ('DSimple t) (K1 i f) where
  toSchemaTypeDef _ (K1 x) = TSimple (toSchemaFieldType x)
  fromSchemaTypeDef _ (TSimple x) = K1 (fromSchemaFieldType x)
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GSchemaTypeDef sch fmap ('DSimple t) f 
         => GSchemaTypeDef sch fmap ('DSimple t) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)

class GSchemaFieldType (sch :: Schema ts fs) (t :: FieldType ts) (f :: *) where
  toSchemaFieldType   :: f -> FieldValue sch t
  fromSchemaFieldType :: FieldValue sch t -> f

-- These instances are straightforward,
-- just turn the "real types" into their
-- schema correspondants.
instance GSchemaFieldType sch 'TNull () where
  toSchemaFieldType _   = FNull
  fromSchemaFieldType _ = ()
instance GSchemaFieldType sch ('TPrimitive t) t where
  toSchemaFieldType x = FPrimitive x
  fromSchemaFieldType (FPrimitive x) = x
-- This instance "ties the loop" with the whole schema,
-- and it the reason why we need to thread the @sch@
-- type throghout the whole implementation.
instance HasSchema sch t v => GSchemaFieldType sch ('TSchematic t) v where
  toSchemaFieldType x = FSchematic $ toSchema x
  fromSchemaFieldType (FSchematic x) = fromSchema x
instance GSchemaFieldType sch t v => GSchemaFieldType sch ('TOption t) (Maybe v) where
  toSchemaFieldType x = FOption (toSchemaFieldType <$> x)
  fromSchemaFieldType (FOption x) = fromSchemaFieldType <$> x
instance GSchemaFieldType sch t v => GSchemaFieldType sch ('TList t) [v] where
  toSchemaFieldType x = FList (toSchemaFieldType <$> x)
  fromSchemaFieldType (FList x) = fromSchemaFieldType <$> x
instance (GSchemaFieldType sch sk hk, GSchemaFieldType sch sv hv,
          Ord (FieldValue sch sk), Ord hk)  -- Ord is required to build a map
         => GSchemaFieldType sch ('TMap sk sv) (M.Map hk hv) where
  toSchemaFieldType x = FMap (M.mapKeys toSchemaFieldType (M.map toSchemaFieldType x))
  fromSchemaFieldType (FMap x) = M.mapKeys fromSchemaFieldType (M.map fromSchemaFieldType x) 
-- This assumes that a union is represented by
-- a value of type 'NS', where types are in
-- the same order.
instance AllZip (GSchemaFieldType sch) ts vs
         => GSchemaFieldType sch ('TUnion ts) (NS I vs) where
  toSchemaFieldType t = FUnion (go t)
    where go :: AllZip (GSchemaFieldType sch) tss vss
             => NS I vss -> NS (FieldValue sch) tss
          go (Z (I x)) = Z (toSchemaFieldType x)
          go (S n)     = S (go n)
  fromSchemaFieldType (FUnion t) = go t
    where go :: AllZip (GSchemaFieldType sch) tss vss
             => NS (FieldValue sch) tss -> NS I vss 
          go (Z x) = Z (I (fromSchemaFieldType x))
          go (S n) = S (go n)

-- ---------------
-- ENUMERATIONS --
------------------

instance {-# OVERLAPPABLE #-}
         (GToSchemaEnumDecompose fmap choices f, GFromSchemaEnumDecompose fmap choices f)
         => GSchemaTypeDef sch fmap ('DEnum name choices) f where
  toSchemaTypeDef p x = TEnum (toSchemaEnumDecomp p x)
  fromSchemaTypeDef p (TEnum x) = fromSchemaEnumDecomp p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GSchemaTypeDef sch fmap ('DEnum name choices) f 
         => GSchemaTypeDef sch fmap ('DEnum name choices) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)

-- 'toSchema' for enumerations:
-- 1. recursively decompose the (:+:)s into their atomic components
--    this is done by 'GToSchemaEnumSymbol'
-- 2. for each atomic component, figure out which is the element
--    in the schema's enumeration that it corresponds to
--    this is done by 'MappingRight' and 'Find'
-- 3. from that location, build a 'Proxy' value
--    this is done by 'GToSchemaEnumProxy'
class GToSchemaEnumDecompose (fmap :: Mappings Symbol fs) (choices :: [fs]) (f :: * -> *) where
  toSchemaEnumDecomp :: Proxy fmap -> f a -> NS Proxy choices
instance forall fmap choices oneway oranother.
         (GToSchemaEnumDecompose fmap choices oneway, GToSchemaEnumDecompose fmap choices oranother)
         => GToSchemaEnumDecompose fmap choices (oneway :+: oranother) where
  toSchemaEnumDecomp p (L1 x) = toSchemaEnumDecomp p x
  toSchemaEnumDecomp p (R1 x) = toSchemaEnumDecomp p x
instance forall fmap choices c p s f.
         GToSchemaEnumProxy choices (Find choices (MappingRight fmap c))
         => GToSchemaEnumDecompose fmap choices (C1 ('MetaCons c p s) f) where
  toSchemaEnumDecomp _ _
    = toSchemaEnumProxy (Proxy @choices) (Proxy @(Find choices (MappingRight fmap c)))
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
class GFromSchemaEnumDecompose (fmap :: Mappings Symbol fs) (choices :: [fs]) (f :: * -> *) where
  fromSchemaEnumDecomp :: Proxy fmap -> NS Proxy choices -> f a
instance GFromSchemaEnumDecompose fmap '[] f where
  fromSchemaEnumDecomp _ _ = error "This should never happen"
instance forall fmap c cs f.
         (GFromSchemaEnumU1 f (FindCon f (MappingLeft fmap c)), GFromSchemaEnumDecompose fmap cs f)
         => GFromSchemaEnumDecompose fmap (c ': cs) f where
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
         (GToSchemaRecord sch fmap args f, GFromSchemaRecord sch fmap args f)
         => GSchemaTypeDef sch fmap ('DRecord name args) f where
  toSchemaTypeDef p x = TRecord (toSchemaRecord p x)
  fromSchemaTypeDef p (TRecord x) = fromSchemaRecord p x
-- This instance removes unneeded metadata from the
-- top of the type.
instance {-# OVERLAPS #-}
         GSchemaTypeDef sch fmap ('DRecord name args) f 
         => GSchemaTypeDef sch fmap ('DRecord name args) (D1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
  fromSchemaTypeDef p x = M1 (fromSchemaTypeDef p x)
instance {-# OVERLAPS #-}
         GSchemaTypeDef sch fmap ('DRecord name args) f 
         => GSchemaTypeDef sch fmap ('DRecord name args) (C1 meta f) where
  toSchemaTypeDef p (M1 x) = toSchemaTypeDef p x
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
class GToSchemaRecord (sch :: Schema ts fs) (fmap :: Mappings Symbol fs)
                      (args :: [FieldDef ts fs]) (f :: * -> *) where
  toSchemaRecord :: Proxy fmap -> f a -> NP (Field sch) args
instance GToSchemaRecord sch fmap '[] f where
  toSchemaRecord _ _ = Nil
instance forall sch fmap name t cs f.
         ( GToSchemaRecord sch fmap cs f
         , GToSchemaRecordSearch sch t f (FindSel f (MappingLeft fmap name)) )
         => GToSchemaRecord sch fmap ('FieldDef name t ': cs) f where
  toSchemaRecord p x = this  :* toSchemaRecord p x
    where this = Field (toSchemaRecordSearch (Proxy @(FindSel f (MappingLeft fmap name))) x)

class GToSchemaRecordSearch (sch :: Schema ts fs) (t :: FieldType ts) (f :: * -> *) (w :: Where) where
  toSchemaRecordSearch :: Proxy w -> f a -> FieldValue sch t
instance GSchemaFieldType sch t v 
         => GToSchemaRecordSearch sch t (S1 m (K1 i v)) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x)) = toSchemaFieldType x
instance GSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t (S1 m (K1 i v) :*: rest) 'Here where
  toSchemaRecordSearch _ (M1 (K1 x) :*: _) = toSchemaFieldType x
instance GSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((S1 m (K1 i v) :*: other) :*: rest) 'HereLeft where
  toSchemaRecordSearch _ ((M1 (K1 x) :*: _) :*: _) = toSchemaFieldType x
instance GSchemaFieldType sch t v
         => GToSchemaRecordSearch sch t ((other :*: S1 m (K1 i v)) :*: rest) 'HereRight where
  toSchemaRecordSearch _ ((_ :*: M1 (K1 x)) :*: _) = toSchemaFieldType x
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
instance GFromSchemaRecordSearch sch v args (FindField args (MappingRight fmap name))
         => GFromSchemaRecord sch fmap args (S1 ('MetaSel ('Just name) u ss ds) (K1 i v)) where
  fromSchemaRecord _ x = M1 $ K1 $ fromSchemaRecordSearch (Proxy @(FindField args (MappingRight fmap name))) x
instance (GFromSchemaRecord sch fmap args oneway, GFromSchemaRecord sch fmap args oranother)
         => GFromSchemaRecord sch fmap args (oneway :*: oranother) where
  fromSchemaRecord p x = fromSchemaRecord p x :*: fromSchemaRecord p x

class GFromSchemaRecordSearch (sch :: Schema ts fs) (v :: *) (args :: [FieldDef ts fs]) (w :: Where) where
  fromSchemaRecordSearch :: Proxy w -> NP (Field sch) args -> v
instance GSchemaFieldType sch t v => GFromSchemaRecordSearch sch v ('FieldDef name t ': rest) 'Here where
  fromSchemaRecordSearch _ (Field x :* _) = fromSchemaFieldType x
instance forall sch v other rest n.
         GFromSchemaRecordSearch sch v rest n
         => GFromSchemaRecordSearch sch v (other ': rest) ('There n) where
  fromSchemaRecordSearch _ (_ :* xs) = fromSchemaRecordSearch (Proxy @n) xs