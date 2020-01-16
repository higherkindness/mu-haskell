{-# language DataKinds             #-}
{-# language DeriveFunctor         #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}

module Mu.GraphQL where

import           Data.Kind
import           Data.SOP.NP
import           GHC.TypeLits
import           Mu.Schema

-- | Defines whether we should get each field.
--   Invariant: if we have a recursive appearance
--   of the type, 'WantedSubset' must be used.
--   We could fix if with a fixed-point view
--   (using 'Fix'), but it's going too far.
data Wanted a
  = NotWanted
  | WantedValue
  | WantedSubset a
  deriving (Show, Eq, Ord, Functor)

-- | This is the resolver which takes the entire
--   query and returns the entire set of results.
type FullResolver m f
  = f Wanted -> m (f Maybe)

-- COMPOSABLE RESOLVERS
-- ====================
-- We can implement resolvers "piece by piece",
-- and then have a final step which puts
-- everything together.

type SchemaResolver m (sch :: Schema tn fn)
  = NP (TypeResolver m sch) sch

data TypeResolver m (sch :: Schema tn fn) (ty :: TypeDef tn fn) where
  -- | Give the resolver of a record field by field.
  RR :: NP (FieldResolver m sch name) fields
     -> TypeResolver m sch ('DRecord name fields)
  -- | Direct resolver for a record.
  DR :: FullResolver m (W Term sch t)
     -> TypeResolver m sch t
  -- | Enumerations do not get resolved directly,
  --   only as part of fields.
  ER :: TypeResolver m sch ('DEnum name choices)

data FieldResolver m (sch :: Schema tn fn) (ty :: tn) (fld :: FieldDef tn fn) where
  FR :: (Term Maybe sch (sch :/: ty) -> m (ConstructFieldType sch fld))
     -> FieldResolver m sch ty ('FieldDef name fld)

type family ConstructFieldType (sch :: Schema tn fn) (fld :: FieldType tn) :: Type where
  ConstructFieldType sch 'TNull = ()
  ConstructFieldType sch ('TPrimitive p) = p
  ConstructFieldType sch ('TSchematic other) = Term Maybe sch (sch :/: other)
  ConstructFieldType sch ('TOption fld) = Maybe (ConstructFieldType sch fld)
  ConstructFieldType sch ('TList fld) = [ConstructFieldType sch fld]

-- | The composer

newtype W f a b w = W { unW :: f w a b }

-- FOR THE NEXT THING YOU HAVE TWO POSSIBIILITIES

-- Possibility (1)
fullResolverTy
  :: SchemaResolver m sch
  -> FullResolver m (W Term sch ty)
fullResolverTy = undefined -- TODO:

-- Possibility (2), I think this is easier
newtype FullResolver' m sch ty
  = FullResolver' { unFullResolver' :: FullResolver m (W Term sch ty) }

fullResolver
  :: forall m sch tys.
     Applicative m
  => NP (TypeResolver  m sch) tys
  -> NP (FullResolver' m sch) tys
fullResolver Nil = Nil
fullResolver (r :* rs) = typeResolver r :* fullResolver rs
  where
    typeResolver
      :: forall x.
         TypeResolver m sch x
      -> FullResolver' m sch x
    typeResolver ER                  = FullResolver' (\(W (TEnum c)) -> pure $ W (TEnum c))
    typeResolver (DR directResolver) = FullResolver' directResolver
    typeResolver (RR fieldResolvers) = fullResolverFromFields fieldResolvers
    fullResolverFromFields
      :: forall name fields x.
         NP (FieldResolver m sch name) fields
      -> FullResolver' m sch x
    fullResolverFromFields Nil       = undefined -- TODO:
    fullResolverFromFields (f :* fs) = undefined -- TODO:

class FindResolver (sch :: Schema tn fn) (iter :: Schema tn fn) (ty :: TypeDef tn fn) where
  findResolver :: NP (FullResolver' m sch) iter -> FullResolver m (W Term sch ty)

instance TypeError ('Text "cannot find resolver for " ':<>: 'ShowType ty)
  => FindResolver sch '[] ty where
  findResolver = error "this should never be called"

instance {-# OVERLAPS #-} FindResolver sch (ty ': tys) ty where
  findResolver (r :* _) = unFullResolver' r

instance {-# OVERLAPPABLE #-} FindResolver sch rest ty => FindResolver sch (other ': rest) ty where
  findResolver (_ :* rs) = findResolver rs

fullResolverTy'
  :: (Applicative m, FindResolver sch sch ty)
  => SchemaResolver m sch
  -> FullResolver m (W Term sch ty)
fullResolverTy' = findResolver . fullResolver

-- COMPOSABLE RESOLVERS OVER DOMAIN TYPES
-- ======================================

type SchemaResolverD m (sch :: Schema tn fn)
  = NP (TypeResolverD m sch) sch

data TypeResolverD m (sch :: Schema tn fn) (ty :: TypeDef tn fn) where
  RR_ :: NP (FieldResolverD m sch name) fields
      -> TypeResolverD m sch ('DRecord name fields)
  DR_ :: ( FromSchema Wanted sch ty input
         , ToSchema   Maybe  sch ty output
         , sch :/: ty ~ 'DRecord ty fields)
      => (input -> m output)
      -> TypeResolverD m sch ('DRecord ty fields)
  ER_ :: TypeResolverD m sch ('DEnum name choice)

data FieldResolverD m (sch :: Schema tn fn) (ty :: tn) (fld :: FieldDef tn fn) where
  FR_ :: ( FromSchema Maybe sch ty input
         , ToSchemaD output (ConstructFieldType sch fld) )
      => (input -> m output)
      -> FieldResolverD m sch ty ('FieldDef name fld)

class ToSchemaD r term where
  toSchemaD :: r -> term

instance {-# OVERLAPPABLE #-} ToSchemaD p p where
  toSchemaD = id

instance {-# OVERLAPS #-} (ToSchema Maybe sch ty r, sch :/: ty ~ t) => ToSchemaD r (Term Maybe sch t) where
  toSchemaD = toSchema @_ @_ @Maybe @sch @ty

instance {-# OVERLAPS #-} (ToSchemaD r term) => ToSchemaD (Maybe r) (Maybe term) where
  toSchemaD = fmap toSchemaD

instance {-# OVERLAPS #-} (ToSchemaD r term) => ToSchemaD [r] [term] where
  toSchemaD = fmap toSchemaD

resolverDomain
  :: forall m sch tys.
     Functor m
  => NP (TypeResolverD m sch) tys
  -> NP (TypeResolver  m sch) tys
resolverDomain Nil = Nil
resolverDomain (r :* rs) = resolveDomainT r :* resolverDomain rs
  where
    resolveDomainT
      :: forall ty.
         TypeResolverD m sch ty
      -> TypeResolver m sch ty
    resolveDomainT ER_       = ER
    resolveDomainT (DR_ f)   = DR $
      (W . toSchema' @_ @_ @sch @Maybe <$>) . f . fromSchema' @_ @_ @sch @Wanted . unW
    resolveDomainT (RR_ frs) = RR $ resolveDomainFs frs
    resolveDomainFs
      :: forall name flds.
         NP (FieldResolverD m sch name) flds
      -> NP (FieldResolver  m sch name) flds
    resolveDomainFs Nil       = Nil
    resolveDomainFs (f :* fs) = resolveField f :* resolveDomainFs fs
    resolveField
      :: forall name fld.
         FieldResolverD m sch name fld
      -> FieldResolver  m sch name fld
    resolveField (FR_ f) = FR $
      (toSchemaD <$>) . f . fromSchema' @_ @_ @sch @Maybe

resolve
  :: forall tn fn (sch :: Schema tn fn) (ty :: tn)
            (m :: Type -> Type) (r :: Type) (s :: Type).
     (Functor m, ToSchema Wanted sch ty r, FromSchema Maybe sch ty s)
  => SchemaResolverD m sch -> r -> m s
resolve r x
  = fromSchema @tn @fn @Maybe @sch @ty . unW <$>
    (fullResolverTy $ resolverDomain r)
    (W $ toSchema @tn @fn @Wanted @sch @ty x)
