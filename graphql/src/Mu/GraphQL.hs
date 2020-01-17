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
  = f Wanted -> f Maybe -> m (f Maybe)

-- COMPOSABLE RESOLVERS
-- ====================
-- We can implement resolvers "piece by piece",
-- and then have a final step which puts
-- everything together.

type SchemaResolver m (sch :: Schema tn fn)
  = NP (TypeResolver m sch) sch

data TypeResolver m (sch :: Schema tn fn) (ty :: TypeDef tn fn) where
  -- | Give the resolver of a record field by field.
  RR :: ( sch :/: name ~ 'DRecord name fields )
     => NP (FieldResolver m sch name) fields
     -> TypeResolver m sch ('DRecord name fields)
  -- | Direct resolver for a record.
  DR :: FullResolver m (W Term sch t)
     -> TypeResolver m sch t
  -- | Enumerations do not get resolved directly,
  --   only as part of fields.
  ER :: TypeResolver m sch ('DEnum name choices)

data FieldResolver m (sch :: Schema tn fn) (ty :: tn) (fld :: FieldDef tn fn) where
  FR :: HasResolver sch fld
     => (Term Maybe sch (sch :/: ty) -> m (FieldValue Maybe sch fld))
     -> FieldResolver m sch ty ('FieldDef name fld)

-- | The composer

newtype W f a b w = W { unW :: f w a b }

newtype FullResolver' m sch ty
  = FullResolver' { unFullResolver' :: FullResolver m (W Term sch ty) }

fullResolver
  :: forall m sch.
     Monad m
  => SchemaResolver m sch
  -> NP (FullResolver' m sch) sch
fullResolver schr = typeResolvers schr
  where
    typeResolvers
      :: forall tys.
         NP (TypeResolver  m sch) tys
      -> NP (FullResolver' m sch) tys
    typeResolvers Nil = Nil
    typeResolvers (r :* rs) = typeResolver r :* typeResolvers rs

    typeResolver
      :: forall x.
         TypeResolver m sch x
      -> FullResolver' m sch x
    typeResolver ER
      = FullResolver' $ \_ (W (TEnum c)) -> pure $ W (TEnum c)
    typeResolver (DR directResolver)
      = FullResolver' directResolver
    typeResolver (RR fieldResolvers)
      = FullResolver' $ \(W (TRecord wanteds)) (W parent) ->
          W . TRecord <$> fullResolverFromFields parent fieldResolvers wanteds

    fullResolverFromFields
      :: forall name fields allFields.
         (sch :/: name ~ 'DRecord name allFields)
      => Term Maybe sch ('DRecord name allFields)
      -> NP (FieldResolver m sch name) fields
      -> NP (Field Wanted sch) fields
      -> m (NP (Field Maybe sch) fields)
    fullResolverFromFields _ Nil Nil = pure Nil
    fullResolverFromFields parent (_ :* fs) (Field NotWanted :* vs)
      = (Field Nothing :*) <$> fullResolverFromFields parent fs vs
    fullResolverFromFields parent (FR f :* fs) (Field WantedValue :* vs)
      = (:*) <$> (Field . Just <$> f parent) <*> fullResolverFromFields parent fs vs
    fullResolverFromFields parent (FR f :* fs) (Field (WantedSubset (FSchematic ws)) :* vs)
      = (:*) <$> (Field . Just . FSchematic <$> (f parent >>= g ws))
             <*> fullResolverFromFields parent fs vs
      where g :: forall ity.
                 HasResolver sch ('TSchematic ity)
              => Term Wanted sch (sch :/: ity)
              -> FieldValue Maybe sch ('TSchematic ity)
              -> m (Term Maybe sch (sch :/: ity))
            g wws (FSchematic t) = unW <$> fullResolverTy' @_ @sch @(sch :/: ity) schr (W wws) (W t)

type family HasResolver (sch :: Schema tn fn) (f :: FieldType tn) :: Constraint where
  HasResolver sch ('TSchematic ity) = FindResolver sch sch (sch :/: ity)
  HasResolver sch 'TNull            = ()
  HasResolver sch ('TPrimitive p)   = ()
  HasResolver sch ('TOption fld)    = HasResolver sch fld
  HasResolver sch ('TList fld)      = HasResolver sch fld

-- f Wanted -> m (f Maybe)

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
  :: forall m sch ty.
     (Monad m, FindResolver sch sch ty)
  => SchemaResolver m sch
  -> FullResolver m (W Term sch ty)
fullResolverTy' = findResolver . fullResolver

-- COMPOSABLE RESOLVERS OVER DOMAIN TYPES
-- ======================================

type SchemaResolverD m (sch :: Schema tn fn)
  = NP (TypeResolverD m sch) sch

data TypeResolverD m (sch :: Schema tn fn) (ty :: TypeDef tn fn) where
  RR_ :: ( sch :/: name ~ 'DRecord name fields )
      => NP (FieldResolverD m sch name) fields
      -> TypeResolverD m sch ('DRecord name fields)
  DR_ :: ( FromSchema Wanted sch ty wanteds
         , FromSchema Maybe  sch ty input
         , ToSchema   Maybe  sch ty output
         , sch :/: ty ~ 'DRecord ty fields )
      => (wanteds -> input -> m output)
      -> TypeResolverD m sch ('DRecord ty fields)
  ER_ :: TypeResolverD m sch ('DEnum name choice)

data FieldResolverD m (sch :: Schema tn fn) (ty :: tn) (fld :: FieldDef tn fn) where
  FR_ :: ( FromSchema Maybe sch ty input
         , ToSchemaD sch output fld, HasResolver sch fld )
      => (input -> m output)
      -> FieldResolverD m sch ty ('FieldDef name fld)

class ToSchemaD sch r t where
  toSchemaD :: r -> FieldValue Maybe sch t

instance ToSchemaD sch () 'TNull where
  toSchemaD _ = FNull
instance (ToSchema Maybe sch t r) => ToSchemaD sch r ('TSchematic t) where
  toSchemaD = FSchematic . toSchema @_ @_ @Maybe @sch @t
instance (ToSchemaD sch r t) => ToSchemaD sch (Maybe r) ('TOption t) where
  toSchemaD = FOption . fmap toSchemaD
instance (ToSchemaD sch r t) => ToSchemaD sch [r] ('TList t) where
  toSchemaD = FList . fmap toSchemaD

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
    resolveDomainT (DR_ f)   = DR $ \(W w) (W x) ->
      W . toSchema' @_ @_ @sch @Maybe
        <$> f (fromSchema' @_ @_ @sch @Wanted w)
              (fromSchema' @_ @_ @sch @Maybe x)
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
            (m :: Type -> Type) (w :: Type) (r :: Type) (s :: Type).
     ( Functor m
     , ToSchema   Wanted sch ty w
     , ToSchema   Maybe  sch ty r
     , FromSchema Maybe  sch ty s)
  => SchemaResolverD m sch -> w -> r -> m s
resolve r w x
  = fromSchema @tn @fn @Maybe @sch @ty . unW <$>
    (fullResolverTy $ resolverDomain r)
    (W $ toSchema @tn @fn @Wanted @sch @ty w)
    (W $ toSchema @tn @fn @Maybe @sch @ty x)
