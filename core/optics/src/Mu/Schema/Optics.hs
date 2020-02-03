{-# language DataKinds              #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language KindSignatures         #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Schema.Optics (
  module Optics.Core
) where

import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           GHC.OverloadedLabels
import           GHC.TypeLits
import           Optics.Core

import           Mu.Schema

instance {-# OVERLAPS #-}
         (FieldLabel Identity sch args fieldName r, k ~ A_Lens, is ~ NoIx)
         => IsLabel fieldName (Optic' k is (Term Identity sch ('DRecord name args)) r) where
  fromLabel = lens (\(TRecord r) -> runIdentity $ fieldLensGet (Proxy @fieldName) r)
                   (\(TRecord r) x -> TRecord $ fieldLensSet (Proxy @fieldName) r (Identity x))
instance {-# OVERLAPPABLE #-}
         (FieldLabel w sch args fieldName r, t ~ w r, k ~ A_Lens, is ~ NoIx)
         => IsLabel fieldName (Optic' k is (Term w sch ('DRecord name args)) t) where
  fromLabel = lens (\(TRecord r) -> fieldLensGet (Proxy @fieldName) r)
                   (\(TRecord r) x -> TRecord $ fieldLensSet (Proxy @fieldName) r x)

class FieldLabel (w :: Type -> Type)
                 (sch :: Schema Symbol Symbol)
                 (args :: [FieldDef Symbol Symbol])
                 (fieldName :: Symbol) (r :: Type)
                 | w sch args fieldName -> r where
  fieldLensGet :: Proxy fieldName -> NP (Field w sch) args -> w r
  fieldLensSet :: Proxy fieldName -> NP (Field w sch) args -> w r -> NP (Field w sch) args

{- Removed due to FunDeps
instance TypeError ('Text "cannot find field " ':<>: 'ShowType f)
         => FieldLabel w sch '[] f t where
  fieldLensGet = error "this should never be run"
  fieldLensSet = error "this should never be run"
-}
instance {-# OVERLAPS #-} (Applicative w, TypeLabel w sch t r)
         => FieldLabel w sch ('FieldDef f t ': rest) f r where
  fieldLensGet _ (Field x :* _) = typeLensGet <$> x
  fieldLensSet _ (Field old :* r) new = Field (typeLensSet <$> old <*> new) :* r
instance {-# OVERLAPPABLE #-} FieldLabel w sch rest g t
         => FieldLabel w sch (f ': rest) g t where
  fieldLensGet p (_ :* r) = fieldLensGet p r
  fieldLensSet p (x :* r) new = x :* fieldLensSet p r new

class TypeLabel w (sch :: Schema Symbol Symbol) (t :: FieldType Symbol) (r :: Type)
      | w sch t -> r where
  typeLensGet :: FieldValue w sch t -> r
  typeLensSet :: FieldValue w sch t -> r -> FieldValue w sch t

instance TypeLabel w sch ('TPrimitive t) t where
  typeLensGet (FPrimitive x) = x
  typeLensSet _ = FPrimitive
instance (r ~ (sch :/: t)) => TypeLabel w sch ('TSchematic t) (Term w sch r) where
  typeLensGet (FSchematic x) = x
  typeLensSet _ = FSchematic


