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
, record
) where

import           Data.Functor.Identity
import           Data.Kind
import           Data.Map
import           Data.Proxy
import           GHC.TypeLits
import           Optics.Core

import           Mu.Schema

instance {-# OVERLAPS #-}
         (FieldLabel Identity sch args fieldName r)
         => LabelOptic fieldName A_Lens
                       (Term Identity sch ('DRecord name args))
                       (Term Identity sch ('DRecord name args))
                       r r where
  labelOptic = lens (\(TRecord r) -> runIdentity $ fieldLensGet (Proxy @fieldName) r)
                    (\(TRecord r) x -> TRecord $ fieldLensSet (Proxy @fieldName) r (Identity x))
instance {-# OVERLAPPABLE #-}
         (FieldLabel w sch args fieldName r, t ~ w r)
         => LabelOptic fieldName A_Lens
                       (Term w sch ('DRecord name args))
                       (Term w sch ('DRecord name args))
                       t t where
  labelOptic = lens (\(TRecord r) -> fieldLensGet (Proxy @fieldName) r)
                    (\(TRecord r) x -> TRecord $ fieldLensSet (Proxy @fieldName) r x)

record :: BuildRecord w sch args r => r -> Term w sch ('DRecord name args)
record values = TRecord $ buildR values

class BuildRecord (w :: Type -> Type)
                  (sch :: Schema Symbol Symbol)
                  (args :: [FieldDef Symbol Symbol])
                  (r :: Type) | w sch args -> r where
  buildR :: r -> NP (Field w sch) args
instance BuildRecord w sch '[] () where
  buildR _ = Nil
instance (Functor w, TypeLabel w sch t1 r1)
         => BuildRecord w sch '[ 'FieldDef x1 t1 ] (w r1) where
  buildR v = Field (typeLensSet <$> v) :* Nil
instance (Functor w, TypeLabel w sch t1 r1, TypeLabel w sch t2 r2)
         => BuildRecord w sch '[ 'FieldDef x1 t1, 'FieldDef x2 t2 ] (w r1, w r2) where
  buildR (v1, v2) = Field (typeLensSet <$> v1)
                  :* Field (typeLensSet <$> v2) :* Nil
instance (Functor w, TypeLabel w sch t1 r1, TypeLabel w sch t2 r2, TypeLabel w sch t3 r3)
         => BuildRecord w sch
                        '[ 'FieldDef x1 t1, 'FieldDef x2 t2, 'FieldDef x3 t3 ]
                        (w r1, w r2, w r3) where
  buildR (v1, v2, v3) = Field (typeLensSet <$> v1)
                      :* Field (typeLensSet <$> v2)
                      :* Field (typeLensSet <$> v3) :* Nil


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
instance {-# OVERLAPS #-} (Functor w, TypeLabel w sch t r)
         => FieldLabel w sch ('FieldDef f t ': rest) f r where
  fieldLensGet _ (Field x :* _) = typeLensGet <$> x
  fieldLensSet _ (_ :* r) new = Field (typeLensSet <$> new) :* r
instance {-# OVERLAPPABLE #-} FieldLabel w sch rest g t
         => FieldLabel w sch (f ': rest) g t where
  fieldLensGet p (_ :* r) = fieldLensGet p r
  fieldLensSet p (x :* r) new = x :* fieldLensSet p r new

class TypeLabel w (sch :: Schema Symbol Symbol) (t :: FieldType Symbol) (r :: Type)
      | w sch t -> r where
  typeLensGet :: FieldValue w sch t -> r
  typeLensSet :: r -> FieldValue w sch t

instance TypeLabel w sch ('TPrimitive t) t where
  typeLensGet (FPrimitive x) = x
  typeLensSet = FPrimitive
instance (r ~ (sch :/: t)) => TypeLabel w sch ('TSchematic t) (Term w sch r) where
  typeLensGet (FSchematic x) = x
  typeLensSet = FSchematic
instance (TypeLabel w sch o r', r ~ Maybe r')
         => TypeLabel w sch ('TOption o) r where
  typeLensGet (FOption x) = typeLensGet <$> x
  typeLensSet new = FOption (typeLensSet <$> new)
instance (TypeLabel w sch o r', r ~ [r'])
         => TypeLabel w sch ('TList o) r where
  typeLensGet (FList x) = typeLensGet <$> x
  typeLensSet new = FList (typeLensSet <$> new)
instance ( TypeLabel w sch k k', TypeLabel w sch v v'
         , r ~ Map k' v', Ord k', Ord (FieldValue w sch k) )
         => TypeLabel w sch ('TMap k v) r where
  typeLensGet (FMap x) = mapKeys typeLensGet (typeLensGet <$> x)
  typeLensSet new = FMap (mapKeys typeLensSet (typeLensSet <$> new))

instance (EnumLabel choices choiceName, r ~ ())
         => LabelOptic choiceName A_Prism
                       (Term w sch ('DEnum name choices))
                       (Term w sch ('DEnum name choices))
                       r r where
  labelOptic = prism' (\_ -> TEnum $ enumPrismBuild (Proxy @choiceName))
                     (\(TEnum r) -> enumPrismMatch (Proxy @choiceName) r)

class EnumLabel (choices :: [ChoiceDef Symbol])
                (choiceName :: Symbol) where
  enumPrismBuild :: Proxy choiceName -> NS Proxy choices
  enumPrismMatch :: Proxy choiceName -> NS Proxy choices -> Maybe ()

instance TypeError ('Text "cannot find choice " ':<>: 'ShowType c)
         => EnumLabel '[] c where
  enumPrismBuild = error "this should never be run"
  enumPrismMatch = error "this should never be run"
instance {-# OVERLAPS #-} EnumLabel ('ChoiceDef c ': rest) c where
  enumPrismBuild _ = Z Proxy
  enumPrismMatch _ (Z _) = Just ()
  enumPrismMatch _ _     = Nothing
instance {-# OVERLAPPABLE #-} EnumLabel rest c
         => EnumLabel (d ': rest) c where
  enumPrismBuild p = S (enumPrismBuild p)
  enumPrismMatch _ (Z _) = Nothing
  enumPrismMatch p (S x) = enumPrismMatch p x
