{-# language AllowAmbiguousTypes    #-}
{-# language DataKinds              #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language LambdaCase             #-}
{-# language PolyKinds              #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description : Optics-based interface for @mu-schema@ terms

This module provides instances of 'LabelOptic' to be
used in conjunction with the @optics@ package.
In particular, there are two kind of optics to access
different parts of a 'Term':

* With @#field@ you obtain the lens (that is, a getter
  and a setter) for the corresponding field in a record.
* With @#choice@ you obtain the prism for the
  desired choice in an enumeration. You can use then
  'review' to construct a term with the value.

In addition, we provide a utility function 'record' to
build a record out of the inner values. We intend the
interface to be very simple, so this function is overloaded
to take tuples of different size, with as many components
as values in the schema type.
-}
module Mu.Schema.Optics (
  -- * Build a term
  record, record1, enum
, _U0, _Next, _U1, _U2, _U3
  -- * Re-exported for convenience.
, module Optics.Core
  -- * Additional utilities.
, is
) where

import           Data.Kind
import           Data.Map
import           Data.Maybe   (isJust)
import           Data.Proxy
import           GHC.TypeLits
import           Optics.Core

import           Mu.Schema

instance (FieldLabel sch args fieldName r)
         => LabelOptic fieldName A_Lens
                       (Term sch ('DRecord name args))
                       (Term sch ('DRecord name args))
                       r r where
  labelOptic = lens (\(TRecord r) -> fieldLensGet (Proxy @fieldName) r)
                    (\(TRecord r) x -> TRecord $ fieldLensSet (Proxy @fieldName) r x)

-- | Build a Mu record 'Term' from a tuple of its values.
--
--   Note: if the record has exactly _one_ field,
--   you must use 'record1' instead.
record :: BuildRecord sch args r => r -> Term sch ('DRecord name args)
record values = TRecord $ buildR values

-- | Build a Mu record 'Term' with exactly one field.
record1 :: TypeLabel sch t1 r1 => r1 -> Term sch ('DRecord name '[ 'FieldDef x1 t1 ])
record1 value = TRecord $ Field (typeLensSet value) :* Nil

class BuildRecord (sch :: Schema Symbol Symbol)
                  (args :: [FieldDef Symbol Symbol])
                  (r :: Type) | sch args -> r where
  buildR :: r -> NP (Field sch) args

instance BuildRecord sch '[] () where
  buildR _ = Nil

instance  (TypeLabel sch t1 r1, TypeLabel sch t2 r2)
         => BuildRecord sch '[ 'FieldDef x1 t1, 'FieldDef x2 t2 ] (r1, r2) where
  buildR (v1, v2) = Field (typeLensSet v1)
                  :* Field (typeLensSet v2) :* Nil

instance (TypeLabel sch t1 r1, TypeLabel sch t2 r2, TypeLabel sch t3 r3)
         => BuildRecord sch
                        '[ 'FieldDef x1 t1, 'FieldDef x2 t2, 'FieldDef x3 t3 ] (r1, r2, r3) where
  buildR (v1, v2, v3) = Field (typeLensSet v1)
                      :* Field (typeLensSet v2)
                      :* Field (typeLensSet v3) :* Nil

class FieldLabel (sch :: Schema Symbol Symbol)
                 (args :: [FieldDef Symbol Symbol])
                 (fieldName :: Symbol) (r :: Type)
                 | sch args fieldName -> r where
  fieldLensGet :: Proxy fieldName -> NP (Field sch) args -> r
  fieldLensSet :: Proxy fieldName -> NP (Field sch) args -> r -> NP (Field sch) args

{- Removed due to FunDeps
instance TypeError ('Text "cannot find field " ':<>: 'ShowType f)
         => FieldLabel w sch '[] f t where
  fieldLensGet = error "this should never be run"
  fieldLensSet = error "this should never be run"
-}
instance {-# OVERLAPS #-} (TypeLabel sch t r)
         => FieldLabel sch ('FieldDef f t ': rest) f r where
  fieldLensGet _ (Field x :* _) = typeLensGet x
  fieldLensSet _ (_ :* r) new = Field (typeLensSet new) :* r
instance {-# OVERLAPPABLE #-} FieldLabel sch rest g t
         => FieldLabel sch (f ': rest) g t where
  fieldLensGet p (_ :* r) = fieldLensGet p r
  fieldLensSet p (x :* r) new = x :* fieldLensSet p r new

class TypeLabel (sch :: Schema Symbol Symbol) (t :: FieldType Symbol) (r :: Type)
      | sch t -> r where
  typeLensGet :: FieldValue sch t -> r
  typeLensSet :: r -> FieldValue sch t

instance TypeLabel sch ('TPrimitive t) t where
  typeLensGet (FPrimitive x) = x
  typeLensSet = FPrimitive

instance (r ~ (sch :/: t)) => TypeLabel sch ('TSchematic t) (Term sch r) where
  typeLensGet (FSchematic x) = x
  typeLensSet = FSchematic

instance (TypeLabel sch o r', r ~ Maybe r')
         => TypeLabel sch ('TOption o) r where
  typeLensGet (FOption x) = typeLensGet <$> x
  typeLensSet new = FOption (typeLensSet <$> new)

instance (TypeLabel sch o r', r ~ [r'])
         => TypeLabel sch ('TList o) r where
  typeLensGet (FList x) = typeLensGet <$> x
  typeLensSet new = FList (typeLensSet <$> new)

instance ( TypeLabel sch k k', TypeLabel sch v v'
         , r ~ Map k' v', Ord k', Ord (FieldValue sch k) )
         => TypeLabel sch ('TMap k v) r where
  typeLensGet (FMap x) = mapKeys typeLensGet (typeLensGet <$> x)
  typeLensSet new = FMap (mapKeys typeLensSet (typeLensSet <$> new))

instance (r ~ NS (FieldValue sch) choices)
         => TypeLabel sch ('TUnion choices) r where
  typeLensGet (FUnion x) = x
  typeLensSet = FUnion

-- | Build a Mu enumeration 'Term' from the name of the choice.
enum :: forall (choiceName :: Symbol) choices sch name.
        EnumLabel choices choiceName
     => Term sch ('DEnum name choices)
enum = TEnum $ enumPrismBuild (Proxy @choiceName)

-- Useful utility to check whether a value
-- matches a given enumeration choice.
--
-- > f e | e `is` #sunny = ...
-- >     | e `is` #rainy = ...
is :: Is k An_AffineFold => s -> Optic' k is s a -> Bool
is s k = isJust (preview k s)
{-# INLINE is #-}

instance (EnumLabel choices choiceName, r ~ ())
         => LabelOptic choiceName A_Prism
                       (Term sch ('DEnum name choices))
                       (Term sch ('DEnum name choices))
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

-- | Prism to access the first choice of a union.
_U0 :: forall (sch :: Schema') x xs r. TypeLabel sch x r
    => Prism' (NS (FieldValue sch) (x ': xs)) r
_U0 = prism' (Z . typeLensSet)
             (\case (Z x) -> Just $ typeLensGet x
                    (S _) -> Nothing)

-- | Prism to access all other choices of a union
--   except for the first. Intended to use be used
--   iteratively until you reach the desired choice
--   with '_U0'.
--
--   > _Next % _Next % _U0  -- access third choice
_Next :: forall (sch :: Schema') x xs.
         Prism' (NS (FieldValue sch) (x ': xs))
                (NS (FieldValue sch) xs)
_Next = prism' S
               (\case (Z _) -> Nothing
                      (S x) -> Just x)

-- | Prism to access the second choice of a union.
_U1 :: forall (sch :: Schema') a b xs r. TypeLabel sch b r
    => Prism' (NS (FieldValue sch) (a ': b ': xs)) r
_U1 = _Next % _U0

-- | Prism to access the third choice of a union.
_U2 :: forall (sch :: Schema') a b c xs r. TypeLabel sch c r
    => Prism' (NS (FieldValue sch) (a ': b ': c ': xs)) r
_U2 = _Next % _U1

-- | Prism to access the fourth choice of a union.
_U3 :: forall (sch :: Schema') a b c d xs r. TypeLabel sch d r
    => Prism' (NS (FieldValue sch) (a ': b ': c ': d ': xs)) r
_U3 = _Next % _U2
