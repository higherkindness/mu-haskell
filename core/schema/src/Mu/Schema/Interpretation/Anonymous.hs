{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language StandaloneDeriving    #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Anonymous terms for schema types

This module provides "anonymous terms". These
terms can be used when you don't want to write
your own Haskell type, but simply have a quick
and dirty interpretation for a schema type.
An important limitation is that anonymous terms
may only contain primitive fields.

The names of the types exposed in this module
refer to the amount of fields in the record.
Hence, use 'V0' for empty record, 'V1' for a record
with one field, 'V2' for two, and so forth.
-}
module Mu.Schema.Interpretation.Anonymous where

import           Data.Profunctor
import           Data.SOP

import           Mu.Schema

-- | Anonymous term for a record with zero fields.
data V0 w sch sty where
  V0 :: (sch :/: sty ~ 'DRecord nm '[])
     => V0 w sch sty

deriving instance Show (V0 w sch sty)
deriving instance Eq   (V0 w sch sty)
deriving instance Ord  (V0 w sch sty)

instance (sch :/: sty ~ 'DRecord nm '[])
         => ToSchema w sch sty (V0 w sch sty) where
  toSchema V0 = TRecord Nil
instance (sch :/: sty ~ 'DRecord nm '[])
         => FromSchema w sch sty (V0 w sch sty) where
  fromSchema (TRecord Nil) = V0

-- | Anonymous term for a record with one field.
data V1 w sch sty where
  V1 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ])
     => w () a -> V1 w sch sty

deriving instance (Show (w () a), sch :/: sty
                                 ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ])
                  => Show (V1 w sch sty)
deriving instance (Eq (w () a), sch :/: sty
                               ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ])
                  => Eq (V1 w sch sty)
deriving instance (Ord (w () a), sch :/: sty
                                ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ])
                  => Ord (V1 w sch sty)

instance ( Profunctor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ] )
         => ToSchema w sch sty (V1 w sch sty) where
  toSchema (V1 x) = TRecord (Field (dimap (const ()) FPrimitive x) :* Nil)
instance ( Profunctor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a) ] )
         => FromSchema w sch sty (V1 w sch sty) where
  fromSchema (TRecord (Field x :* Nil)) = V1 (dimap (const Nil) unPrimitive x)
    where unPrimitive :: FieldValue w sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l

-- | Anonymous term for a record with two fields.
data V2 w sch sty where
  V2 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                          , 'FieldDef g '[] ('TPrimitive b) ])
     => w () a -> w () b -> V2 w sch sty

deriving instance (Show (w () a), Show (w () b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                                              , 'FieldDef g '[] ('TPrimitive b) ])
                  => Show (V2 w sch sty)
deriving instance (Eq (w () a), Eq (w () b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                                              , 'FieldDef g '[] ('TPrimitive b) ])
                  => Eq (V2 w sch sty)
deriving instance (Ord (w () a), Ord (w () b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                                              , 'FieldDef g '[] ('TPrimitive b) ])
                  => Ord (V2 w sch sty)

instance ( Profunctor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                                      , 'FieldDef g '[] ('TPrimitive b) ] )
         => ToSchema w sch sty (V2 w sch sty) where
  toSchema (V2 x y) = TRecord (Field (dimap (const ()) FPrimitive x)
                    :* Field (dimap (const ()) FPrimitive y) :* Nil)
instance ( Profunctor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f '[] ('TPrimitive a)
                                      , 'FieldDef g '[] ('TPrimitive b) ] )
         => FromSchema w sch sty (V2 w sch sty) where
  fromSchema (TRecord (Field x :* Field y :* Nil))
    = V2 (dimap (const Nil) unPrimitive x) (dimap (const Nil) unPrimitive y)
    where unPrimitive :: FieldValue w sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l
