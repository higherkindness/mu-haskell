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

import           Data.SOP

import           Mu.Schema

-- | Anonymous term for a record with zero fields.
data V0 sch sty where
  V0 :: (sch :/: sty ~ 'DRecord nm '[])
     => V0 sch sty

deriving instance Show (V0 sch sty)
deriving instance Eq   (V0 sch sty)
deriving instance Ord  (V0 sch sty)

instance (sch :/: sty ~ 'DRecord nm '[])
         => ToSchema sch sty (V0 sch sty) where
  toSchema V0 = TRecord Nil
instance (sch :/: sty ~ 'DRecord nm '[])
         => FromSchema sch sty (V0 sch sty) where
  fromSchema (TRecord Nil) = V0

-- | Anonymous term for a record with one field.
data V1 sch sty where
  V1 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
     => a -> V1 sch sty

deriving instance ( Show a
                  , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
                  => Show (V1 sch sty)
deriving instance ( Eq a
                  , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
                  => Eq (V1 sch sty)
deriving instance ( Ord a
                  , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
                  => Ord (V1 sch sty)

instance ( sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
         => ToSchema sch sty (V1 sch sty) where
  toSchema (V1 x) = TRecord (Field (FPrimitive x) :* Nil)
instance ( sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
         => FromSchema sch sty (V1 sch sty) where
  fromSchema (TRecord (Field x :* Nil)) = V1 (unPrimitive x)
    where unPrimitive :: FieldValue sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l

-- | Anonymous term for a record with two fields.
data V2 sch sty where
  V2 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                          , 'FieldDef g ('TPrimitive b) ])
     => a -> b -> V2 sch sty

deriving instance (Show a, Show b,
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Show (V2 sch sty)
deriving instance (Eq a, Eq b,
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Eq (V2 sch sty)
deriving instance (Ord a, Ord b,
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Ord (V2 sch sty)

instance ( sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                      , 'FieldDef g ('TPrimitive b) ] )
         => ToSchema sch sty (V2 sch sty) where
  toSchema (V2 x y) = TRecord (Field (FPrimitive x) :* Field (FPrimitive y) :* Nil)
instance ( sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                      , 'FieldDef g ('TPrimitive b) ] )
         => FromSchema sch sty (V2 sch sty) where
  fromSchema (TRecord (Field x :* Field y :* Nil)) = V2 (unPrimitive x) (unPrimitive y)
    where unPrimitive :: FieldValue sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l
