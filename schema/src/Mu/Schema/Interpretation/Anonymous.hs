{-# language PolyKinds, DataKinds, GADTs,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances,
             StandaloneDeriving #-}
module Mu.Schema.Interpretation.Anonymous where

import Data.SOP

import Mu.Schema

data V0 sch sty where
  V0 :: (sch :/: sty ~ 'DRecord nm '[])
     => V0 sch sty

deriving instance Show (V0 sch sty)
deriving instance Eq   (V0 sch sty)
deriving instance Ord  (V0 sch sty)

instance (sch :/: sty ~ 'DRecord nm '[])
         => HasSchema sch sty (V0 sch sty) where
  toSchema V0 = TRecord Nil
  fromSchema (TRecord Nil) = V0

data V1 sch sty where
  V1 :: (sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
     => a -> V1 sch sty

deriving instance (Show a, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Show (V1 sch sty)
deriving instance (Eq a, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Eq (V1 sch sty)
deriving instance (Ord a, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Ord (V1 sch sty)

instance (sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
         => HasSchema sch sty (V1 sch sty) where
  toSchema (V1 x) = TRecord (Field (FPrimitive x) :* Nil)
  fromSchema (TRecord (Field (FPrimitive x) :* Nil)) = V1 x

data V2 sch sty where
  V2 :: (sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                    , 'FieldDef g ('TPrimitive b) ])
     => a -> b -> V2 sch sty

deriving instance (Show a, Show b, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                                              , 'FieldDef g ('TPrimitive b) ])
                  => Show (V2 sch sty)
deriving instance (Eq a, Eq b, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                                          , 'FieldDef g ('TPrimitive b) ])
                  => Eq (V2 sch sty)
deriving instance (Ord a, Ord b, sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                                            , 'FieldDef g ('TPrimitive b) ])
                  => Ord (V2 sch sty)

instance (sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                     , 'FieldDef g ('TPrimitive b) ])
         => HasSchema sch sty (V2 sch sty) where
  toSchema (V2 x y) = TRecord (Field (FPrimitive x) :* Field (FPrimitive y) :* Nil)
  fromSchema (TRecord (Field (FPrimitive x) :* Field (FPrimitive y) :* Nil)) = V2 x y