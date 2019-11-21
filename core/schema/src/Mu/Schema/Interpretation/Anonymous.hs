{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language StandaloneDeriving    #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
module Mu.Schema.Interpretation.Anonymous where

import           Data.SOP

import           Mu.Schema

data V0 sch sty where
  V0 :: (sch :/: sty ~ 'DRecord nm anns '[])
     => V0 sch sty

deriving instance Show (V0 sch sty)
deriving instance Eq   (V0 sch sty)
deriving instance Ord  (V0 sch sty)

instance (sch :/: sty ~ 'DRecord nm anns '[])
         => HasSchema sch sty (V0 sch sty) where
  toSchema V0 = TRecord Nil
  fromSchema (TRecord Nil) = V0

data V1 sch sty where
  V1 :: (sch :/: sty
           ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a) ])
     => a -> V1 sch sty

deriving instance (Show a, sch :/: sty
                             ~ 'DRecord anns nm '[ 'FieldDef f fanns ('TPrimitive a) ])
                  => Show (V1 sch sty)
deriving instance (Eq a, sch :/: sty
                          ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a) ])
                  => Eq (V1 sch sty)
deriving instance (Ord a, sch :/: sty
                            ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a) ])
                  => Ord (V1 sch sty)

instance (sch :/: sty
            ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a) ])
         => HasSchema sch sty (V1 sch sty) where
  toSchema (V1 x) = TRecord (Field (FPrimitive x) :* Nil)
  fromSchema (TRecord (Field (FPrimitive x) :* Nil)) = V1 x

data V2 sch sty where
  V2 :: (sch :/: sty
           ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a)
                               , 'FieldDef g ganns ('TPrimitive b) ])
     => a -> b -> V2 sch sty

deriving instance (Show a, Show b,
                   sch :/: sty ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a)
                                                   , 'FieldDef g ganns ('TPrimitive b) ])
                  => Show (V2 sch sty)
deriving instance (Eq a, Eq b,
                   sch :/: sty ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a)
                                                   , 'FieldDef g ganns ('TPrimitive b) ])
                  => Eq (V2 sch sty)
deriving instance (Ord a, Ord b,
                   sch :/: sty ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a)
                                                   , 'FieldDef g ganns ('TPrimitive b) ])
                  => Ord (V2 sch sty)

instance (sch :/: sty ~ 'DRecord nm anns '[ 'FieldDef f fanns ('TPrimitive a)
                                         , 'FieldDef g ganns ('TPrimitive b) ])
         => HasSchema sch sty (V2 sch sty) where
  toSchema (V2 x y) = TRecord (Field (FPrimitive x) :* Field (FPrimitive y) :* Nil)
  fromSchema (TRecord (Field (FPrimitive x) :* Field (FPrimitive y) :* Nil)) = V2 x y
