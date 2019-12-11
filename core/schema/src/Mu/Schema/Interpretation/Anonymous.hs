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

data V0 w sch sty where
  V0 :: (sch :/: sty ~ 'DRecord nm '[])
     => V0 w sch sty

deriving instance Show (V0 w sch sty)
deriving instance Eq   (V0 w sch sty)
deriving instance Ord  (V0 w sch sty)

instance (sch :/: sty ~ 'DRecord nm '[])
         => HasSchema w sch sty (V0 w sch sty) where
  toSchema V0 = TRecord Nil
  fromSchema (TRecord Nil) = V0

data V1 w sch sty where
  V1 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
     => w a -> V1 w sch sty

deriving instance (Show (w a), sch :/: sty
                                 ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Show (V1 w sch sty)
deriving instance (Eq (w a), sch :/: sty
                               ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Eq (V1 w sch sty)
deriving instance (Ord (w a), sch :/: sty
                                ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ])
                  => Ord (V1 w sch sty)

instance ( Functor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a) ] )
         => HasSchema w sch sty (V1 w sch sty) where
  toSchema (V1 x) = TRecord (Field (FPrimitive <$> x) :* Nil)
  fromSchema (TRecord (Field x :* Nil)) = V1 (unPrimitive <$> x)
    where unPrimitive :: FieldValue w sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l

data V2 w sch sty where
  V2 :: (sch :/: sty
           ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                          , 'FieldDef g ('TPrimitive b) ])
     => w a -> w b -> V2 w sch sty

deriving instance (Show (w a), Show (w b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Show (V2 w sch sty)
deriving instance (Eq (w a), Eq (w b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Eq (V2 w sch sty)
deriving instance (Ord (w a), Ord (w b),
                   sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                              , 'FieldDef g ('TPrimitive b) ])
                  => Ord (V2 w sch sty)

instance ( Functor w
         , sch :/: sty ~ 'DRecord nm '[ 'FieldDef f ('TPrimitive a)
                                      , 'FieldDef g ('TPrimitive b) ] )
         => HasSchema w sch sty (V2 w sch sty) where
  toSchema (V2 x y) = TRecord (Field (FPrimitive <$> x) :* Field (FPrimitive <$> y) :* Nil)
  fromSchema (TRecord (Field x :* Field y :* Nil)) = V2 (unPrimitive <$> x) (unPrimitive <$> y)
    where unPrimitive :: FieldValue w sch ('TPrimitive t) -> t
          unPrimitive (FPrimitive l) = l
