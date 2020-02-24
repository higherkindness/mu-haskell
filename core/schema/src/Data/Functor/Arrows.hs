{-# language DataKinds                  #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language InstanceSigs               #-}
{-# language PatternSynonyms            #-}
{-# language PolyKinds                  #-}
{-# language TypeOperators              #-}
module Data.Functor.Arrows where

import           Data.Functor.Identity
import           Data.SOP.NP

data Functions xs ys where
  F0    :: Functions '[] '[]
  (:-*) :: (x -> y) -> Functions xs ys -> Functions (x ': xs) (y ': ys)

class Profunctor1 (f :: [*] -> * -> *) where
  dimap1 :: Functions xs ys -> (a -> b) -> f xs a -> f ys b

newtype Effectful m as b
  = Effectful { unEffectful :: NP Identity as -> m b }

instance Functor m => Functor (Effectful m a) where
  fmap :: (b -> c) -> Effectful m a b -> Effectful m a c
  fmap f = Effectful . fmap (fmap f) . unEffectful
instance Applicative m => Applicative (Effectful m a) where
  pure :: b -> Effectful m a b
  pure x = Effectful $ pure (pure x)
  (<*>) :: Effectful m a (b -> c) -> Effectful m a b -> Effectful m a c
  Effectful f <*> Effectful x = Effectful (\t -> f t <*> x t)

newtype DropArgs m a b
  = DropArgs { unDropArgs :: m b }
  deriving (Functor, Applicative, Monad, Foldable)
instance Traversable m => Traversable (DropArgs m a) where
  traverse f (DropArgs x) = DropArgs <$> traverse f x
