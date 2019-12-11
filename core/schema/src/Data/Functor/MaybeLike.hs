module Data.Functor.MaybeLike where

import Data.Functor.Identity

class MaybeLike f where
  likeMaybe :: f a -> Maybe a

instance MaybeLike Identity where
  likeMaybe = Just . runIdentity
instance MaybeLike Maybe where
  likeMaybe = id
instance MaybeLike (Either a) where
  likeMaybe (Left  _) = Nothing
  likeMaybe (Right y) = Just y
