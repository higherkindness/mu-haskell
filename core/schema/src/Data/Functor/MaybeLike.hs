{-|
Description : Type constructors which can be turned into 'Maybe'.

Type constructors which can be turned into 'Maybe'.
-}
module Data.Functor.MaybeLike where

import Data.Functor.Identity

-- | This class may be defined in two ways:
--
--   * Type constructors which can be turned into 'Maybe' generically.
--   * Type constructors which admit a natural transformation to 'Maybe'.
--
--   We expect the following rules to hold for those
--   instances of 'MaybeLike' which are also 'Control.Applicative.Alternative':
--
--   * @likeMaybe empty = empty = Nothing@
--   * @likeMaybe (x <|> y) = likeMaybe x <|> likeMaybe y@
class MaybeLike f where
  likeMaybe :: f a -> Maybe a

instance MaybeLike Identity where
  likeMaybe = Just . runIdentity
instance MaybeLike Maybe where
  likeMaybe = id
instance MaybeLike (Either a) where
  likeMaybe (Left  _) = Nothing
  likeMaybe (Right y) = Just y
