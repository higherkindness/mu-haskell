{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
module Mu.Schema.Registry (
  -- * Registry of schemas
  Registry, fromRegistry
  -- * Terms without an associated schema
, SLess.Term(..), SLess.Field(..), SLess.FieldValue(..)
) where

import           Control.Applicative
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits

import           Mu.Schema.Class
import           Mu.Schema.Definition
import qualified Mu.Schema.Interpretation.Schemaless as SLess

type Registry = Mappings Nat Schema'

fromRegistry :: forall r t w. FromRegistry w r t
             => SLess.Term w -> Maybe t
fromRegistry = fromRegistry' (Proxy @r)

class FromRegistry (w :: * -> *) (ms :: Registry) (t :: Type) where
  fromRegistry' :: Proxy ms -> SLess.Term w -> Maybe t

instance FromRegistry w '[] t where
  fromRegistry' _ _ = Nothing
instance ( Traversable w, HasSchema w s sty t
         , SLess.CheckSchema s (s :/: sty), FromRegistry w ms t )
         => FromRegistry w ((n ':-> s) ': ms) t where
  fromRegistry' _ t = SLess.fromSchemalessTerm @s @w t <|> fromRegistry' (Proxy @ms) t
