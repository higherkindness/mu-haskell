{-# language PolyKinds, DataKinds, TypeFamilies,
             ScopedTypeVariables, MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             TypeOperators, UndecidableInstances,
             TypeApplications, AllowAmbiguousTypes #-}
module Mu.Schema.Registry (
  -- * Registry of schemas
  Registry, fromRegistry
  -- * Terms without an associated schema
, SLess.Term(..), SLess.Field(..), SLess.FieldValue(..)
) where

import Data.Proxy
import Data.Kind
import Control.Applicative
import GHC.TypeLits

import Mu.Schema.Definition
import Mu.Schema.Class
import qualified Mu.Schema.Interpretation.Schemaless as SLess

type Registry = Mappings Nat Schema'

fromRegistry :: forall r t. 
                FromRegistry r t
             => SLess.Term -> Maybe t
fromRegistry = fromRegistry' (Proxy @r)

class FromRegistry (ms :: Registry) (t :: Type) where
  fromRegistry' :: Proxy ms -> SLess.Term -> Maybe t

instance FromRegistry '[] t where
  fromRegistry' _ _ = Nothing
instance (HasSchema s sty t, SLess.CheckSchema s (s :/: sty), FromRegistry ms t)
         => FromRegistry ( (n ':-> s) ': ms) t where
  fromRegistry' _ t = SLess.fromSchemalessTerm @s t <|> fromRegistry' (Proxy @ms) t