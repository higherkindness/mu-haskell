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
