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
{-|
Description : Registry of schemas

A registry of schemas saves the different schemas
supported by an application. Since messages and
protocols may evolve, it's useful to keep an updated
view of the different shapes of data we can handle.

Examples of registries are found in
<https://docs.confluent.io/current/schema-registry/index.html Kafka>
and <https://github.com/higherkindness/compendium Compendium>.
-}
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

-- | A 'Registry' is defined as a map from
--   version numbers to type-level schemas.
--
--   /Implementation note/: you __must__
--   write newer schemas at the head of the
--   'Registry'. Otherwise, older schemas
--   take precedence during conversion.
type Registry = Mappings Nat Schema'

-- | Converts a schemaless term into a value
--   by checking all the possible schemas in
--   a 'Registry'.
--
--   /Implementation note/: schemas are checked
--   __in the same order__ in which they appear
--   in the 'Registry' definition.
fromRegistry :: forall r t. FromRegistry r t
             => SLess.Term -> Maybe t
fromRegistry = fromRegistry' (Proxy @r)

class FromRegistry (ms :: Registry) (t :: Type) where
  fromRegistry' :: Proxy ms -> SLess.Term -> Maybe t

instance FromRegistry '[] t where
  fromRegistry' _ _ = Nothing
instance ( Traversable w, FromSchema s sty t
         , SLess.CheckSchema s (s :/: sty), FromRegistry ms t )
         => FromRegistry ((n ':-> s) ': ms) t where
  fromRegistry' _ t = SLess.fromSchemalessTerm @s t <|> fromRegistry' (Proxy @ms) t
