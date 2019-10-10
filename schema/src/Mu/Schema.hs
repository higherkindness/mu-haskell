{-# language DataKinds #-}
-- | Schemas for Mu microservices
module Mu.Schema (
  -- * Schema definition
  Schema, Schema', Annotation, KnownName(..)
, TypeDef(..), ChoiceDef(..), FieldDef(..), FieldType(..)
  -- ** Lookup type in schema
, (:/:)
  -- * Interpretation of schemas
, Term(..), Field(..), FieldValue(..)
, NS(..), NP(..), Proxy(..)
  -- * Conversion from types to schemas
, WithSchema(..), HasSchema(..), toSchema', fromSchema'
  -- ** Mappings between fields
, Mapping(..), Mappings, MappingRight, MappingLeft
) where

import Mu.Schema.Definition
import Mu.Schema.Interpretation
import Mu.Schema.Class