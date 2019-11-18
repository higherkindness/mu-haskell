{-# language DataKinds #-}
-- | Schemas for Mu microservices
module Mu.Schema (
  -- * Schema definition
  Schema, Schema'
, Annotation, KnownName(..)
, TypeDef, TypeDefB(..)
, ChoiceDef(..)
, FieldDef, FieldDefB(..)
, FieldType, FieldTypeB(..)
  -- ** Lookup type in schema
, (:/:)
  -- * Interpretation of schemas
, Term(..), Field(..), FieldValue(..)
, NS(..), NP(..), Proxy(..)
  -- * Conversion from types to schemas
, WithSchema(..), HasSchema(..), toSchema', fromSchema'
  -- ** Mappings between fields
, Mapping(..), Mappings, MappingRight, MappingLeft
  -- ** Field annotations
, ProtoBufId, ProtoBufOneOfIds
) where

import Mu.Schema.Annotations
import Mu.Schema.Definition
import Mu.Schema.Interpretation
import Mu.Schema.Class