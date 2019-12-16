{-# language DataKinds #-}
-- | Schemas for Mu microservices
module Mu.Schema (
  -- * Schema definition
  Schema, Schema'
, KnownName(..)
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
, WithSchema(..)
, FromSchema(..), fromSchema'
, ToSchema(..), toSchema'
, CustomFieldMapping(..)
  -- ** Mappings between fields
, Mapping(..), Mappings, MappingRight, MappingLeft
  -- ** Field annotations
, AnnotatedSchema, AnnotationDomain, Annotation(..)
) where

import           Mu.Schema.Annotations
import           Mu.Schema.Class
import           Mu.Schema.Definition
import           Mu.Schema.Interpretation
