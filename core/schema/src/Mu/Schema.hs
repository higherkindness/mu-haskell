{-# language DataKinds #-}
{-|
Description : Schemas for Mu microservices

Definition and interpretation of schemas in
the vein of Avro, Protocol Buffers, or JSON Schema.

Each 'Schema' is made out of types (which in turn
be records or enumerations). A value which obbeys
such a schema is called a 'Term'. Conversion between
Haskell types and schema types is mediated by the
type classes 'ToSchema' and 'FromSchema'.
-}
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
, WithSchema(..), unWithSchema
, FromSchema(..), fromSchema'
, ToSchema(..), toSchema'
, CustomFieldMapping(..)
, Underlying(..), UnderlyingConversion(..)
  -- ** Mappings between fields
, Mapping(..), Mappings, MappingRight, MappingLeft
  -- ** Field annotations
, AnnotatedSchema, AnnotationDomain, Annotation(..)
) where

import           Mu.Schema.Annotations
import           Mu.Schema.Class
import           Mu.Schema.Definition
import           Mu.Schema.Interpretation
