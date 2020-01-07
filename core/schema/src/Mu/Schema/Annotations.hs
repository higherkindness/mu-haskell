{-# language DataKinds            #-}
{-# language GADTs                #-}
{-# language PolyKinds            #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
{-|
Description : Protocol-defined annotations.

Libraries can define custom annotations to
indicate additional information not found
in the 'Schema' itself. For example, Protocol
Buffers requires a numerical identifier for
each field in a record.
-}
module Mu.Schema.Annotations (
  -- * Annotate a schema
  Annotation(..)
, AnnotatedSchema
, AnnotationDomain
  -- * Find annotations for an element
, GetSchemaAnnotation
, GetTypeAnnotation
, GetFieldAnnotation
) where

import           Data.Kind
import           GHC.TypeLits

import           Mu.Schema.Definition

-- | Each annotation belongs to a domain.
type AnnotationDomain = Type

-- | Annotations proper.
data Annotation domain typeName fieldName where
  -- | Annotation over the whole schema.
  AnnSchema :: domain
            -> Annotation domain typeName fieldName
  -- | Annotation over a type in the schema.
  AnnType   :: typeName -> domain
            -> Annotation domain typeName fieldName
  -- | Annotation over a field in a record
  --   or a choice in an enumeration.
  AnnField  :: typeName -> fieldName -> domain
            -> Annotation domain typeName fieldName

-- | This type family links each schema to
--   its corresponding annotations from one domain.
type family AnnotatedSchema domain (sch :: Schema typeName fieldName)
  :: [Annotation domain typeName fieldName]

-- | Find the annotation over the schema in the given set.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetSchemaAnnotation (anns :: [Annotation domain t f]) :: domain where
  GetSchemaAnnotation '[]
    = TypeError ('Text "cannot find schema annotation")
  GetSchemaAnnotation ('AnnSchema d ': rs) = d
  GetSchemaAnnotation (r            ': rs) = GetSchemaAnnotation rs

-- | Find the annotation over the given type in the given set.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetTypeAnnotation (anns :: [Annotation domain t f]) (ty :: t) :: domain where
  GetTypeAnnotation '[] ty
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType ty)
  GetTypeAnnotation ('AnnType ty d ': rs) ty = d
  GetTypeAnnotation (r ': rs) ty = GetTypeAnnotation rs ty

-- | Find the annotation over the given field or choice in the given type.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetFieldAnnotation (anns :: [Annotation domain t f]) (ty :: t) (fl :: f) :: domain where
  GetFieldAnnotation '[] ty fl
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType ty ':<>: 'Text "/" ':<>: 'ShowType fl)
  GetFieldAnnotation ('AnnField ty fl d ': rs) ty fl = d
  GetFieldAnnotation (r                 ': rs) ty fl = GetFieldAnnotation rs ty fl
