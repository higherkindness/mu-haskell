{-# language DataKinds            #-}
{-# language GADTs                #-}
{-# language PolyKinds            #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
module Mu.Schema.Annotations where

import           Data.Kind
import           GHC.TypeLits

import           Mu.Schema.Definition

-- | Libraries can define custom annotations.
--   Each annotation belongs to a domain.
type AnnotationDomain = Type

-- | Libraries can define custom annotations
--   to indicate additional information.
data Annotation domain typeName fieldName where
  AnnSchema :: domain
            -> Annotation domain typeName fieldName
  AnnType   :: typeName -> domain
            -> Annotation domain typeName fieldName
  AnnField  :: typeName -> fieldName -> domain
            -> Annotation domain typeName fieldName

-- | This type family links each schema to
--   its corresponding annotations from one domain.
type family AnnotatedSchema domain (sch :: Schema typeName fieldName)
  :: [Annotation domain typeName fieldName]

type family GetSchemaAnnotation (anns :: [Annotation domain t f]) :: domain where
  GetSchemaAnnotation '[]
    = TypeError ('Text "cannot find schema annotation")
  GetSchemaAnnotation ('AnnSchema d ': rs) = d
  GetSchemaAnnotation (r            ': rs) = GetSchemaAnnotation rs

type family GetTypeAnnotation (anns :: [Annotation domain t f]) (ty :: t) :: domain where
  GetTypeAnnotation '[] ty
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType ty)
  GetTypeAnnotation ('AnnType ty d ': rs) ty = d
  GetTypeAnnotation (r ': rs) ty = GetTypeAnnotation rs ty

type family GetFieldAnnotation (anns :: [Annotation domain t f]) (ty :: t) (fl :: f) :: domain where
  GetFieldAnnotation '[] ty fl
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType ty ':<>: 'Text "/" ':<>: 'ShowType fl)
  GetFieldAnnotation ('AnnField ty fl d ': rs) ty fl = d
  GetFieldAnnotation (r                 ': rs) ty fl = GetFieldAnnotation rs ty fl
