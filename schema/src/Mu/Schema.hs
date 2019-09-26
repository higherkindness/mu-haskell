{-# language PolyKinds             #-}
{-# language DataKinds             #-}
{-# language GADTs                 #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language FlexibleInstances     #-}
{-# language FlexibleContexts      #-}
{-# language TypeApplications      #-}
{-# language ScopedTypeVariables   #-}
{-# language UndecidableInstances  #-}
{-# language MultiParamTypeClasses #-}
{-# language DefaultSignatures     #-}
-- | Schemas for Mu microservices
module Mu.Schema (
  -- * Schema definition
  Schema, Schema', KnownName(..)
, TypeDef(..), FieldDef(..), FieldType(..)
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