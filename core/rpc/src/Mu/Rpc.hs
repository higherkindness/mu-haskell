{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language GADTs                     #-}
{-# language PolyKinds                 #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
{-|
Description : Protocol-independent declaration of services

This module defines a type-level language to describe
RPC-like microservices independently of the transport
and protocol.
-}
module Mu.Rpc (
  Package', Package(..)
, Service', Service(..), Object
, ServiceAnnotation, Method', Method(..), ObjectField
, LookupService, LookupMethod
, TypeRef(..), Argument', Argument(..), Return(..)
) where

import           Data.Kind
import           GHC.TypeLits
import qualified Language.Haskell.TH as TH

import           Mu.Schema
import           Mu.Schema.Registry

-- | Packages whose names are given by type-level strings.
type Package' = Package Symbol Symbol Symbol
-- | Services whose names are given by type-level strings.
type Service' = Service Symbol Symbol Symbol
-- | Methods whose names are given by type-level strings.
type Method' = Method Symbol Symbol Symbol
-- | Arguments whose names are given by type-level strings.
type Argument' = Argument Symbol Symbol
-- | Annotations for services. At this moment, such
--   annotations can be of any type.
type ServiceAnnotation = Type

-- | A package is a set of services.
data Package serviceName methodName argName
  = Package (Maybe serviceName)
            [Service serviceName methodName argName]

-- | A service is a set of methods.
data Service serviceName methodName argName
  = Service serviceName
            [ServiceAnnotation]
            [Method serviceName methodName argName]

-- | A method is defined by its name, arguments, and return type.
data Method serviceName methodName argName
  = Method methodName [ServiceAnnotation]
           [Argument serviceName argName]
           (Return serviceName)

-- Synonyms for GraphQL
-- | An object is a set of fields, in GraphQL lingo.
type Object = 'Service
-- | A field in an object takes some input objects,
--   and returns a value or some other object,
--   in GraphQL lingo.
type ObjectField = 'Method

type family LookupService (ss :: [Service snm mnm anm]) (s :: snm) :: Service snm mnm anm where
  LookupService '[] s = TypeError ('Text "could not find method " ':<>: 'ShowType s)
  LookupService ('Service s anns ms ': ss) s = 'Service s anns ms
  LookupService (other              ': ss) s = LookupService ss s

-- | Look up a method in a service definition using its name.
type family LookupMethod (s :: [Method snm mnm anm]) (m :: mnm) :: Method snm mnm anm where
  LookupMethod '[] m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  LookupMethod ('Method m anns args r ': ms) m = 'Method m anns args r
  LookupMethod (other                 ': ms) m = LookupMethod ms m

data TypeRef serviceName where
  -- | A primitive type.
  PrimitiveRef :: Type -> TypeRef serviceName
  -- | Chain with another service.
  ObjectRef    :: serviceName -> TypeRef serviceName
  -- | Point to schema.
  SchemaRef    :: Schema typeName fieldName -> typeName -> TypeRef serviceName
  -- | Registry subject, type to convert to, and preferred serialization version
  RegistryRef  :: Registry -> Type -> Nat -> TypeRef serviceName
  -- | To be used only during TH generation!
  THRef        :: TH.Type -> TypeRef serviceName
  -- Combinators found in the gRPC and GraphQL languages.
  -- | Represents a list of values.
  ListRef      :: TypeRef serviceName -> TypeRef serviceName
  -- | Represents a possibly-missing value.
  OptionalRef  :: TypeRef serviceName -> TypeRef serviceName

-- | Defines the way in which arguments are handled.
data Argument serviceName argName where
  -- | Use a single value.
  ArgSingle :: Maybe argName -> [ServiceAnnotation]
            -> TypeRef serviceName -> Argument serviceName argName
  -- | Consume a stream of values.
  ArgStream :: Maybe argName -> [ServiceAnnotation]
            -> TypeRef serviceName -> Argument serviceName argName

-- | Defines the different possibilities for returning
--   information from a method.
data Return serviceName where
  -- | Fire and forget.
  RetNothing :: Return serviceName
  -- | Return a single value.
  RetSingle  :: TypeRef serviceName -> Return serviceName
  -- | Return a stream of values.
  RetStream  :: TypeRef serviceName -> Return serviceName
  -- | Return a value or an error.
  RetThrows  :: TypeRef serviceName -> TypeRef serviceName -> Return serviceName
