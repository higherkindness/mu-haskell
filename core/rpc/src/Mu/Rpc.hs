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
, Service', Service(..)
, ServiceAnnotation, Method(..)
, LookupService, LookupMethod
, TypeRef(..), Argument(..), Return(..)
) where

import           Data.Kind
import           GHC.TypeLits
import qualified Language.Haskell.TH as TH

import           Mu.Schema
import           Mu.Schema.Registry

-- | Packages whose names are given by type-level strings.
type Package' = Package Symbol Symbol
-- | Services whose names are given by type-level strings.
type Service' = Service Symbol Symbol
-- | Annotations for services. At this moment, such
--   annotations can be of any type.
type ServiceAnnotation = Type

-- | A package is a set of services.
data Package serviceName methodName
  = Package (Maybe serviceName)
            [Service serviceName methodName]

-- | A service is a set of methods.
data Service serviceName methodName
  = Service serviceName
            [ServiceAnnotation]
            [Method serviceName methodName]

-- | A method is defined by its name, arguments, and return type.
data Method serviceName methodName
  = Method methodName [ServiceAnnotation]
           [Argument serviceName]
           (Return serviceName)

type family LookupService (ss :: [Service snm mnm]) (s :: snm) :: Service snm mnm where
  LookupService '[] s = TypeError ('Text "could not find method " ':<>: 'ShowType s)
  LookupService ('Service s anns ms ': ss) s = 'Service s anns ms
  LookupService (other              ': ss) s = LookupService ss s

-- | Look up a method in a service definition using its name.
type family LookupMethod (s :: [Method snm mnm]) (m :: mnm) :: Method snm mnm where
  LookupMethod '[] m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  LookupMethod ('Method m anns args r ': ms) m = 'Method m anns args r
  LookupMethod (other                 ': ms) m = LookupMethod ms m

data TypeRef serviceName where
  -- | A primitive type.
  PrimitiveRef :: Type -> TypeRef serviceName
  -- | Chain with another service.
  ServiceRef   :: serviceName -> TypeRef serviceName
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
data Argument serviceName where
  -- | Use a single value.
  ArgSingle :: TypeRef serviceName -> Argument serviceName
  -- | Consume a stream of values.
  ArgStream :: TypeRef serviceName -> Argument serviceName

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
