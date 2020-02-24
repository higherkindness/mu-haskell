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
, ServiceAnnotation
, Method(..), (:-->:)
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

-- | Look up a method in a service definition using its name.
--   Useful to declare handlers like @HandlerIO (MyService :-->: "MyMethod")@.
type family (:-->:) (s :: Service snm mnm) (m :: mnm) :: Method snm mnm where
  'Service sname anns methods :-->: m = LookupMethod methods m

type family LookupMethod (s :: [Method snm mnm]) (m :: mnm) :: Method snm mnm where
  LookupMethod '[] m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  LookupMethod ('Method m anns args r ': ms) m = 'Method m anns args r
  LookupMethod (other                 ': ms) m = LookupMethod ms m

-- | Defines how to handle the type
data TypeRef where
  ViaSchema   :: Schema typeName fieldName -> typeName -> TypeRef
  -- | Registry subject, type to convert to, and preferred serialization version
  ViaRegistry :: Registry -> Type -> Nat -> TypeRef
  -- | To be used only during TH generation!
  ViaTH       :: TH.Type -> TypeRef

-- | Defines the way in which arguments are handled.
data Argument serviceName where
  -- | Use a single value.
  ArgSingle :: TypeRef -> Argument serviceName
  -- | Consume a stream of values.
  ArgStream :: TypeRef -> Argument serviceName

-- | Defines the different possibilities for returning
--   information from a method.
data Return serviceName where
  -- | Fire and forget.
  RetNothing :: Return serviceName
  -- | Return a single value.
  RetSingle :: TypeRef -> Return serviceName
  -- | Return a value or an error
  --   (this can be found in Avro IDL).
  RetThrows :: TypeRef -> TypeRef -> Return serviceName
  -- | Return a stream of values
  --   (this can be found in gRPC).
  RetStream :: TypeRef -> Return serviceName
  -- | Continue with another service.
  RetService :: serviceName -> Return serviceName
