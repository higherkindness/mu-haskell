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
  Service', Service(..)
, ServiceAnnotation, Package, FindPackageName
, Method(..), (:-->:)
, TypeRef(..), Argument(..), Return(..)
) where

import           Data.Kind
import           GHC.TypeLits
import qualified Language.Haskell.TH as TH

import           Mu.Schema
import           Mu.Schema.Registry

-- | Services whose names are given by type-level strings.
type Service' = Service Symbol Symbol
-- | Annotations for services. At this moment, such
--   annotations can be of any type.
type ServiceAnnotation = Type

-- | A service is a set of methods.
data Service serviceName methodName
  = Service serviceName [ServiceAnnotation] [Method methodName]

-- | An annotation to define a package name.
--   This is used by some handlers, like gRPC.
data Package (s :: Symbol)

-- | Find the 'Package' for a service, to be found
--   as part of the annotations.
type family FindPackageName (anns :: [ServiceAnnotation]) :: Symbol where
  FindPackageName '[] = TypeError ('Text "Cannot find package name for the service")
  FindPackageName (Package s ': rest) = s
  FindPackageName (other     ': rest) = FindPackageName rest

-- | A method is defined by its name, arguments, and return type.
data Method methodName
  = Method methodName [ServiceAnnotation] [Argument] Return

-- | Look up a method in a service definition using its name.
--   Useful to declare handlers like @HandlerIO (MyService :-->: "MyMethod")@.
type family (:-->:) (s :: Service snm mnm) (m :: mnm) :: Method mnm where
  'Service sname anns methods :-->: m = LookupMethod methods m

type family LookupMethod (s :: [Method mnm]) (m :: snm) :: Method snm where
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
data Argument where
  -- | Use a single value.
  ArgSingle :: TypeRef -> Argument
  -- | Consume a stream of values.
  ArgStream :: TypeRef -> Argument

-- | Defines the different possibilities for returning
--   information from a method.
data Return where
  -- | Fire and forget.
  RetNothing :: Return
  -- | Return a single value.
  RetSingle :: TypeRef -> Return
  -- | Return a value or an error
  --   (this can be found in Avro IDL).
  RetThrows :: TypeRef -> TypeRef -> Return
  -- | Return a stream of values
  --   (this can be found in gRPC).
  RetStream :: TypeRef -> Return
