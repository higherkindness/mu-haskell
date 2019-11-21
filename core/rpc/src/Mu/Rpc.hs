{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language GADTs                     #-}
{-# language PolyKinds                 #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
-- | Protocol-independent declaration of services
module Mu.Rpc (
  Service', Service(..)
, Annotation, Package, FindPackageName
, Method(..), (:-->:)
, TypeRef(..), Argument(..), Return(..)
) where

import           Data.Kind
import           GHC.TypeLits
import qualified Language.Haskell.TH as TH

import           Mu.Schema
import           Mu.Schema.Registry

type Service' = Service Symbol Symbol

-- | A service is a set of methods.
data Service serviceName methodName
  = Service serviceName [Annotation] [Method methodName]

-- | An annotation to define a package name.
--   This is used by some handlers, like gRPC.
data Package (s :: Symbol)

type family FindPackageName (anns :: [Annotation]) :: Symbol where
  FindPackageName '[] = TypeError ('Text "Cannot find package name for the service")
  FindPackageName (Package s ': rest) = s
  FindPackageName (other     ': rest) = FindPackageName rest

-- | A method is defined by its name, arguments, and return type.
data Method methodName
  = Method methodName [Annotation] [Argument] Return

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
  FromSchema   :: Schema typeName fieldName -> typeName -> TypeRef
  -- | Registry subject, type to convert to, and preferred serialization version
  FromRegistry :: Registry -> Type -> Nat -> TypeRef
  -- | To be used only during TH generation!
  FromTH       :: TH.Type -> TypeRef

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
