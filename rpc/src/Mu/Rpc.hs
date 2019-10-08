{-# language DataKinds, PolyKinds,
             GADTs, ExistentialQuantification,
             TypeFamilies, ConstraintKinds,
             TypeOperators,
             UndecidableInstances #-}
module Mu.Rpc where

import Data.Kind
import GHC.TypeLits

import Mu.Schema

type Service' = Service Symbol Symbol

-- | A service is a set of methods.
data Service serviceName methodName
  = Service serviceName [Method methodName]

-- | A method is defined by its name, arguments, and return type.
data Method methodName
  = Method methodName [Argument] Return

-- | Look up a method in a service definition using its name.
--   Useful to declare handlers like @HandlerIO (MyService :-->: "MyMethod")@.
type family (:-->:) (s :: Service snm mnm) (m :: mnm) :: Method mnm where
  'Service sname methods :-->: m = LookupMethod methods m

type family LookupMethod (s :: [Method mnm]) (m :: snm) :: Method snm where
  LookupMethod '[] m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  LookupMethod ('Method m args r ': ms) m = 'Method m args r
  LookupMethod (other            ': ms) m = LookupMethod ms m

-- | Defines how to handle the type
data TypeRef where
  FromSchema   :: Schema typeName fieldName -> typeName -> TypeRef
  -- | Registry subject, type to convert to, and preferred serialization version
  FromRegistry :: subject -> Type -> Nat -> TypeRef

-- | Defines the way in which arguments are handled.
data Argument where
  -- | Use a single value.
  ArgSingle :: TypeRef -> Argument
  -- | Consume a stream of values.
  ArgStream :: TypeRef -> Argument

-- | Defines the different possibilities for returning
--   information from a method.
data Return where
  -- | Return a single value.
  RetSingle :: TypeRef -> Return
  -- | Return a value or an error
  --   (this can be found in Avro IDL).
  RetThrows :: TypeRef -> TypeRef -> Return
  -- | Return a stream of values
  --   (this can be found in gRPC).
  RetStream :: TypeRef -> Return