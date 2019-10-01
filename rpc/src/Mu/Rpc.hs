{-# language DataKinds, PolyKinds,
             GADTs, ExistentialQuantification,
             TypeFamilies, ConstraintKinds,
             TypeOperators,
             UndecidableInstances #-}
module Mu.Rpc where

import Data.Kind
import GHC.TypeLits

import Mu.Schema

type Service' = Service Symbol

-- | A service is a set of methods.
type Service serviceName
  = [Method serviceName]

-- | A method is defined by its name, arguments, and return type.
data Method serviceName
  = Method serviceName [Argument] Return

-- | Look up a method in a service definition using its name.
--   Useful to declare handlers like @HandlerIO (MyService :-->: "MyMethod")@.
type family (:-->:) (s :: Service snm) (m :: snm) :: Method snm where
  '[] :-->: m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  ('Method m args r ': ms) :-->: m = 'Method m args r
  (other ': ms) :-->: m = ms :-->: m

-- | This is used to pack together a schema and a type defined in that schema.
type SchemaAndType typeName fieldName
  = (Schema typeName fieldName, typeName)

-- | Version of 'HasSchema' that works with 'SchemaAndType'
type family HasSchema' (st :: SchemaAndType typeName fieldName) (t :: Type) :: Constraint where
  HasSchema' '(sch, sty) t = HasSchema sch sty t

-- | Defines the way in which arguments are handled.
data Argument where
  -- | Use a single value.
  ArgSingle :: SchemaAndType typeName fieldName -> Argument
  -- | Consume a stream of values.
  ArgStream :: SchemaAndType typeName fieldName -> Argument

-- | Defines the different possibilities for returning
--   information from a method.
data Return where
  -- | Return a single value.
  RetSingle :: SchemaAndType typeName fieldName -> Return
  -- | Return a value or an error
  --   (this can be found in Avro IDL).
  RetThrows :: SchemaAndType typeName fieldName
            -> SchemaAndType typeName fieldName
            -> Return
  -- | Return a stream of values
  --   (this can be found in gRPC).
  RetStream :: SchemaAndType typeName fieldName -> Return