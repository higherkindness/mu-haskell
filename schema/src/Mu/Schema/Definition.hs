{-# language PolyKinds, DataKinds,
             TypeFamilies, TypeOperators,
             UndecidableInstances,
             FlexibleInstances #-}
-- | Schema definition
module Mu.Schema.Definition where

import Data.Kind
import GHC.TypeLits

-- | A set of type definitions,
--   where the names of types and fields are
--   defined by type-level strings ('Symbol's).
type Schema' = Schema Symbol Symbol

-- | Type names and field names can be of any
--   kind, but for many uses we need a way
--   to turn them into strings at run-time.
--   This class generalizes 'KnownSymbol'.
class KnownName (a :: k) where
  nameVal :: proxy a -> String
instance KnownSymbol s => KnownName (s :: Symbol) where
  nameVal = symbolVal
instance KnownName 'True where
  nameVal _ = "True"
instance KnownName 'False where
  nameVal _ = "False"
instance KnownNat n => KnownName (n :: Nat) where
  nameVal = show . natVal

-- | A set of type definitions.
--   In general, we can use any kind we want for
--   both type and field names, although in practice
--   you always want to use 'Schema''.
type Schema typeName fieldName
  = [TypeDef typeName fieldName]

-- | Defines a type in a schema.
--   Each type can be:
--   * a record: a list of key-value pairs,
--   * an enumeration: an element of a list of choices,
--   * a reference to a primitive type.
data TypeDef typeName fieldName
  = DRecord typeName [FieldDef typeName fieldName]
  | DEnum   typeName [fieldName]
  | DSimple (FieldType typeName)

-- | Defines a field in a record
--   by a name and the corresponding type.
data FieldDef typeName fieldName
  = FieldDef fieldName (FieldType typeName)

-- | Types of fields of a record.
--   References to other types in the same schema
--   are done via the 'TSchematic' constructor.
data FieldType typeName
  = TNull
  | TPrimitive Type
  | TSchematic typeName
  | TOption (FieldType typeName)
  | TList   (FieldType typeName)
  | TMap    (FieldType typeName) (FieldType typeName)
  | TUnion  [FieldType typeName]

-- | Lookup a type in a schema by its name.
type family (sch :: Schema t f) :/: (name :: t) :: TypeDef t f where
  '[] :/: name = TypeError ('Text "Cannot find type " ':<>: 'ShowType name ':<>: 'Text " in the schema")
  ('DRecord name fields  ': rest) :/: name = 'DRecord name fields
  ('DEnum   name choices ': rest) :/: name = 'DEnum   name choices
  (other                 ': rest) :/: name = rest :/: name

-- | Defines a mapping between two elements.
data Mapping  a b = a :<->: b
-- | Defines a set of mappings between elements of @a@ and @b@.
type Mappings a b = [Mapping a b]

-- | Finds the corresponding right value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
type family MappingRight (ms :: Mappings a b) (v :: a) :: b where
  MappingRight '[] (v :: Symbol) = v
  MappingRight '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingRight ((x ':<->: y) ': rest) x = y
  MappingRight (other        ': rest) x = MappingRight rest x

-- | Finds the corresponding left value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
type family MappingLeft (ms :: Mappings a b) (v :: b) :: a where
  MappingLeft '[] (v :: Symbol) = v
  MappingLeft '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingLeft ((x ':<->: y) ': rest) y = x
  MappingLeft (other        ': rest) y = MappingLeft rest y