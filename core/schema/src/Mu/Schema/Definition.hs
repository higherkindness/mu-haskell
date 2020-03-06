{-# language DataKinds            #-}
{-# language FlexibleInstances    #-}
{-# language PolyKinds            #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeApplications     #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
{-|
Description : Definition of schemas

This module gives a set of combinators
to define schemas in the sense of Avro
or Protocol Buffers.

In order to re-use definitions at both
the type and term levels, the actual
constructors are defined in types ending
with @B@, and are parametrized by the type
used to describe identifiers.
The versions without the suffix set this
parameter to 'Type', and are thought as the
API to be used in the type-level.
If you use 'reflectSchema' to obtain a term-
level representation, the parameter is set
to 'TypeRep'.
-}
module Mu.Schema.Definition (
-- * Definition of schemas
  Schema', Schema, SchemaB
, TypeDef, TypeDefB(..)
, ChoiceDef(..)
, FieldDef, FieldDefB(..)
, FieldType, FieldTypeB(..)
, (:/:)
-- * One-to-one mappings
, Mapping(..), Mappings
-- ** Finding correspondences
, MappingRight, MappingLeft
-- * Reflection to term-level
, reflectSchema
, reflectFields, reflectChoices
, reflectFieldTypes, reflectFieldType
-- * Supporting type classes
, KnownName(..)
) where

import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits

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
--   you always want to use 'Symbol'.
type Schema typeName fieldName
  = SchemaB Type typeName fieldName
-- | A set of type definitions,
--   parametric on type representations.
type SchemaB builtin typeName fieldName
  = [TypeDefB builtin typeName fieldName]

-- | Defines a type in a schema.
--   Each type can be:
--   * a record: a list of key-value pairs,
--   * an enumeration: an element of a list of choices,
--   * a reference to a primitive type.
type TypeDef = TypeDefB Type
-- | Defines a type in a schema,
--   parametric on type representations.
data TypeDefB builtin typeName fieldName
  = -- | A list of key-value pairs.
    DRecord typeName [FieldDefB builtin typeName fieldName]
    -- | An element of a list of choices.
  | DEnum   typeName [ChoiceDef fieldName]
    -- | A reference to a primitive type.
  | DSimple (FieldTypeB builtin typeName)

-- | Defines each of the choices in an enumeration.
newtype ChoiceDef fieldName
  = -- | One single choice from an enumeration.
    ChoiceDef fieldName

-- | Defines a field in a record
--   by a name and the corresponding type.
type FieldDef = FieldDefB Type
-- | Defines a field in a record,
--   parametric on type representations.
data FieldDefB builtin typeName fieldName
  = -- | One single field in a record.
    FieldDef fieldName (FieldTypeB builtin typeName)

-- | Types of fields of a record.
--   References to other types in the same schema
--   are done via the 'TSchematic' constructor.
type FieldType = FieldTypeB Type
-- | Types of fields of a record,
--   parametric on type representations.
data FieldTypeB builtin typeName
  = -- | Null, as found in Avro.
    TNull
    -- | Reference to a primitive type, such as integers or Booleans.
    --   The set of supported primitive types depends on the protocol.
  | TPrimitive builtin
    -- | Reference to another type in the schema.
  | TSchematic typeName
    -- | Optional value.
  | TOption (FieldTypeB builtin typeName)
    -- | List of values.
  | TList   (FieldTypeB builtin typeName)
    -- | Map of values.
    --   The set of supported key types depends on the protocol.
  | TMap    (FieldTypeB builtin typeName) (FieldTypeB builtin typeName)
    -- | Represents a choice between types.
  | TUnion  [FieldTypeB builtin typeName]

instance KnownName n => KnownName ('DRecord n fields) where
  nameVal _ = nameVal (Proxy @n)
instance KnownName n => KnownName ('DEnum n choices) where
  nameVal _ = nameVal (Proxy @n)
instance KnownName n => KnownName ('ChoiceDef n) where
  nameVal _ = nameVal (Proxy @n)
instance KnownName n => KnownName ('FieldDef n t) where
  nameVal _ = nameVal (Proxy @n)

-- | Lookup a type in a schema by its name.
type family (sch :: Schema t f) :/: (name :: t) :: TypeDef t f where
  '[] :/: name = TypeError ('Text "Cannot find type " ':<>: 'ShowType name ':<>: 'Text " in the schema")
  ('DRecord name fields  ': rest) :/: name = 'DRecord name fields
  ('DEnum   name choices ': rest) :/: name = 'DEnum   name choices
  (other                 ': rest) :/: name = rest :/: name

-- | Defines a mapping between two elements.
data Mapping  a b = a :-> b
-- | Defines a set of mappings between elements of @a@ and @b@.
type Mappings a b = [Mapping a b]

-- | Finds the corresponding right value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
--   When the return type is 'Type', return ' ()'
--   if the value is not found.
type family MappingRight (ms :: Mappings a b) (v :: a) :: b where
  MappingRight '[] (v :: Symbol) = (v :: Symbol)
  MappingRight '[] (v :: Symbol) = (() :: Type)
  MappingRight '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingRight ((x ':-> y) ': rest) x = y
  MappingRight (other      ': rest) x = MappingRight rest x

-- | Finds the corresponding left value of @v@
--   in a mapping @ms@. When the kinds are 'Symbol',
--   return the same value if not found.
--   When the return type is 'Type', return ' ()'
--   if the value is not found.
type family MappingLeft (ms :: Mappings a b) (v :: b) :: a where
  MappingLeft '[] (v :: Symbol) = (v :: Symbol)
  MappingLeft '[] (v :: Symbol) = (() :: Type)
  MappingLeft '[] v             = TypeError ('Text "Cannot find value " ':<>: 'ShowType v)
  MappingLeft ((x ':-> y) ': rest) y = x
  MappingLeft (other      ': rest) y = MappingLeft rest y

class ReflectSchema (s :: Schema tn fn) where
  -- | Reflect a schema into term-level.
  reflectSchema :: Proxy s -> SchemaB TypeRep String String
instance ReflectSchema '[] where
  reflectSchema _ = []
instance (ReflectFields fields, KnownName name, ReflectSchema s)
         => ReflectSchema ('DRecord name fields ': s) where
  reflectSchema _ = DRecord (nameVal (Proxy @name)) (reflectFields (Proxy @fields))
                  : reflectSchema (Proxy @s)
instance (ReflectChoices choices, KnownName name, ReflectSchema s)
         => ReflectSchema ('DEnum name choices ': s) where
  reflectSchema _ = DEnum (nameVal (Proxy @name)) (reflectChoices (Proxy @choices))
                  : reflectSchema (Proxy @s)
instance (ReflectFieldType ty, ReflectSchema s)
         => ReflectSchema ('DSimple ty ': s) where
  reflectSchema _ = DSimple (reflectFieldType (Proxy @ty))
                  : reflectSchema (Proxy @s)

class ReflectFields (fs :: [FieldDef tn fn]) where
  -- | Reflect a list of fields into term-level.
  reflectFields :: Proxy fs -> [FieldDefB TypeRep String String]
instance ReflectFields '[] where
  reflectFields _ = []
instance (KnownName name, ReflectFieldType ty, ReflectFields fs)
         => ReflectFields ('FieldDef name ty ': fs) where
  reflectFields _ = FieldDef (nameVal (Proxy @name)) (reflectFieldType (Proxy @ty))
                  : reflectFields (Proxy @fs)

class ReflectChoices (cs :: [ChoiceDef fn]) where
  -- | Reflect a list of enumeration choices into term-level.
  reflectChoices :: Proxy cs -> [ChoiceDef String]
instance ReflectChoices '[] where
  reflectChoices _ = []
instance (KnownName name, ReflectChoices cs)
         => ReflectChoices ('ChoiceDef name ': cs) where
  reflectChoices _ = ChoiceDef (nameVal (Proxy @name))
                   : reflectChoices (Proxy @cs)

class ReflectFieldType (ty :: FieldType tn) where
  -- | Reflect a schema type into term-level.
  reflectFieldType :: Proxy ty -> FieldTypeB TypeRep String
instance ReflectFieldType 'TNull where
  reflectFieldType _ = TNull
instance (Typeable ty) => ReflectFieldType ('TPrimitive ty) where
  reflectFieldType _ = TPrimitive (typeRep (Proxy @ty))
instance (KnownName nm) => ReflectFieldType ('TSchematic nm) where
  reflectFieldType _ = TSchematic (nameVal (Proxy @nm))
instance (ReflectFieldType t) => ReflectFieldType ('TOption t) where
  reflectFieldType _ = TOption (reflectFieldType (Proxy @t))
instance (ReflectFieldType t) => ReflectFieldType ('TList t) where
  reflectFieldType _ = TList (reflectFieldType (Proxy @t))
instance (ReflectFieldType k, ReflectFieldType v)
         => ReflectFieldType ('TMap k v) where
  reflectFieldType _ = TMap (reflectFieldType (Proxy @k)) (reflectFieldType (Proxy @v))
instance (ReflectFieldTypes ts) => ReflectFieldType ('TUnion ts) where
  reflectFieldType _ = TUnion (reflectFieldTypes (Proxy @ts))

class ReflectFieldTypes (ts :: [FieldType tn]) where
  -- | Reflect a list of schema types into term-level.
  reflectFieldTypes :: Proxy ts -> [FieldTypeB TypeRep String]
instance ReflectFieldTypes '[] where
  reflectFieldTypes _ = []
instance (ReflectFieldType t, ReflectFieldTypes ts)
         => ReflectFieldTypes (t ': ts) where
  reflectFieldTypes _ = reflectFieldType (Proxy @t) : reflectFieldTypes (Proxy @ts)
