# `mu-schema`: format-independent schemas for serialization

Using `mu-schema` you can describe a schema for your data using type-level techniques. You can then automatically generate:

* conversion between you Haskell data types and the values as expected by the schema,
* generalization to [Avro](https://avro.apache.org/), [Protocol Buffers](https://developers.google.com/protocol-buffers/), and [JSON](https://www.json.org/).

Since `mu-schema` makes heavy use of type-level techniques, you need to open up the Pandora's box by enabling (at least) the following extensions: `PolyKinds` and `DataKinds`.

## Records and enumerations

Here is a simple schema which defines the schema types `gender`, `address`, and `person`:

```haskell
{-# language PolyKinds, DataKinds #-}

import Mu.Schema
import qualified Data.Text as T

type ExampleSchema
  = '[ 'DEnum   "gender" '["male", "female", "nb"]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address") ]
     ]
```

As you can see, a *schema* is just a list of schema types. Each of these types has a *name* and can either be an enumeration or a record.

* An *enumeration* defines a set of values that the type can take,
* A *record* contains a list of *fields*, each of them with a name and a *field type*. The allowed types for the fields are:
  * `TPrimitive` for primitive types such as `Int` and `Bool`. Note that if you want to have a string yoiu should *not* use the `String` from `Prelude`, but rather `Text` from `Data.Text`.
  * `TSchematic` to reference another type *in the same schema* by name.
  * `TOption`, `TList`, `TMap`, and `TUnion` are combinators for the field types.

Note that GHC requires all of `DEnum`, `DRecord`, `FieldDef`, and so forth to be prefixed by a quote sign `'`. This declares that we are working with [promoted types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion) (you do not have to understand what a promoted type is, but you need to remember to use the quote sign).

### Creating an initial schema

Writing the schema can be quite tiring, especially if most of the field names coincide between schema and Haskell type. As part of the library we offer you a facility to generate the initial schema using the GHC interpeter.

In order to use it, you need to create a Stack project with `mu-schema` as dependency. You then have to enter into an interpreter session and load the `Mu.Schema.FromTypes` module. Finally, you can ask for the schema of a list of types using `kind!`:

```haskell
> import Data.Text as T
> import Mu.Schema.FromTypes
> :set -XDataKinds -XKindSignatures
> :kind! (SchemaFromTypes '[ AsRecord Person "person", AsRecord Address "address", AsEnum Gender "gender" ] :: Schema')
(SchemaFromTypes '[ AsRecord Person "person", AsRecord Address "address", AsEnum Gender "gender" ] :: Schema') :: [TypeDef Symbol Symbol]
= '[ 'DRecord
       "person"
       '[ 'FieldDef "firstName" ('TPrimitive Text),
          'FieldDef "lastName" ('TPrimitive Text),
          'FieldDef "age" ('TOption ('TPrimitive Int)),
          'FieldDef "gender" ('TOption ('TSchematic "gender")),
          'FieldDef "address" ('TSchematic "address")],
     'DRecord
       "address"
       '[ 'FieldDef "postcode" ('TPrimitive Text),
          'FieldDef "country" ('TPrimitive Text)],
     'DEnum "gender" '["Male", "Female", "NonBinary"]]
```

The result of the last operation can be copied as-is in your source file, provided that you import all the required modules.

## Mapping Haskell types

These schemas become more useful once you can map your Haskell types to them. `mu-schema` uses the generics mechanism built in GHC to automatically derive these mappings, asuming that you declare your data types using field names.

```haskell
{-# language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# language DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics

data Address
  = Address { postcode :: T.Text
            , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (HasSchema ExampleSchema "address")
```

Once again, you need to enable some extensions in the compiler (but do not worry, GHC should tell you which ones you need in case you forgot). You first must include `Generic` in the list of automatically-derived classes. Then you *derive* the mapping by using the line:

```haskell
  deriving (HasSchema YourSchema "yourSchemaType")
```

## Customizing the mapping

Sometimes the names of the fields in the Haskell data type and the names of the fields in the schema do not match. For example, in our `gender` enumeration above the values are `Male`, `Female`, and `NonBinary`; but in the schema type you use `male`, `female`, and `nb`. By using a stand-along `HasSchema` instance you can declare this custom mapping:

```haskell
{-# language TypeFamilies #-}

instance HasSchema ExampleSchema "gender" Gender where
  type FieldMapping ExampleSchema "gender" Gender
    = '[ "Male"      ':<->: "male"
       , "Female"    ':<->: "female"
       , "NonBinary" ':<->: "nb" ]
```

### Protocol Buffers field identifiers

If you want to use (de)serialization to Protocol Buffers, you need to declare one more piece of information. A Protocol Buffer record or enumeration assigns both names and *numeric identifiers* to each field or value, respectively. Thus, if you want to use Protocol Buffers or gRPC support from `mu-grpc`, you need to write a few more lines:

```haskell
import Mu.Schema.Adapter.ProtoBuf

type instance ProtoBufFieldIds ExampleSchema "address"
  = '[ "postcode" ':<->: 1, "country" ':<->: 2 ]
```

## Registry

Schemas evolve over time. It is a good practice to keep an inventory of all the schemas you can work with, in the form of a *registry*. Using `mu-schema` you can declare one such registry by giving an instance of the `Registry` type family:

```haskell
{-# language TypeFamilies #-}

type instance Registry "example"
  = '[ 2 ':<->: ExampleSchemaV2, 1 ':<->: ExampleSchema ]
```

The argument to registry is a tag which identifies that set of schemas. Here we use a type-level string, but you can use any other kind. We then indicate to which type-level schema each version corresponds to. Once we have done that you can use functions like `fromRegistry` to try to parse a term into a Haskell type by trying each of the schemas.

## In the future

Unfortunately, in the current version you have to write all these mappings by hand. In the future, we intend to provide a quasiquoter which reads an Avro or Protocol Buffers schema definition and writes the schema type for you.