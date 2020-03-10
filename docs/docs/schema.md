---
layout: docs
title: Schemas
permalink: schema/
---

# Schemas

Using `mu-schema` you can describe a schema for your data using type-level techniques. You can then automatically generate:

* conversion between your Haskell data types and the values as expected by the schema,
* serialization to [Avro](https://avro.apache.org/), [Protocol Buffers](https://developers.google.com/protocol-buffers/), and [JSON](https://www.json.org/).

Since `mu-schema` makes heavy use of type-level techniques, you need to open up the Pandora's box by enabling (at least) the following extensions: `PolyKinds` and `DataKinds`.

## Records and enumerations

Here is a simple schema which defines the schema types `gender`, `address`, and `person`:

```haskell
{-# language PolyKinds, DataKinds #-}

import Mu.Schema
import qualified Data.Text as T

type ExampleSchema
  = '[ 'DEnum   "gender"
                '[ 'ChoiceDef "male"
                 , 'ChoiceDef "female"
                 , 'ChoiceDef "nb" ]
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
  * `TPrimitive` for primitive types such as `Int` and `Bool`. Note that if you want to have a string you should *not* use the `String` from `Prelude`, but rather `Text` from `Data.Text`.
  * `TSchematic` to reference another type *in the same schema* by name.
  * `TOption`, `TList`, `TMap`, and `TUnion` are combinators for the field types.

Note that GHC requires all of `DEnum`, `DRecord`, `FieldDef`, and so forth to be prefixed by a quote sign `'`. This declares that we are working with [promoted types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#datatype-promotion) (you do not have to understand what a promoted type is, but you need to remember to use the quote sign).

### Defining a schema using Protocol Buffers

As discussed in the introduction, `mu-schema` has been developed with some common schema formats in mind. Instead of writing the type-level schemas by hand, you can also import your [Protocol Buffers](https://developers.google.com/protocol-buffers/) schemas.

The idea is that your schema lives in an external file, so you can share it with other components of your system. To declare that we want the file to be pre-processed before compilation, we use a GHC feature called `TemplateHaskell`, hence the initial line starting with `language`.

```haskell
{-# language TemplateHaskell #-}

import Mu.Quasi.ProtoBuf

protobuf "ExampleSchema" "path/to/file.proto"
```

That single line asks the compiler to generate a `ExampleSchema` type which represents the schema from the given file. In addition, it also generates a mapping from fields to identifiers, as described below.

One word of warning: GHC reads the contents of the file *in order*, resolving `TemplateHaskell` blocks when found. Only then the results are visible to the rest of the file. In particular, the `protobuf` line should appear *before* any other code mentioning the `ExampleSchema` type.

### Schemas part of services

If you use the `grpc` function to import a gRPC `.proto` file in the type-level, that function already takes care of creating an appropiate schema for *all* the messages. If you prefer to have different schemas for different subsets of messages (for example, aggregated by services), you can either:

* Write the schemas by hand,
* Split the definition file into several ones, and import each of them in its own `protobuf` block.

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
  deriving (ToSchema   ExampleSchema "address")
  deriving (FromSchema ExampleSchema "address")
```

Once again, you need to enable some extensions in the compiler (but do not worry, GHC should tell you which ones you need in case you forgot). You first must include `Generic` in the list of automatically-derived classes. Then you *derive* the mapping by using the lines:

```haskell
  deriving (ToSchema   YourSchema "yourSchemaType")
  deriving (FromSchema YourSchema "yourSchemaType")
```

## Customizing the mapping

Sometimes the names of the fields in the Haskell data type and the names of the fields in the schema do not match. For example, in our schema above we use `male`, `female`, and `nb`, but in a Haskell enumeration the name of each constructor must begin with a capital letter. By using a standalone `ToSchema` instance you can declare a custom mapping from Haskell fields or constructors to schema fields or enum choices, respectively:

```haskell
{-# language DerivingVia  #-}
{-# language TypeFamilies #-}

type GenderFieldMapping
  = '[ "Male"      ':-> "male"
     , "Female"    ':-> "female"
     , "NonBinary" ':-> "nb" ]

data Gender = Male | Female | NonBinary
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "gender", FromSchema ExampleSchema "gender")
    via (CustomFieldMapping "gender" GenderFieldMapping Gender)
```

### Protocol Buffers field identifiers

If you want to use (de)serialization to Protocol Buffers, you need to declare one more piece of information. A Protocol Buffer record or enumeration assigns both names and *numeric identifiers* to each field or value, respectively. If you use `protobuf` or `grpc` to import your Protocol Buffers schemas, this is done automatically for you.

`mu-schema` supports extending the information of a schema by means of *annotations*. Annotations are linked to both a certain format (`ProtoBufAnnotation` in this case) and a certain schema. Furthermore, annotations may range over the whole schema, a specific record or enumeration, or a specific field or choice. In the case of Protocol Buffers, we only need the latter:

```haskell
{-# language TypeFamilies #-}

import Mu.Adapter.ProtoBuf

type instance AnnotatedSchema ProtoBufAnnotation ExampleSchema
  = '[ ...
     , 'AnnField "address" "postcode" ('ProtoBufId 1)
     , 'AnnField "address" "country " ('ProtoBufId 2)
     , ... ]
```
