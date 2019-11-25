# Schema definition

Using `mu-schema` you can describe a schema for your data using type-level techniques. You can then automatically generate:

* conversion between your Haskell data types and the values as expected by the schema,
* generalization to [Avro](https://avro.apache.org/), [Protocol Buffers](https://developers.google.com/protocol-buffers/), and [JSON](https://www.json.org/).

Since `mu-schema` makes heavy use of type-level techniques, you need to open up the Pandora's box by enabling (at least) the following extensions: `PolyKinds` and `DataKinds`.

## Records and enumerations

Here is a simple schema which defines the schema types `gender`, `address`, and `person`:

```haskell
{-# language PolyKinds, DataKinds #-}

import Mu.Schema
import qualified Data.Text as T

type ExampleSchema
  = '[ 'DEnum   "gender" '[]
                '[ 'ChoiceDef "male"   '[]
                 , 'ChoiceDef "female" '[]
                 , 'ChoiceDef "nb"     '[] ]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" '[] ('TPrimitive T.Text)
                , 'FieldDef "country"  '[] ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" '[] ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  '[] ('TPrimitive T.Text)
                 , 'FieldDef "age"       '[] ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    '[] ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   '[] ('TSchematic "address") ]
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

The most common case is that your schema lives in an external file, maybe shared with other components of your system. To declare that we want the file to be pre-processed before compilation, we use a GHC feature called a *quasi-quote*. Be careful with the ending of the quasi-quote, which is a weird combination `|]`.

```haskell
{-# language QuasiQuotes #-}

type ExampleSchema = [protobufFile|path/to/file.proto|]
```

Another possibility is to write them in-line. In that case you replace `protobufFile` with `protobuf` and write the schema directly between the `|` symbols.

```haskell
{-# language QuasiQuotes #-}

type ExampleSchema = [protobuf|
enum gender {
  male   = 1;
  female = 2;
  nb     = 3;
}
message address {
  string postcode = 1;
  string country  = 2;
}
message person {
  string  firstName = 1;
  string  lastName  = 2;
  int     age       = 3;
  gender  gender    = 4;
  address address   = 5;
}
|]
```

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

Sometimes the names of the fields in the Haskell data type and the names of the fields in the schema do not match. For example, in our schema above we use `male`, `female`, and `nb`, but in a Haskell enumeration the name of each constructor must begin with a capital letter. By using a stand-along `HasSchema` instance you can declare a custom mapping from Haskell fields or constructors to schema fields or enum choices, respectively:

```haskell
{-# language TypeFamilies #-}

data Gender = Male | Female | NonBinary

instance HasSchema ExampleSchema "gender" Gender where
  type FieldMapping ExampleSchema "gender" Gender
    = '[ "Male"      ':-> "male"
       , "Female"    ':-> "female"
       , "NonBinary" ':-> "nb" ]
```

### Protocol Buffers field identifiers

If you want to use (de)serialization to Protocol Buffers, you need to declare one more piece of information. A Protocol Buffer record or enumeration assigns both names and *numeric identifiers* to each field or value, respectively. This is done via an *annotation* in each field:

```haskell
import Mu.Schema.Adapter.ProtoBuf

type ExampleSchema
  = '[ ...
     , 'DRecord "address"
               '[ 'FieldDef "postcode" '[ ProtoBufId 1 ] ('TPrimitive T.Text)
                , 'FieldDef "country"  '[ ProtoBufId 2 ] ('TPrimitive T.Text) ]
     , ... ]
```

If you use the `protobuf` or `protobufFile` quasi-quoters to import your Protocol Buffers schemas, this is done automatically for you.

## Registry

Schemas evolve over time. It is a good practice to keep an inventory of all the schemas you can work with, in the form of a *registry*. Using `mu-schema` you can declare one such registry by giving an instance of the `Registry` type family:

```haskell
{-# language TypeFamilies #-}

type instance Registry "example"
  = '[ 2 ':-> ExampleSchemaV2, 1 ':-> ExampleSchema ]
```

The argument to registry is a tag which identifies that set of schemas. Here we use a type-level string, but you can use any other kind. We then indicate to which type-level schema each version corresponds to. Once we have done that you can use functions like `fromRegistry` to try to parse a term into a Haskell type by trying each of the schemas.
