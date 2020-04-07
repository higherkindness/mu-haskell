---
layout: docs
title: RPC services
permalink: rpc/
---

# RPC services

There are several formats in the wild used to declare service APIs, including [Avro IDL](https://avro.apache.org/docs/current/idl.html), [gRPC](https://grpc.io/), and [OpenAPI](https://swagger.io/specification/). `mu-rpc` abstract the commonalities into a single type-level format for declaring these services, building on the format-independent schema facilities of `mu-schema`. In addition, this package provides a generic notion of *server* of a service. One such server defines one behavior for each method in the service, but does not bother with (de)serialization mechanisms.

## Importing the schema and the service

Let us begin with an example taken from the [gRPC Quickstart Guide](https://grpc.io/docs/quickstart/):

```java
package helloworld;

service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

message HelloRequest { string name = 1; }
message HelloReply { string message = 1; }
```

As with our sibling `mu-schema` library, we use type-level techniques to represent the messages and services. Since the mapping from such a Protocol Buffers file into the require types is quite direct, you can just import them using one line (in addition to enabling the `TemplateHaskell` extension):

```haskell
{-# language TemplateHaskell #-}

import Mu.Quasi.GRpc

grpc "QuickstartSchema" (const "QuickstartService") "quickstart.proto"
```

The `grpc` function takes three arguments:

* The first one defines the name of the schema type which is going to be generated, and which includes the declaration of all the messages in the file.
* The second one declares how to map the name of *each* service in the file (since more than one may appear) to the name of a Haskell type. In this case, we declare a constant name "QuickstartService". But we could also use `(++ "Service")`, which would then give `GreeterService` as name for the only service in the file.
* The third argument is the route to the file *with respect to the project root*.

This is everything you need to start using gRPC services and clients in Haskell!

### Looking at the resulting code

In order to use the library proficiently, we should look a bit at the code generated in the previous sample. A type-level description of the messages is put into the type `QuickstartSchema`. However, there is some code you still have to write by hand, namely the Haskell type which correspond to that schema. Using `mu-schema` facilities, this amounts to declaring a bunch of data types and including `deriving (Generic, ToSchema <SchemaName> "<MessageType>", FromSchema <SchemaName> "<MessageType>")` at the end of each of them.

```haskell
{-# language PolyKinds, DataKinds, TypeFamilies #-}
{-# language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# language DeriveGeneric, DeriveAnyClass #-}

import qualified Data.Text as T
import GHC.Generics

import Mu.Adapter.ProtoBuf
import Mu.Schema

-- GENERATED
type QuickstartSchema
  = '[ 'DRecord "HelloRequest"  '[ 'FieldDef "name"    ('TPrimitive T.Text) ]
     , 'DRecord "HelloResponse" '[ 'FieldDef "message" ('TPrimitive T.Text) ] ]

type instance AnnotatedSchema ProtoBufAnnotation QuickstartSchema
  = '[ 'AnnField "HelloRequest"  "name"    ('ProtoBufId 1)
     , 'AnnField "HelloResponse" "message" ('ProtoBufId 1) ]

-- TO BE WRITTEN
newtype HelloRequest
  = HelloRequest { name :: T.Text }
  deriving (Generic
           , ToSchema   QuickstartSchema "HelloRequest"
           , FromSchema QuickstartSchema "HelloRequest")
newtype HelloResponse
  = HelloResponse { message :: T.Text }
  deriving (Generic
           , ToSchema   QuickstartSchema "HelloResponse"
           , FromSchema QuickstartSchema "HelloResponse")
```

The service declaration looks very similar to a schema declaration, but instead of records and enumerations you define *methods*. Each method has a name, a list of arguments, and a return type.

```haskell
import Mu.Rpc

-- GENERATED
type QuickstartService
  = 'Service "Greeter"
      '[ 'Method "SayHello" '[]
        '[ 'ArgSingle 'Nothing '[] ('FromSchema QuickstartSchema "HelloRequest") ]
        ('RetSingle ('FromSchema QuickstartSchema "HelloResponse")) ]
```

In order to support both [Avro IDL](https://avro.apache.org/docs/current/idl.html) and [gRPC](https://grpc.io/), the declaration of the method arguments and return types is a bit fancier than you might expect:

* Each *argument* declares the schema type used for serialization. Furthermore, the argument can be declared as `ArgSingle` (only one value is provided by the client) or `ArgStream` (a stream of values is provided).
* gRPC defines no names for arguments, hence the use of `Nothing` in `ArgSingle`. Other service APIs, like GraphQL, have names on that possitions.
* The *return types* gives the same two choices under the names `RetSingle` or `RetStream`, and additionally supports the declaration of methods which may raise exceptions using `RetThrows`, or methods which do not retun any useful information using `RetNothing`.

Note that depending on the concrete implementation you use to run the server, one or more of these choices may not be available. For example, gRPC only supports one argument and return value, either single or streaming, but not exceptions.
