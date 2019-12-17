---
layout: docs
title: Mu-Haskell
permalink: rpc/
---

# Services and servers

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

In order to use the library proficiently, we should look a bit at the code generated in the previous code. A type-level description of the messages is put into the type `QuickstartSchema`. However, there is some code you still have to write by hand, namely the Haskell type which correspond to that schema. Using `mu-schema` facilities, this amounts to declaring a bunch of data types and including `deriving (Generic, HasSchema Schema "type")` at the end of each of them.

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
newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloRequest")
newtype HelloResponse = HelloResponse { message :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloResponse")
```

The service declaration looks very similar to an schema declaration, but instead of record and enumerations you define *methods*. Each method has a name, a list of arguments, and a return type.

```haskell
import Mu.Rpc

-- GENERATED
type QuickstartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle ('FromSchema QuickstartSchema "HelloRequest") ]
                 ('RetSingle ('FromSchema QuickstartSchema "HelloResponse")) ]
```

In order to support both [Avro IDL](https://avro.apache.org/docs/current/idl.html) and [gRPC](https://grpc.io/), the declaration of the method arguments and returns in a bit fancier that you might expect:

* Each *argument* declares the schema type used for serialization. Furthermore, the argument can be declared as `ArgSingle` (only one value is provided by the client) or `ArgStream` (a stream of values is provided).
* The *return types* gives the same two choices under the names `RetSingle` or `RetStream`, and additionally supports the declaration of methods which may raise exceptions using `RetThrows`, or methods which do not retun any useful information using `RetNothing`.

Note that depending on the concrete implementation you use to run the server, one or more of these choices may not be available. For example, gRPC only supports one argument and return value, either single or streaming, but not exceptions.

## Implementing the service

In order to implement the service, you have to define the behavior of each method by means of a *handler*. You can use Haskell types for your handlers, given that you had previously declared that they can be mapped back and forth the schema types using `HasSchema`. For example, the following is a handler for the `SayHello` method in `Greeter`:

```haskell
sayHello :: (MonadServer m) => HelloRequest -> m HelloResponse
sayHello (HelloRequest nm) = return (HelloResponse ("hi, " <> nm))
```

Notice the use of `MonadServer` in this case. This gives us the ability to:

* Run arbitrary `IO` actions by using `liftIO`,
* Return an error code by calling `serverError`.

Being polymorphic here allows us to run the same server in multiple back-ends. Furthermore, by enlarging the set of abilities required for our monad `m`, we can [integrate with other libraries](transformer.md), including logging and resource pools.

Since you can declare more than one method in a service, you need to join them into a `Server`. You do so by using `(:<|>:)` between each handler and ending the sequence with `H0`. In addition to the name of the service, `Server` has an additional parameter which records the types of the handlers. Since that list may become quite long, we can ask GHC to write it for us by using the `PartialTypeSignatures` extension and writing an underscore `_` in that position.

```haskell
{-# language PartialTypeSignatures #-}

quickstartServer :: (MonadServer m) => ServerT QuickstartService m _
quickstartServer = Server (sayHello :<|>: H0)
```
