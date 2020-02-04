---
layout: docs
title: Introduction
permalink: intro/
---

# Introduction to Mu-Haskell

Many companies have embraced microservices architectures as the best way to scale up their internal software systems, and separate work across different company divisions and development teams. Microservices architectures also allow teams to turn an idea or bug report into a working feature or fix in production more quickly, in accordance to the agile principles.

However, microservices are not without costs. Every connection between microservices becomes now a boundary that requires one service to act as a server, and the other to act as the client. Each service needs to include an implementation of the protocol, the encoding of the data for transmission, etc. The business logic of the application also starts to spread around several code bases, making it difficult to maintain.

## What is Mu-Haskell?

The main goal of Mu-Haskell is to allow you to focus on your domain logic, instead of worrying about format and protocol issues. To achieve this goal, Mu-Haskell provides two sets of packages:

* `mu-schema` and `mu-rpc` define schemas for data and services, in a format- and protocol-independent way. These schemas are checked at compile-time, so you also gain an additional layer of type-safety.
* `mu-avro`, `mu-protobuf`, `mu-grpc` (and other to come) implement each concrete format and protocol, following the interfaces laid out by the former two. In addition, most of those packages can turn a schema in the corresponding format into the corresponding one in `mu-schema` and `mu-rpc` terms, alleviating the need to duplicate definitions.

## Quickstart

### Super-quick summary

1. Create a new project with `stack new`.
2. Define your schema and your services in the `.proto` file.
3. Map to your Haskell data types in `src/Schema.hs`, or use optics.
4. Implement the server in `src/Main.hs`.

### Step by step

As an appetizer we are going to develop the same service as in the [gRPC Quickstart Guide](https://grpc.io/docs/quickstart/). The service is defined as a `.proto` file, which includes the schema for the messages and the signature for the methods in the service:

```java
service Service {
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

message HelloRequest { string name = 1; }
message HelloReply { string message = 1; }
```

To get started with the project, we provide a [Stack](https://docs.haskellstack.org) template (in fact, we recommend that you use Stack as your build tool, although Cabal should also work perfectly fine). You should run:

```
stack new my_project https://raw.githubusercontent.com/higherkindness/mu-haskell/master/templates/grpc-server.hsfiles -p "author-email:your@email.com" -p "author-name:Your name"
```

**WARNING:** Do not include a hyphen in your project name, as it will cause the template to generate a '.proto' file containing an invalid package name. Use `my_project`, not `my-project`.

This command creates a new folder called `my_project`, with a few files. The most important from those are the `.proto` file, in which you will define your service; `src/Schema.hs`, which loads the service definition at compile-time; and `src/Main.hs`, which contains the code of the server.

The first step to get your project running is defining the right schema and service. In this case, you can just copy the definition above after the `package` declaration.

#### Data type definition

The second step is to define Haskell types corresponding to the message types in the gRPC definition. The recommended route is to create new Haskell data types and check for compatibility at compile-time. The goal is to discourage from making your domain types simple copies of the protocol types. Another possibility is to use the `optics` bridge and work with lenses for the fields.

##### Using Haskell types

The aforementioned `.proto` file defines two messages. The corresponding data types are as follows:

```haskell
data HelloRequestMessage
  = HelloRequestMessage { name :: Maybe T.Text }
  deriving (Eq, Show, Generic
           , ToSchema   Maybe TheSchema "HelloRequest"
           , FromSchema Maybe TheSchema "HelloRequest")

data HelloReplyMessage
  = HelloReplyMessage { message :: Maybe T.Text }
  deriving (Eq, Show, Generic
           , ToSchema   Maybe TheSchema "HelloReply"
           , FromSchema Maybe TheSchema "HelloReply")
```

These data types should be added to the file `src/Schema.hs`, under the line that starts `grpc ...`. (See the [gRPC page]({% link docs/grpc.md %}) for information about what that line is doing.)

You can give the data types and their constructors any name you like. However, keep in mind that:

* The names of the fields must correspond with those in the `.proto` files. Otherwise you have to use a *custom mapping*, which is fully supported by `mu-schema` but requires more code.
* All the fields must be wrapped in `Maybe` since all fields in `proto3` are **optional by default**.
* The name `TheSchema` refers to a type generated by the `grpc` function, so it must match the first argument to that function.
* The name between quotes in each `deriving` clause defines the message type in the `.proto` file each data type corresponds to.
* To use the automatic-mapping functionality, it is required to also derive `Generic`, don't forget it!

##### Using optics

As we mentioned above, you may decide to not introduce new Haskell types, at the expense of losing some automatic checks against the current version of the schema. However, you gain access to a set of lenses and optics which can be used to inspect the values. In the Mu jargon, values from a schema which are not Haskell types are called *terms*, and we usually define type synonyms for each of them.

```haskell
type HelloRequestMessage' = Term Maybe TheSchema (TheSchema :/: "HelloRequest")
type HelloReplyMessage'   = Term Maybe TheSchema (TheSchema :/: "HelloReply")
```

The arguments to `Term` closely correspond to those in `FromSchema` and `ToSchema` described above.

#### Server implementation

If you try to compile the project right now by means of `stack build`, you will receive an error about `server` not having the right type. This is because you haven't yet defined any implementation for your service. This is one of the advantages of making the compiler aware of your service definitions: if the `.proto` file changes, you need to adapt your code correspondingly, or otherwise the project doesn't even compile!

Open the `src/Main.hs` file. The contents are quite small right now: a `main` function asks to run the gRPC service defined by `server`. The `server` function, on the other hand, declares that it implements the `Service` service in its signature, but contains no implementations.

```haskell
main :: IO ()
main = runGRpcApp 8080 server

server :: (MonadServer m) => ServerT Maybe Service m _
server = Server H0
```

The simplest way to provide an implementation for a service is to define one function for each method. You can define those functions completely in terms of Haskell data types; in our case `HelloRequestMessage` and `HelloReplyMessage`. Here is an example definition:

```haskell
sayHello :: (MonadServer m) => HelloRequestMessage -> m HelloReplyMessage
sayHello (HelloRequestMessage nm)
  = return (HelloReplyMessage (("hi, " <>) <$> nm))
```

The `MonadServer` portion in the type is mandated by `mu-rpc`; it tells us that in a method we can perform any `IO` actions and additionally throw server errors (for conditions such as *not found*). We do not make use of any of those here, so we simply use `return` with a value. We could even make the definition a bit more polymorphic by replacing `MonadServer` by `Monad`.

Another possibility is to use the `optics`-based API in `Mu.Schema.Optics`. In that case, you access the value of the fields using `(^.)` followed by the name of the field after `#`, and build messages by using `record` followed by a tuple of the components. The previous example would then be written:

```haskell
{-# language OverloadedLabels #-}

sayHello :: (MonadServer m) => HelloRequestMessage' -> m HelloReplyMessage'
sayHello (HelloRequestMessage nm)
  = return $ record (("hi, " <>) <$> (nm ^. #name))
```

How does `server` know that `sayHello` (any of the two versions) is part of the implementation of the service? We have to tell it, by adding `sayHello` to the list of methods. Unfortunately, we cannot use a normal list, so we use `(:<|>:)` to join them, and `H0` to finish it.

```haskell
server = Server (sayHello :<|>: H0)
```

At this point you can build the project using `stack build`, and then execute via `stack run`. This spawns a gRPC server at port 8080, which you can test using applications such as [BloomRPC](https://github.com/uw-labs/bloomrpc).
