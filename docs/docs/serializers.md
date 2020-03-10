---
layout: docs
title: Serialization formats
permalink: serializers/
---

# Serialization formats

Mu supports two serialization formats for messages when defining a gRPC end-point: Protocol Buffers and Avro. That is, the "outer layer" of your network packages uses gRPC, and the "inner data layer" has a choice between the aforementioned formats.

There are three places where this choice must be made visible:

1. When reading a service definition file,
2. When defining the conversion from Haskell types to schema types,
3. When starting a server or client.

## Protocol Buffers

In this case loading the service definition file looks as follows:

```haskell
{-# language TemplateHaskell #-}
import Mu.Quasi.GRpc

grpc "Schema" id "defn.proto"
```

This asks the compiler to load the `defn.proto` file in order to create a series of (Haskell) types. The first one, which is called `"Schema"`, as the first argument shows, includes the definition of all the schema types. Then, for every service defined in the file (since `.proto` files may have more than one), the second function is applied to obtain the name of the corresponding Haskell type. In this case we want to use the same names, so we use the identity function.

```haskell
data PersonMsg
  = PersonMsg { name :: Text, age :: Int }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Schema "Person"
           , FromSchema Schema "Person" )
```

Protocol Buffers version 3 has complicated rules about when a field may or may not appear in the message. For example it is not possible to distinguish whether the empty string is to be transmitted, or that field is missing. In that case, deserialization from a Protocol Buffers message returns the default value. However, for references to other message we can spot the difference, so those references *must* be wrapped in a `Maybe`.

Finally, when starting a server or client you must provide `msgProtoBuf` as argument. For example, a server is started by running:

```haskell
runGRpcApp msgProtoBuf 8080 server
```

## Avro

In this case loading the service definition file looks as follows:

```haskell
{-# language TemplateHaskell #-}
import Mu.Quasi.Avro

avdl "Schema" "Service" "." "defn.avdl"
```

Each `.avdl` file defines just one protocol, so instead of a function like in the case of Protocol Buffers, the second argument is simply the name of the Haskell type to create. But you might have noticed that there's an additional argument, which in this case is `"."`. The reason for this argument is that `.avdl` files routinely *import* other files. This third argument indicates the *base directory* to search for those files.

The conversion is almost identical to Protocol Buffers too. In Avro fields are required by default, only those fields which are optional (usually specified as a union with `null`) must still use `Maybe`:

```haskell
data PersonMsg
  = PersonMsg { name :: Text, age :: Int }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Schema "Person"
           , FromSchema Schema "Person" )
```

Finally, when starting a server or client you must provide `msgAvro` as argument. For example, a server is started by running:

```haskell
runGRpcApp msgAvro 8080 server
```
