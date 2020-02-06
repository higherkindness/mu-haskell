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

When defining the conversion, it is important for `FromSchema` and `ToSchema` to receive `Maybe` as first argument:

```haskell
data PersonMsg
  = PersonMsg { name :: Maybe Text, age :: Maybe Int }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe Schema "Person"
           , FromSchema Maybe Schema "Person" )
```

This indicates that every field in the record should we wrapped by a `Maybe` layer. This is required because Protocol Buffers version 3 mandates all fields to be optional. This also means that every field in your Haskell types must be wrapped in that `Maybe` layer.

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

The conversion is almost identical to Protocol Buffers too, except that instead of `Maybe` you have to use `Identity`. This is because Avro makes fields *required* by default, so there's no need to have an additional `Maybe` layer. Of course, those fields which are optional (usually specified as a union with `null`) must still use `Maybe`:

```haskell
import Data.Functor.Identity

data PersonMsg
  = PersonMsg { name :: Text, age :: Int }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Identity Schema "Person"
           , FromSchema Identity Schema "Person" )
```

Finally, when starting a server or client you must provide `msgAvro` as argument. For example, a server is started by running:

```haskell
runGRpcApp msgAvro 8080 server
```
