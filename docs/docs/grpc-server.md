---
layout: docs
title: gRPC servers
permalink: grpc/server/
---

# gRPC servers

Mu-Haskell defines a generic notion of service and server that implements it. This generic server can then be used by `mu-grpc-server`, to provide a concrete implementation using a specific wire format.

## Implementing the service

Let's get back to the example we used in the [generic RPC section]({% link docs/rpc.md %}). In order to implement the corresponding service, you have to define the behavior of each method by means of a *handler*. You can use Haskell types for your handlers, given that you had previously declared that they can be mapped back and forth the schema types using `ToSchema` and `FromSchema`. For example, the following is a handler for the `SayHello` method in `Greeter`:

```haskell
sayHello :: (MonadServer m) => HelloRequest -> m HelloResponse
sayHello (HelloRequest nm) = pure $ HelloResponse ("hi, " <> nm)
```

Notice the use of `MonadServer` in this case. This gives us the ability to:

* Run arbitrary `IO` actions by using `liftIO`,
* Return an error code by calling `serverError`.

Being polymorphic here allows us to run the same server in multiple back-ends. Furthermore, by enlarging the set of abilities required for our monad `m`, we can [integrate with other libraries]({% link docs/transformer.md %}), including logging and resource pools.

Since you can declare more than one method in a service, you need to join them into a `SingleServerT`. You do so by using `singleService` (since gRPC servers may only expose one), and a *tuple* of methods indexed by their name *in the gRPC definition*. In addition to the name of the service, `SingleServerT` has an additional parameter which records the types of the handlers. Since that list may become quite long, we can ask GHC to write it for us by using the `PartialTypeSignatures` extension and writing an underscore `_` in that position.

```haskell
{-# language PartialTypeSignatures #-}

quickstartServer :: (MonadServer m) => SingleServerT QuickstartService m _
quickstartServer = singleService (method @"SayHello" sayHello)
```


## Running the server with `mu-grpc`

The combination of the declaration of a service API and a corresponding implementation as a `Server` may be served directly using a concrete wire protocol. One example is gRPC, provided by our sibling library `mu-grpc`. The following line starts a server at port `8080`, using Protocol Buffers as serialization layer:

```haskell
main = runGRpcApp msgProtoBuf 8080 quickstartServer
```

# Streams

In the docs about [service definition]({% link docs/rpc.md %}) we had one single `SayHello` method which takes one value and produces one value. However, we can also declare methods which perform streaming, such as:

```protobuf
service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply) {}
  rpc SayManyHellos (stream HelloRequest) returns (stream HelloReply) {}
}
```

Adding this method to the service definition should be easy, we just need to use `ArgStream` and `RetStream` to declare that behavior (of course, this is done automatically if you import the service from a file):

```haskell
type QuickstartService
  = 'Service "Greeter"
      '[ 'Method "SayHello" ...
       , 'Method "SayManyHellos" '[]
        '[ 'ArgStream 'Nothing '[] ('FromSchema QuickstartSchema "HelloRequest")]
        ('RetStream ('FromSchema QuickstartSchema "HelloResponse")) ]
```

To define the implementation of this method we build upon the great [Conduit](https://github.com/snoyberg/conduit) library. Your input is now a producer of values, as defined by that library, and you must write the results to the provided sink. Better said with an example:

```haskell
sayManyHellos
  :: (MonadServer m)
  => ConduitT () HelloRequest m ()
  -> ConduitT HelloResponse Void m ()
  -> m ()
sayManyHellos source sink
  = runConduit $ source .| C.mapM sayHello .| sink
```

In this case we are connecting the `source` to the `sink`, transforming in between each value using the `sayHello` function. More complicated pipelines can be built in this form.

Since now the service has more than one method, we need to update our server declaration to bring together this new handler:

```haskell
quickstartServer = singleService ( method @"SayHello" sayHello
                                 , method @"SayManyHellos" sayManyHellos )
```
