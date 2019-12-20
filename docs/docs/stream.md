---
layout: docs
title: Mu-Haskell
permalink: stream/
---

# Streams

In the docs about [service definition](rpc.md) we had one single `SayHello` method which takes one value and produces one value. However, we can also declare methods which perform streaming, such as:

```java
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
       , 'Method "SayManyHellos"
        '[ 'ArgStream ('FromSchema QuickstartSchema "HelloRequest")]
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
quickstartServer = Server (sayHello :<|>: sayManyHellos :<|>: H0)
```
