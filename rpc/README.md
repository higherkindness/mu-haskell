# `mu-rpc`: protocol-independent declaration of services and servers

There are several formats in the wild used to declare service APIs, including [Avro IDL](https://avro.apache.org/docs/current/idl.html), [gRPC](https://grpc.io/), and [OpenAPI](https://swagger.io/specification/). `mu-rpc` abstract the commonalities into a single type-level format for declaring these services, building on the format-independent schema facilities of `mu-schema`.

In addition, this package provides a generic notion of *server* of a service. One such server defines one behavior for each method in the service, but does not bother with (de)serialization mechanisms. This generic server can then be used by other packages, such as `mu-grpc`, to provide a concrete implementation using a specific wire format.

## Defining the schema and the service

Let us begin with an example taken from the [gRPC Quickstart Guide](https://grpc.io/docs/quickstart/):

```java
package helloworld;

service Greeter {
  rpc SayHello (HelloRequest) returns (HelloReply) {}
}

message HelloRequest { string name = 1; }
message HelloReply { string message = 1; }
```

The first step is to declare the schema for the message types. We do so using our sibling library `mu-schema`.

```haskell
{-# language PolyKinds, DataKinds #-}
{-# language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# language DeriveGeneric, DeriveAnyClass #-}

import qualified Data.Text as T
import GHC.Generics
import Mu.Schema

type QuickstartSchema
  = '[ 'DRecord "HelloRequest"  '[]
                '[ 'FieldDef "name"    '[ ProtoBufId 1 ] ('TPrimitive T.Text) ]
     , 'DRecord "HelloResponse" '[]
                '[ 'FieldDef "message" '[ ProtoBufId 1 ] ('TPrimitive T.Text) ]
     ]

newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloRequest")
newtype HelloResponse = HelloResponse { message :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloResponse")
```

The service declaration looks very similar to an schema declaration, but instead of record and enumerations you define *methods*. Each method has a name, a list of arguments, and a return type.

```haskell
import Mu.Rpc

type QuickStartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle ('FromSchema QuickstartSchema "HelloRequest") ]
                 ('RetSingle ('FromSchema QuickstartSchema "HelloResponse")) ]
```

In order to support both [Avro IDL](https://avro.apache.org/docs/current/idl.html) and [gRPC](https://grpc.io/), the declaration of the method arguments and returns in a bit fancier that you might expect:

* Each *argument* declares the schema type used for serialization. Furthermore, the argument can be declared as `ArgSingle` (only one value is provided by the client) or `ArgStream` (a stream of values is provided).
* The *return types* gives the same two choices under the names `RetSingle` or `RetStream`, and additionally supports the declaration of methods which may raise exceptions using `RetThrows`.

Note that depending on the concrete implementation you use to run the server, one or more of these choices may not be available. For example, gRPC only supports one argument and return value, either single or streaming, but not exceptions.

## Implementing the service

In order to implement the service, you have to define the behavior of each method by means of a *handler*. You can use Haskell types for your handlers, given that you had previously declared that they can be mapped back and forth the schema types using `HasSchema`. For example, the following is a handler for the `SayHello` method in `Greeter`:

```haskell
sayHello :: HelloRequest -> IO HelloResponse
sayHello (HelloRequest nm) = return (HelloResponse ("hi, " <> nm))
```

Since you can declare more than once method in a service, you need to join then into a `Server`. You do so by using `(:<|>:)` between each handler and ending the sequence with `H0`. In addition to the name of the service, `Server` has an additional parameter which records the types of the handlers. Since that list may become quite long, we can ask GHC to write it for us by using the `PartialTypeSignatures` extension and writing an underscore `_` in that position. One final observation is that in the code below we are using `ServerIO`, which is an instance of `Server` which allows running `IO` operations.

```haskell
{-# language PartialTypeSignatures #-}

quickstartServer :: ServerIO QuickStartService _
quickstartServer = Server (sayHello :<|>: H0)
```

## Streaming methods

The `SayHello` method above has a straightforward signature: it takes one value and produces one value. However, we can also declare methods which perform streaming, such as:

```java
service Greeter {
  rpc SayManyHellos (stream HelloRequest) returns (stream HelloReply) {}
}
```

Adding this method to the service definition should be easy, we just need to use `ArgStream` and `RetStream` to declare that behavior:

```haskell
type QuickStartService
  = 'Service "Greeter"
      '[ 'Method "SayHello" ...
       , 'Method "SayManyHellos"
                 '[ 'ArgStream ('FromSchema QuickstartSchema "HelloRequest")]
                 ('RetStream ('FromSchema QuickstartSchema "HelloResponse")) ]
```

To define the implementation of this method we build upon the great [Conduit](https://github.com/snoyberg/conduit) library. Your input is now a producer of values, as defined by that library, and you must write the results to the provided sink. Better said with an example:

```haskell
sayManyHellos
  :: ConduitT () HelloRequest IO ()
  -> ConduitT HelloResponse Void IO ()
  -> IO ()
sayManyHellos source sink
  = runConduit $ source .| C.mapM sayHello .| sink
```

In this case we are connecting the `source` to the `sink`, transforming in between each value using the `sayHello` function. More complicated pipelines can be built in this form.

Since now the service has more than one method, we need to update our server declaration to bring together this new handler:

```haskell
quickstartServer = Server (sayHello :<|>: sayManyHellos :<|>: H0)
```

## Running the server with `mu-grpc`

The combination of the declaration of a service API and a corresponding implementation as a `Server` may may served directly using a concrete wire protocol. One example is gRPC, provided by our sibling library `mu-grpc`. The following line starts a server at port 8080, where the service can be found under the package name `helloworld`:

```haskell
main = runGRpcApp 8080 "helloworld" quickstartServer
```

## Using the Registry

In this example we have used `FromSchema` to declare a specific schema the arguments must adhere to. However, schemas evolve over time, and you might want to handle all those versions. To do so, you first need to register your schemas using `mu-rpc`'s registry:

```haskell
type instance Registry "helloworld"
  = '[ 2 ':-> QuickstartSchemaV2, 1 ':-> QuickstartSchema ]
```

Now you can use the name of the subject in the registry to accomodate for different schemas. In this case, apart from that name, we need to specify the *Haskell* type to use during (de)serialization, and the *version number* to use for serialization.

```haskell
type QuickStartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle ('FromRegistry "helloworld" HelloRequest 2) ]
                 ('RetSingle ('FromRegistry "helloworld" HelloResponse 1)) ]
```