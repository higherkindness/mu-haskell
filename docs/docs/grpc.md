---
layout: docs
title: Mu-Haskell
permalink: grpc/
---

# gRPC servers and clients

Mu-Haskell defines a generic notion of service and server that implements it. This generic server can then be used by `mu-grpc-server`, to provide a concrete implementation using a specific wire format. Or you can use `mu-grpc-client` to build a client.

## Running the server with `mu-grpc`

The combination of the declaration of a service API and a corresponding implementation as a `Server` may served directly using a concrete wire protocol. One example is gRPC, provided by our sibling library `mu-grpc`. The following line starts a server at port `8080`, where the service can be found under the package name `helloworld`:

```haskell
main = runGRpcApp 8080 "helloworld" quickstartServer
```

## Building a client

Right now there are two options for building clients: using records or with `TypeApplications`. To give a proper introduction to both options let's consider in detail an example client for the following services:

```protobuf
service Service {
  rpc getPerson (PersonRequest) returns (Person);
  rpc newPerson (Person) returns (PersonRequest);
  rpc allPeople (google.protobuf.Empty) returns (stream Person);
}
```

Regardless of the approach we decide to use, we can construct a basic CLI for the client this way:

```haskell
import System.Environment

main :: IO ()
main = do
  let config = grpcClientConfigSimple "127.0.0.1" 8080 False
  Right client <- setupGrpcClient' config
  args <- getArgs
  case args of
    ["watch"]       -> watching client
    ["get", idp]    -> get client idp
    ["add", nm, ag] -> add client nm ag
    _               -> putStrLn "unknown command"
```

Where `watch`, `get` and `add` are the only valid 3 commands that our CLI is going to accept and call each respective service.

### Using records

This option is a bit more verbose but it's also more explicit with the types and _"a bit more magic"_ than the one with `TypeApplications` (due to the use of Generics).

We need to define a new record type (hence the name) that declares the services our client is going to consume. Remember that the names of the record fields **must match** exactly the methods in the service:

```haskell
import GHC.Generics (Generic)
import Mu.GRpc.Client.Record

data Call = Call
  { getPerson :: MPersonRequest -> IO (GRpcReply MPerson)
  , newPerson :: MPerson -> IO (GRpcReply MPersonRequest)
  , allPeople :: IO (ConduitT () (GRpcReply MPerson) IO ())
  } deriving Generic
```

Note that we had to derive `Generic`. We also need to tweak a little bit our `main` function:

```diff
main :: IO ()
main = do
   let config = grpcClientConfigSimple "127.0.0.1" 1234 False
-  Right client <- setupGrpcClient' config
+  Right grpcClient <- setupGrpcClient' config
+  let client = buildService @Service "/grpc" grpcClient
   args <- getArgs
```

Instead of building our client directly, we need to call `buildService` (and enable `TypeApplications`) to create the actual gRPC client. If you don't like `TypeApplications`, it is also possible to use a combination of `ScopedTypeVariables` and something like `(Proxy :: Proxy Service)` to achieve the same result, but it's a bit more verbose and we _encourge you<sup>1</sup>_ to use `TypeApplications`  instead. 😉

That string (or `ByteString`) as a second argument to `buildService` corresponds to the route of the service.

After that, let's have a look at an example implementation of the three service calls:

```haskell
import Text.Read (readMaybe)

get :: Call -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ readMaybe idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  res <- getPerson client req
  putStrLn $ "GET: response was: " ++ show res
```

Notice the use of `readMaybe` to convert the strings to the appropiate type in a safe manner! 👆🏼

```haskell
add :: Call -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (Just $ T.pack nm) (readMaybe ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  res <- newPerson client p
  putStrLn $ "ADD: was creating successful? " ++ show res

watching :: Call -> IO ()
watching client = do
  replies <- allPeople client
  runConduit $ replies .| C.mapM_ print
```

### Using `TypeApplications`

With `TypeApplications` none of the above is needed, all you need to do is call `gRpcCall` with the appropiate service name as a type-level string, and the rest just _magically_ works! ✨

```haskell
import Mu.GRpc.Client.TyApps

get :: GrpcClient -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ readMaybe idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  response :: GRpcReply MPerson
    <- gRpcCall @Service @"getPerson" client req
  putStrLn $ "GET: response was: " ++ show response
```

Notice that the type signatures of our functions needed to change to receive the `GrpcClient` as an argument, instead of our custom record type.

```haskell
add :: GrpcClient -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (Just $ T.pack nm) (readMaybe ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  response :: GRpcReply MPersonRequest
    <- gRpcCall @Service @"newPerson" client p
  putStrLn $ "ADD: was creating successful? " ++ show response
```

We are being a bit more explicit with the types here (for example, `response :: GRpcReply MPersonRequest`) to help a bit the `show` function because GHC is not able to infer the type on its own.

```haskell
watching :: GrpcClient -> IO ()
watching client = do
  replies <- gRpcCall @Service @"allPeople" client
  runConduit $ replies .| C.mapM_ (print :: GRpcReply MPerson -> IO ())
```

Here though, while mapping `print` to the `Conduit`, we needed to add a type annotation because the type was ambiguous... I think it's a small price to pay in exchange for the terseness. 🤑

---

<sup>1</sup> To read more on `TypeApplications`, you can check [this](https://www.reddit.com/r/haskell/comments/6ufnmr/scrap_your_proxy_arguments_with_typeapplications/), [that](https://blog.sumtypeofway.com/posts/fluent-polymorphism-type-applications.html) and [this](https://kseo.github.io/posts/2017-01-08-visible-type-application-ghc8.html).

To see a **working example** you can check all the code at the [example with persistent](https://github.com/higherkindness/mu-haskell/tree/master/examples/with-persistent).
