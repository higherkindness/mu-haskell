---
layout: docs
title: gRPC clients
permalink: grpc/client/
---

# gRPC clients

There are several options for building clients: you can choose between optics, records, and `TypeApplications`. To give a proper introduction to both options let's consider in detail an example client for the following services:

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
  Right client <- setup config
  args <- getArgs
  case args of
    ["watch"]       -> watching client
    ["get", idp]    -> get client idp
    ["add", nm, ag] -> add client nm ag
    _               -> putStrLn "unknown command"
```

Where `watch`, `get` and `add` are the only valid 3 commands that our CLI is going to accept and call each respective service. The `setup` function is responsible from initializing the

### Using optics

The simplest way to call methods is to use the `optics`-based API. In that case, your setup is done using `initGRpc`, which receives the configuration.

```haskell
main :: IO ()
main = do ...
  where setup config = initGRpc config msgProtoBuf
```

To call a method, you use the corresponding getter (for those familiar with optics, a version of a lens which does not allow to set). This means that your code reads `client ^. #method`, where `client` is the value obtained previously in the call to `initGRpc`.

```haskell
{-# language OverloadedLabels #-}

get :: GRpcConnection QuickstartService -> String -> IO ()
get client idPerson = do
  let req = record1 (read idPerson)
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  res <- client ^. #getPerson $ req
  putStrLn $ "GET: response was: " ++ show res
```

Notice the use of `read` to convert the strings to the appropiate type. Be careful, though, since that function throws an exception if the string is not a proper number! In a realistic scenario you should use `readMaybe` from `Text.Read` and handle the appropiate cases.

Using this approach you must also use the optics-based interface to the terms. As a quick reminder: you use `record` to build new values, and use `value ^. #field` to access a field. The rest of the methods look as follows:

```haskell
add :: GRpcConnection QuickstartService -> String -> String -> IO ()
add client nm ag = do
  let p = record (Nothing, T.pack nm, read ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  res <- client ^. #newPerson $ p
  putStrLn $ "ADD: was creating successful? " ++ show res

watching :: GRpcConnection QuickstartService -> IO ()
watching client = do
  replies <- client ^. #allPeople
  runConduit $ replies .| C.mapM_ print
```

### Using records

This option is a bit more verbose but it's also more explicit with the types. Furthermore, it allows us to use our Haskell data types, we are not forced to use plain terms. As discussed several times, this is important to ensure that Haskell types are not mere shadows of the schema types.

We need to define a new record type (hence the name) that declares the services our client is going to consume. The names of the fields **must** match the names of the methods in the service, optionally prefixed by a **common** string. The prefix may also be empty, which means that the names in the record are exactly those in the service definition. In this case, we are prepending `call_` to each of them:

```haskell
import GHC.Generics (Generic)
import Mu.GRpc.Client.Record

data Call = Call
  { call_getPerson :: MPersonRequest -> IO (GRpcReply MPerson)
  , call_newPerson :: MPerson -> IO (GRpcReply MPersonRequest)
  , call_allPeople :: IO (ConduitT () (GRpcReply MPerson) IO ())
  } deriving Generic
```

Note that we had to derive `Generic`. We also need to tweak our `setup` function a little bit:

```haskell
{-# language TypeApplications #-}

main :: IO ()
main = do ...
  where setup config = buildService @Service @"call_" <$> setupGrpcClient' config
```

Instead of building our client directly, we need to call `buildService` (and enable `TypeApplications`) to create the actual gRPC client. There are two type arguments to be explicitly given: the first one is the `Service` definition we want a client for, and the second one is the prefix in the record (in our case, this is `call_`). In the case you want an empty prefix, you write `@""` in that second position.

After that, let's have a look at an example implementation of the three service calls. Almost as before, except that we use `call_` followed by the name of the method.

```haskell
get :: Call -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ read idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  res <- call_getPerson client req
  putStrLn $ "GET: response was: " ++ show res

add :: Call -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (T.pack nm) (read ag)
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  res <- call_newPerson client p
  putStrLn $ "ADD: was creating successful? " ++ show res

watching :: Call -> IO ()
watching client = do
  replies <- call_allPeople client
  runConduit $ replies .| C.mapM_ print
```

### Using `TypeApplications`

With `TypeApplications` none of the above is needed, all you need to do is call `gRpcCall` with the appropiate service name as a type-level string, and the rest just _magically_ works! ✨

If you are not familiar with `TypeApplications`, you can check [this](https://www.reddit.com/r/haskell/comments/6ufnmr/scrap_your_proxy_arguments_with_typeapplications/), [that](https://blog.sumtypeofway.com/posts/fluent-polymorphism-type-applications.html) and [this](https://kseo.github.io/posts/2017-01-08-visible-type-application-ghc8.html).


```haskell
import Mu.GRpc.Client.TyApps

main = do ...
  where setup config = setupGrpcClient' config

get :: GrpcClient -> String -> IO ()
get client idPerson = do
  let req = MPersonRequest $ read idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  response :: GRpcReply MPerson
    <- gRpcCall @Service @"getPerson" client req
  putStrLn $ "GET: response was: " ++ show response
```

Notice that the type signatures of our functions needed to change to receive the `GrpcClient` as an argument, instead of our custom record type.

```haskell
add :: GrpcClient -> String -> String -> IO ()
add client nm ag = do
  let p = MPerson Nothing (T.pack nm) (read ag)
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

To see a **working example** you can check all the code at the [example with persistent](https://github.com/higherkindness/mu-haskell/tree/master/examples/with-persistent).
