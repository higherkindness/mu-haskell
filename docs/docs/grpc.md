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

### Using records

### Using `TypeApplications`
