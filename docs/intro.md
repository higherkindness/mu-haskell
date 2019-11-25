# Introduction to Mu-Haskell

* [`mu-schema`](https://github.com/higherkindness/mu-haskell/tree/master/schema) defines schemas for messages and conversion from and to Avro, Protocol Buffers, and JSON.
* [`mu-rpc`](https://github.com/higherkindness/mu-haskell/tree/master/rpc) defines schemas for service APIs, and the notion of a server for one such API.
* [`mu-grpc`](https://github.com/higherkindness/mu-haskell/tree/master/grpc) serves a `mu-rpc` server using gRPC.

Each library contains a brief tutorial on how to use it. But if you want to see some examples, here they are:

* [Haskell definition](https://github.com/higherkindness/mu-haskell/blob/master/schema/src/Mu/Schema/Examples.hs) of schemas corresponding to this [Avro](https://github.com/higherkindness/mu-haskell/blob/master/schema/test/avro/example.avsc) and [Protocol Buffers](https://github.com/higherkindness/mu-haskell/blob/master/schema/test/protobuf/example.proto) files.
* [Haskell definition and implementation](https://github.com/higherkindness/mu-haskell/blob/master/rpc/src/Mu/Rpc/Examples.hs) of a server corresponding to this [gRPC API](https://github.com/higherkindness/mu-haskell/blob/master/grpc/test/helloworld.proto).
