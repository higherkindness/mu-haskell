# Docs for Mu-Haskell

Mu-Haskell is a set of packages that help you build both servers and clients for (micro)services. The main goal of Mu-Haskell is to make you focus on your domain logic, instead of worrying about format and protocol issues.

* [Introduction](intro.md)
* [Schemas](schema.md)
  * [Registry](registry.md)
* [Services and servers](rpc.md)
  * [gRPC servers and clients](grpc.md)
  * [Streams](stream.md)
  * [Databases](db.md), including resource pools
* Integration with other libraries
  * [Using transformers](transformer.md): look here for logging
  * [WAI Middleware](middleware.md): look here for metrics
