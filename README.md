# Mu for Haskell

This repo defines a set of libraries to write microservices in a format- and protocol-independent way. It shares the same goals as [Mu for Scala](http://higherkindness.io/mu/), but using idiomatic Haskell and more type-level techniques.

## Documentation

* [Introduction](intro.md)
* [Schemas](schema.md)
  * [Type-level schemas](tlschema.md)
  * [Registry](registry.md)
* [Services](rpc.md)
  * [gRPC](grpc.md)
  * [Streams](stream.md)
  * [WAI Middleware](middleware.md)

## Building

This set of libraries are thought to be built using [Stack](https://docs.haskellstack.org). Just jump into the folder and run `stack build`! The top-level `stack.yaml` defines a common resolver and set of dependencies for all the packages.

## Contributing

If you want to contribute, please be sure to read the [development guidelines](DEVELOPMENT.md) first.
