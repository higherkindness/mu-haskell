# Mu for Haskell

This repo defines a set of libraries which implement a similar functionality to [Mu for Scala](http://higherkindness.io/mu/), but in Haskell.

* `mu-schema` defines schemas for messages and conversion from and to Avro, Protocol Buffers, and JSON.
* `mu-rpc` defines schemas for service APIs, and the notion of a server for one such API.

## Building

This set of libraries are thought to be built using [Stack](https://docs.haskellstack.org). Just jump into the folder and run `stack build`! The top-level `stack.yaml` defines a common resolver and set of dependencies for all the packages.