---
layout: docs
title: Docs
permalink: /
---

# Docs for Mu-Haskell

Mu-Haskell is a set of packages that help you build both servers and clients for (micro)services. The main goal of Mu-Haskell is to allow you to focus on your domain logic, instead of worrying about format and protocol issues.

* [Introduction]({% link docs/intro.md %})
* [Schemas]({% link docs/schema.md %})
  * [Serialization formats]({% link docs/serializers.md %}): Protocol Buffers and Avro
  * [Registry]({% link docs/registry.md %})
* [Services and servers]({% link docs/rpc.md %})
  * [gRPC servers and clients]({% link docs/grpc.md %})
  * [Streams]({% link docs/stream.md %})
  * [Databases]({% link docs/db.md %}), including resource pools
* Integration with other libraries
  * [Using transformers]({% link docs/transformer.md %}): look here for logging
  * [WAI Middleware]({% link docs/middleware.md %}): look here for metrics
