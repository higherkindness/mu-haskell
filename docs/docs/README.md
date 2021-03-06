---
layout: docs
title: Docs
permalink: /
---

# Docs for Mu-Haskell

Mu-Haskell is a set of packages that help you build both servers and clients for (micro)services. The main goal of Mu-Haskell is to allow you to focus on your domain logic, instead of worrying about format and protocol issues.

If you prefer listening to reading, we have a bunch of [talks]({% link docs/talks.md %}) covering both the usage and the internals of the library.

* Introduction
  * [For RPC]({% link docs/intro-rpc.md %})
  * [For GraphQL]({% link docs/intro-graphql.md %})
* [Schemas]({% link docs/schema.md %})
  * [Serialization formats]({% link docs/serializers.md %}): Protocol Buffers and Avro
  * [Registry]({% link docs/registry.md %})
  * [Optics]({% link docs/optics.md %})
* [Services]({% link docs/rpc.md %})
  * [gRPC server]({% link docs/grpc-server.md %})
  * [gRPC client]({% link docs/grpc-client.md %})
  * [GraphQL]({% link docs/graphql.md %})
  * [OpenAPI / REST]({% link docs/rest.md %})
* Integration with other libraries
  * [Databases]({% link docs/db.md %}), including resource pools
  * [Using transformers]({% link docs/transformer.md %}): look here for logging
  * [WAI Middleware]({% link docs/middleware.md %}): look here for metrics
