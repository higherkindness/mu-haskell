---
layout: docs
title: Registry
permalink: registry/
---

# Registry

Schemas evolve over time. It is a good practice to keep an inventory of all the schemas you can work with, in the form of a *registry*. Using `mu-schema` you can declare one such registry as simply a mapping from versions to schemas:

```haskell
type ExampleRegistry
  = '[ 2 ':-> ExampleSchemaV2, 1 ':-> ExampleSchema ]
```

Once we have done that you can use functions like `fromRegistry` to try to parse a term into a Haskell type by trying each of the schemas.

## Using the Registry

By default, [service definition]({% link docs/rpc.md %}) talks about concrete schemas and types. If you define a registry, you can also use it to accomodate different schemas. In this case, apart from the registry itself, we need to specify the *Haskell* type to use during (de)serialization, and the *version number* to use for serialization.

```haskell
type QuickstartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle ('FromRegistry ExampleRegistry HelloRequest 2) ]
                 ('RetSingle ('FromRegistry ExampleRegistry HelloResponse 1)) ]
```
