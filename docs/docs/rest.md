---
layout: docs
title: OpenAPI / REST services
permalink: openapi/
---

# OpenAPI / REST services

In order to expose a Mu server using a OpenAPI or REST interface, we make use of the awesome [Servant](https://docs.servant.dev/en/stable/) library. Both libraries describe the API of a server at the type level, use the notion of _handlers_, and follow a similar structure.

The `mu-servant-server` package contains a function `servantServerHandlers` which unpacks the Mu handlers and repackages them as Servant handlers, with some minor changes to support streaming. The trickiest part, however, is translating the Mu server _type_ into a Servant server _type_.

## Annotating the server

When Mu methods are converted to Servant APIs, you may customize certain aspects of the resulting API, including the route, HTTP method, and HTTP status.  Additionally, you must specify which content types use be used when encoding and decoding each type in your schema that appears in your methods. All of this customization is done with annotations, via the `AnnotatedSchema` and `AnnotatedPackage` type families.

For the server we have developed in the [generic RPC section]({% link docs/rpc.md %}), the instances for the services look as follows:

```haskell
type instance AnnotatedPackage ServantRoute QuickstartService
  = '[ 'AnnService "Greeter" ('ServantTopLevelRoute '["greet"])
     , 'AnnMethod "Greeter" "SayHello"
                   ('ServantRoute '["say", "hello"] 'POST 200),
     ]
```

The first annotation defines that the whole service lives in the `/greet` route. Each method then gets its own route and HTTP verb. To execute `SayHello`, one has to make a `POST` request to `/greet/say/hello`. The last element is the HTTP status code to be returned by default, in this case `200` which means success.

You also need to define how message types can be serialized in the API. This will be translated to a `ReqBody` in the corresponding Servant API, which requires a list of acceptable content types for the request. We provide a `DefaultServantContentTypes` which uses JSON for both unary and streaming calls.

```haskell
type instance
  AnnotatedSchema ServantContentTypes QuickstartSchema =
    '[ 'AnnType "HelloRequest"  DefaultServantContentTypes,
       'AnnType "HelloResponse" DefaultServantContentTypes
     ]
```

The `MimeRender`/`MimeUnrender` instances necessary to perform this encoding/decoding must exist for the Haskell type you use to represent messages. In this case, that means that both types must support conversion to JSON, which can be achieved using `mu-schema` in combination with `DerivingVia`.

```haskell
{-# language DerivingVia #-}

import qualified Data.Aeson as J
import Mu.Adapter.Json ()

newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving ( Show, Eq, Generic
           , ToSchema   QuickstartSchema "HelloRequest"
           , FromSchema QuickstartSchema "HelloRequest" )
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema QuickstartSchema "HelloRequest" HelloRequest)
```


If you forget to provide one of these required instances, you will see a message like the following:

```
    • Missing required AnnotatedPackage ServantRoute type instance
      for "myschema" package
    • When checking the inferred type
```

followed by a large and difficult to read type representing several stuck type families.  This message is an indication that you must provide an `AnnotatedPackage` type instance, with a domain of `ServantRoute` for the package with the name `myschema`.

## Exposing the server

You are now ready to expose your server using Servant!

```haskell
import Mu.Servant.Server
import Servant.Server

main =
  let api    = packageAPI (quickstartServer @ServerErrorIO)
      server = servantServerHandlers toHandler quickstartServer
  in run 8081 (serve api server)
```

The last line uses functions from Servant and Warp to run the server. The `serve` function has two parameters:
- One is the definition of the API, which can be obtained using the provided `packageAPI` with your server. In this case we had to make explicit the monad we are operating to avoid an ambiguity error.
- The other is the set of Servant handlers, which can be obtained by using `servantServerHandlers toHandler`.

## Integration with Swagger UI

You can easily expose not only the server itself, but also its [Swagger / OpenAPI](https://swagger.io/) schema easily, alongside a [Swagger UI](https://swagger.io/tools/swagger-ui/) for testing purposes. Here we make use of the awesome [`servant-swagger-ui` package](https://github.com/haskell-servant/servant-swagger-ui).

First of all, you need to specify that you want an additional component in your Servant API. You do so in the annotation:

```haskell
type instance AnnotatedPackage ServantRoute QuickstartService
  = '[ 'AnnPackage ('ServantAdditional (SwaggerSchemaUI "swagger-ui" "swagger.json"))
     , {- rest of annotations -} ]
```

The implementation of this additional component is given by using `servantServerHandlersExtra`, instead of its "non-extra" version. The aforementioned package is ready for consumption in that way:

```haskell
import Mu.Servant.Server
import Servant.Server
import Servant.Swagger.UI

main =
  let svc    = quickstartServer @ServerErrorIO
      api    = packageAPI svc
      server = servantServerHandlersExtra
                (swaggerSchemaUIServer (swagger svc))
                toHandler svc
  in run 8081 (serve api server)
```

And that's all! When you users surf to `yourserver/swagger-ui` they'll see a color- and featureful explanation of the endpoints of your server.

## Type translation

> This is not required for using `mu-servant-server`, but may help you understanding how it works under the hood and diagnosing problems.

There are essentially four categories of `Method` types and each of these is translated slightly differently.

### Full unary

Full unary methods have non-streaming arguments and a non-streaming response. Most HTTP endpoints expect unary requests and return unary responses. Unary method handlers look like this

```haskell
(MonadServer m) => requestType -> m responseType
```

For a handler like this, the corresponding "Servant" API type would be

```haskell
type MyUnaryAPI =
  route :>
    ReqBody ctypes1 requestType :>
      Verb method status ctypes2 responseType
```

As you can see, the request body contains a `requestType` value, and the response body contains a `responseType` value. All other types are derived from Mu annotations.

### Server streaming

Server streaming methods have non-streaming arguments, but the response is streamed back to the client. Server stream handlers look like this

```haskell
(MonadServer m) => requestType -> ConduitT responseType Void m () -> m ()
```

For a handler like this, the corresponding Servant API type would be

```haskell
type MyServerStreamAPI =
  route :>
    ReqBody ctypes requestType :>
      Stream method status framing ctype (SourceIO (StreamResult responseType))
```

The request body contains a `requestType` value. The response body is a stream of `StreamResult` responseType@ values. `StreamResult responseType` contains either a `responseType` value or an error message describing a problem that occurred while producing `responseType` values. All other types are derived from Mu annotations.

### Client streaming

Client streaming methods have a streaming argument, but the response is unary. Client stream handlers look like this

```haskell
(MonadServer m) => ConduitT () requestType m () -> m responseType
```

For a handler like this, the corresponding Servant API type would be

```haskell
type MyClientStreamAPI =
  route :>
    StreamBody framing ctype (SourceIO requestType) :>
      Verb method status ctypes responseType
```

### Bidirectional streaming

Bidirectional streaming method have a streaming argument and a streaming response. Bidirectional stream handlers look like this

```haskell
> (MonadServer m) => ConduitT () requestType m () -> ConduitT responseType Void m () -> m()
```

For a handler like this, the corresponding Servant API type would be

```haskell
type MyBidirectionalStreamAPI =
  StreamBody framing1 ctype1 (SourceIO requestType) :>
    Stream method status framing2 ctype2 (SourceIO (StreamResult responseType))
```

This type should look familiar if you already looked at the server streaming and client streaming examples. The request body is a stream of `requestType` values, and the response body is a stream of `StreamResult responseType` values. All the other types involved are derived from Mu annotations.

