---
layout: docs
title: Transformers
permalink: transformer/
---

# Integration using transformers

You might be wondering: how can I integrate my favorite logging library with `mu-grpc-server`? Our [explanation of services](rpc.md) introduced `MonadServer` as the simplest set of capabilities required for a server:

* Finish successfully by `return`ing,
* Finish with an error code via `serverError`,
* Executing arbitrary `IO` actions via `liftIO`.

But you are not tied to that simple set! You can create servers which need more capabilities if you later define how to run those.

## Reader

One simple example of a capability is having one single piece of information you can access. This is useful to thread configuration data, or if you use a transactional variable as information, as a way to share data between concurrent threads. This is traditionally done using a `Reader` monad.

Let us extend our [`sayHello` example](rpc.md) with a piece of configuration which states the word to use when greeting:

```haskell
import Control.Monad.Reader

sayHello :: (MonadServer m, MonadReader T.Text m)
         => HelloRequest -> m HelloResponse
sayHello (HelloRequest nm) = do
  greeting <- ask
  return (HelloResponse (greeting <> ", " <> nm))
```

Unfortunately, the simple way to run a gRPC application no longer works:

```haskell
main = runGRpcApp 8080 "helloworld" quickstartServer
```

Furthermore, how does the server know which is the actual value? In other words, how do we inject the value for `greeting`? We need to declare how to *handle* that capability. This is traditionally done with a `run` function; this additional argument is used by `runGRpcAppTrans`.

```haskell
main = runGRpcAppTrans 8080 (flip runReaderT "hi") quickstartServer
```

## Logging

There are quite a number of libraries which provide logging support. Let's begin with [`monad-logger`](https://github.com/snoyberg/monad-logger#readme). In this case, an additional [set of functions](http://hackage.haskell.org/package/monad-logger/docs/Control-Monad-Logger.html#g:8) is available when you implement the `MonadLogger` class. For example, we could log a message every time we say hi:

```haskell
import Control.Monad.Logger

sayHello :: (MonadServer m, MonadLogger m)
         => HelloRequest -> m HelloResponse
sayHello (HelloRequest nm) = do
  logInfoN "running hi"
  return (HelloResponse ("hi, " <> nm))
```

The most important addition with respect to the [original code](rpc.md) is in the signature. Before we only had `MonadServer m`, now we have an additional `MonadLogger m` there.

As we have done with the Reader example, we need to define how to handle `MonadLogger`. `monad-logger` provides [three different monad transformers](http://hackage.haskell.org/package/monad-logger-0.3.31/docs/Control-Monad-Logger.html#g:3), so you can choose whether your logging will be completely ignored, will become a Haskell value, or would fire some `IO` action like printing in the console. Each of these monad transformers comes with a `run` action which declares how to handle it; the extended function `runGRpcAppTrans` takes that handler as argument.

```haskell
main = runGRpcAppTrans 8080 runStderrLoggingT quickstartServer
```

If you prefer other logging library, this is fine with us! Replacing `monad-logger` by [`co-log`](https://github.com/kowainik/co-log) means asking for a different capability in the server. In this case we have to declare the type of the log items as part of the `WithLog` constraint:

```haskell
import Colog.Monad

sayHello :: (MonadServer m, WithLog env String m)
         => HelloRequest -> m HelloResponse
sayHello (HelloRequest nm) = do
  logInfoN "running hi"
  return (HelloResponse ("hi, " <> nm))
```

In this case, the top-level handler is called [`usingLoggerT`](http://hackage.haskell.org/package/co-log/docs/Colog-Monad.html#v:usingLoggerT). Its definition is slightly more involved because `co-log` gives you maximum customization power on your logging, instead of defining a set of predefined logging mechanisms.

```haskell
main = runGRpcAppTrans 8080 logger quickstartServer
  where logger = usingLoggerT (LogAction $ liftIO putStrLn)
```

## Warning

The `run` function you provide to `runGRpcAppTrans` may be called more than once! This is fine for readers and logging, but not for `StateT`, for example. In particular, you must ensure that your `run` function is *idempotent*, that is, that the result of calling it more than once is the same as calling it just once.

In the particular case of `StateT`, we suggest using a [transactional variable](http://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TVar.html), passed as either an argument or using `ReaderT`. This has the additional benefit that concurrent access to the variable - which is fairly possible in a gRPC server -- are automatically protected for data races and deadlocks.
