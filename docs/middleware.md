# Integration with WAI middleware

Although you usually run a `mu-rpc` server directly using a function like `runGRpcApp`, this is just a convenience function to make it simpler to run the server. Under the hood, the library generates a so-called WAI application, which is then fed to an actual server.

WAI stands for [*Web Application Interface*](https://www.yesodweb.com/book/web-application-interface). WAI defines a common set of APIs against which web application can be developed. Web servers, like [Warp](http://www.aosabook.org/en/posa/warp.html), take an application which uses that API and serves it against the actual network. The main benefit of this separation is that applications and servers can evolve separately, without a tight coupling between them.

One of the features that WAI provides is the definition of *middleware* components. *Middlewares* are not complete applications; instead they wrap another application to provide additional capabilities. There is a whole ecosystem of WAI middleware, as one can notice by [searching `wai middleware` in Hackage](http://hackage.haskell.org/packages/search?terms=wai+middleware).

## Serving static files

It's a common task in web servers to send some static content for a subset of URLs (think of resources such as images or JavaScript code). `wai-middleware-static` automates that task, and also serves as the simplest example of WAI middleware.

Remember that in our [original code](intro.md) our `main` function looked as follows:

```haskell
main = runGRpcApp 8080 server
```

We can split this single line into two phases: first creating the WAI application by means of `gRpcApp`, and then running it using Warp's `run` function.

```haskell
import Network.Wai.Handler.Warp (run)

main = run 8080 $ gRpcApp server
```

Right in the middle of the two steps we can inject any middleware we want.

```haskell
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static

main = run 8080 $ static (gRpcApp server)
```

With that simple change, our web server now first checks whether a file with the requested name exists in the directory from which the application is running. If that is the case, it's cached and served, otherwise the underlying gRPC application is run. Needless to say, this behavior might not be the desired one, so the library provides [`staticPolicy`](http://hackage.haskell.org/package/wai-middleware-static/docs/Network-Wai-Middleware-Static.html#v:staticPolicy) to customize it.

## Metrics

Another interesting use of middleware is obtaining metrics for your application. Within the Haskell world, [EKG](https://github.com/tibbe/ekg) is a common solution for monitoring any kind of running process. EKG provides [customizable counters](https://ocharles.org.uk/blog/posts/2012-12-11-24-day-of-hackage-ekg.html), so you are not tied to one specific set of variables. This is the idea behind the [`wai-middleware-metrics`](https://github.com/Helkafen/wai-middleware-metrics) package: provide counters for the specific needs of HTTP servers.

```haskell
import System.Remote.Monitoring (serverMetricStore, forkServer)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics

main = do
  -- Taken from the official documentation
  store <- serverMetricStore <$> forkServer "localhost" 8000
  waiMetrics <- registerWaiMetrics store
  -- Wrap the application and run it
  run 8080 $ metrics waiMetrics (gRpcApp server)
```

Another possibility is to expose [Prometheus](https://prometheus.io/) metrics. In this case, a specific monitoring process scrapes your servers from time to time. The gathered data can later be analyzed and visualized. Luckily, there's some [middleware](https://github.com/fimad/prometheus-haskell) exposing the require endpoints automatically.

```haskell
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Prometheus

main = run 8080 $ prometheus def (gRpcApp server)
```

The usage of `def` indicates that we want the default options: providing the metrics under the route `/metrics`.

## Compression, header manipulation, and more!

These docs only scrape the surface of WAI middleware. We encourage you to check the [`wai-extra` package](http://hackage.haskell.org/package/wai-extra), which includes many self-contained middlewares. For example, you may wish to have GZip compression, or to canonicalize routes before passing them to the actual application. Following the Haskell philosophy, the idea is to provide small components which can be easily composed to target your needs.
