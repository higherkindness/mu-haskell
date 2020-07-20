{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trace
import           Data.Conduit
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.TMChan
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy
import qualified Data.Text                     as T
import           DeferredFolds.UnfoldlM
import           Monitor.Tracing.Zipkin        (Endpoint (..))
import           Network.Wai.Handler.Warp
import           Prometheus
import qualified StmContainers.Map             as M

import           Mu.GraphQL.Server
import           Mu.GRpc.Server
import           Mu.Instrumentation.Prometheus
import           Mu.Instrumentation.Tracing
import           Mu.Server

import           Definition

main :: IO ()
main = do
  -- Initialize prometheus
  met <- initPrometheus "health"
  -- Initialize zipkin
  zpk <- newZipkin defaultZipkinSettings
           { settingsPublishPeriod = Just 1
           , settingsEndpoint = Just $ Endpoint (Just "me") Nothing Nothing Nothing }
  let rootInfo = MuTracing alwaysSampled "health-check"
  -- Initialize app
  m <- M.newIO
  upd <- newTBMChanIO 100
  -- Put together the server
  let s = zipkin rootInfo $ prometheus met $ server m upd
  -- Run the app
  putStrLn "running health check application"
  runConcurrently $ (\_ _ _ -> ())
    <$> Concurrently (runner 50051 (gRpcAppTrans msgProtoBuf (runZipkin zpk) s))
    <*> Concurrently (runner 50052 (gRpcAppTrans msgAvro     (runZipkin zpk) s))
    <*> Concurrently (runner 50053 (graphQLAppTransQuery (runZipkin zpk) s
                                      (Proxy @"HealthCheckServiceFS2")))
  where runner p app = run p (prometheusWai ["metrics"] app)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/handler/HealthServiceImpl.scala

type StatusMap = M.Map T.Text T.Text
type StatusUpdates = TBMChan HealthStatusMsg

server :: (MonadServer m, MonadTrace m)
       => StatusMap -> StatusUpdates
       -> ServerT '[] info HealthCheckService m _
server m upd
  = wrapServer (\info h -> liftIO (print info) >> h) $
    singleService ( method @"setStatus"   $ setStatus_ m upd
                  , method @"check"       $ checkH_ m
                  , method @"clearStatus" $ clearStatus_ m
                  , method @"checkAll"    $ checkAll_ m
                  , method @"cleanAll"    $ cleanAll_ m
                  , method @"watch"       $ watch_ upd)

setStatus_ :: (MonadServer m, MonadTrace m)
           => StatusMap -> StatusUpdates -> HealthStatusMsg
           -> m ()
setStatus_ m upd
           s@(HealthStatusMsg (Just (HealthCheckMsg nm)) (Just (ServerStatusMsg ss)))
  = childSpan "setStatus" $ alwaysOk $ do
      putStr "setStatus: " >> print (nm, ss)
      atomically $ do
        M.insert ss nm m
        writeTBMChan upd s
setStatus_ _ _ _ = serverError (ServerError Invalid "name or status missing")

checkH_ :: (MonadServer m, MonadTrace m)
        => StatusMap -> HealthCheckMsg
        -> m ServerStatusMsg
checkH_ _ (HealthCheckMsg "") = serverError (ServerError Invalid "no server name given")
checkH_ m (HealthCheckMsg nm) = alwaysOk $ do
  putStr "check: " >> print nm
  ss <- atomically $ M.lookup nm m
  pure $ ServerStatusMsg (fromMaybe "" ss)

clearStatus_ :: (MonadServer m, MonadTrace m)
             => StatusMap -> HealthCheckMsg -> m ()
clearStatus_ _ (HealthCheckMsg "") = serverError (ServerError Invalid "no server name given")
clearStatus_ m (HealthCheckMsg nm) = alwaysOk $ do
  putStr "clearStatus: " >> print nm
  atomically $ M.delete nm m

checkAll_ :: (MonadServer m, MonadTrace m)
          => StatusMap -> m AllStatusMsg
checkAll_ m = alwaysOk $ do
    putStrLn "checkAll"
    AllStatusMsg <$> atomically (consumeValues kvToStatus (M.unfoldlM m))
  where
    consumeValues :: Monad m => (k -> v -> a) -> UnfoldlM m (k,v) -> m [a]
    consumeValues f = foldlM' (\xs (x,y) -> pure (f x y:xs)) []
    kvToStatus k v = HealthStatusMsg (Just (HealthCheckMsg k)) (Just (ServerStatusMsg v))

cleanAll_ :: (MonadServer m, MonadTrace m)
          => StatusMap -> m ()
cleanAll_ m = alwaysOk $ do
  putStrLn "cleanAll"
  atomically $ M.reset m

watch_ :: (MonadServer m, MonadTrace m)
       => StatusUpdates
       -> HealthCheckMsg
       -> ConduitT ServerStatusMsg Void m ()
       -> m ()
watch_ upd hcm@(HealthCheckMsg nm) sink = do
  alwaysOk (putStr "watch: " >> print nm)
  runConduit $ sourceTBMChan upd
            .| C.filter (\(HealthStatusMsg c _) -> Just hcm == c)
            .| C.map (\(HealthStatusMsg _ s) -> s)
            .| catMaybesC
            .| sink
  where
    catMaybesC = do x <- await
                    case x of
                      Just (Just y) -> yield y >> catMaybesC
                      Just Nothing  -> catMaybesC
                      Nothing       -> pure ()

instance MonadMonitor m => MonadMonitor (TraceT m)
