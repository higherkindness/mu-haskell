{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language PolyKinds           #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
module Mu.Instrumentation.Prometheus (
  initPrometheus
, prometheus
, prometheusWai
) where

import           Control.Concurrent.MVar.Lifted
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Data.Text                         (Text)
import           Mu.Rpc
import           Mu.Server
import           Network.Wai
import qualified Network.Wai.Middleware.Prometheus as Wai
import           Prometheus

-- Taken from https://github.com/higherkindness/mu-scala/blob/master/modules/metrics/prometheus/src/main/scala/higherkindness/mu/rpc/prometheus/PrometheusMetrics.scala

data MuMetrics
  = MuMetrics {
      activeCalls      :: Gauge
    , messagesSent     :: Vector Label2 Counter
    , messagesReceived :: Vector Label2 Counter
    , callsTotal       :: Vector Label2 Histogram
    }

initPrometheus :: Text -> IO MuMetrics
initPrometheus prefix =
  MuMetrics <$> register (gauge $ Info (prefix <> "_active_calls") "")
            <*> register (vector ("service", "method")
                         $ counter $ Info (prefix <> "_messages_sent") "")
            <*> register (vector ("service", "method")
                         $ counter $ Info (prefix <> "_messages_received") "")
            <*> register (vector ("service", "method")
                         $ histogram (Info (prefix <> "_calls_total") "")
                                     defaultBuckets)

prometheus :: (MonadBaseControl IO m, MonadMonitor m)
           => MuMetrics -> ServerT chn info p m topHs -> ServerT chn info p m topHs
prometheus m = wrapServer (prometheusMetrics m)

prometheusMetrics :: forall m a info. (MonadBaseControl IO m, MonadMonitor m)
                  => MuMetrics -> RpcInfo info -> m a -> m a
prometheusMetrics metrics NoRpcInfo run = do
  incGauge (activeCalls metrics)
  run `finally` decGauge (activeCalls metrics)
prometheusMetrics metrics (RpcInfo _pkg (Service sname _) (Method mname _ _) _ _) run = do
  incGauge (activeCalls metrics)
  withLabel (messagesReceived metrics) (sname, mname) incCounter
  ( do -- We are forced to use a MVar because 'withLabel' only allows IO ()
       r <- liftBaseWith $ \runInIO -> do
         result :: MVar (StM m a) <- newEmptyMVar
         withLabel (callsTotal metrics) (sname, mname) $ \h ->
           h `observeDuration` (runInIO run >>= putMVar result)
         takeMVar result
       x <- restoreM r
       withLabel (messagesSent metrics) (sname, mname) incCounter
       pure x )
  `finally` decGauge (activeCalls metrics)

prometheusWai :: [Text] -> Middleware
prometheusWai endpoint
  = Wai.prometheus (Wai.PrometheusSettings endpoint False False)
