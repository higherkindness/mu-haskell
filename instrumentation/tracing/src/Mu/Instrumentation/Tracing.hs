{-# language OverloadedStrings #-}
{-# language ViewPatterns      #-}
{-|
Description : Distributed tracing for Mu

This module injects distributed tracing
for Mu servers. Currently it only supports
Zipkin as backend.

In order to use this module, you need to
follow these steps:

1. Establish a connection with 'newZipkin'.
2. Wrap the server using 'zipkin', giving
   information for the root.
3. Run the server using the transformer version
   of your protocol, like |grpcAppTrans|.
-}
module Mu.Instrumentation.Tracing (
  -- * Distributed tracing
  MuTracing(..)
, zipkin
, runZipkin
  -- ** Establish connection
, newZipkin
, Settings(..)
  -- * Useful re-exports
, module Monitor.Tracing
) where

import           Control.Applicative       ((<|>))
import           Control.Monad.IO.Class
import           Control.Monad.Trace
import           Control.Monad.Trace.Class
import qualified Data.Map.Strict           as M
import           Data.Text
import           Monitor.Tracing
import           Monitor.Tracing.Zipkin
import           Mu.Rpc
import           Mu.Server

data MuTracing
  = MuTracing {
      samplingPolicy :: SamplingPolicy
    , rootName       :: Text
    }

-- | Runs with a given 'Zipkin' connection.
--   You can create one with 'newZipkin'.
runZipkin :: Zipkin -> TraceT m a -> m a
runZipkin = flip run

-- | Create a new connection to 'Zipkin'.
newZipkin :: Settings -> IO Zipkin
newZipkin = new

-- | Wraps a server to do distributed tracing
--   using 'Zipkin' as backend.
zipkin :: (MonadIO m, MonadTrace m)
       => MuTracing -> ServerT chn i p m topHs -> ServerT chn i p m topHs
zipkin m = wrapServer (zipkinTracing m)

zipkinTracing :: (MonadIO m, MonadTrace m)
              => MuTracing -> RpcInfo i -> m a -> m a
zipkinTracing zpk NoRpcInfo h =
  rootSpan (samplingPolicy zpk) (rootName zpk) h
zipkinTracing zpk (RpcInfo _ _ _ (M.fromList -> hdrs) _) h =
  case getB3 of
    Nothing  -> rootSpan (samplingPolicy zpk) (rootName zpk) h
    Just spn -> serverSpan spn h
  where getB3 =   (b3FromHeaderValue =<< M.lookup "b3" hdrs)
              <|> b3FromHeaders hdrs
