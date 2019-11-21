{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
-- | Execute a Mu 'Server' using gRPC as transport layer
module Mu.GRpc.Server (
  -- * Run a 'Server' directly
  runGRpcApp
, runGRpcAppSettings, Settings
, runGRpcAppTLS, TLSSettings
  -- * Convert a 'Server' into a WAI application
, gRpcApp
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.IO.Class
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BS
import           Data.Conduit
import           Data.Conduit.TMChan
import           Data.Kind
import           Data.Proxy
import           Network.GRPC.HTTP2.Encoding   (gzip, uncompressed)
import           Network.GRPC.HTTP2.Proto3Wire
import           Network.GRPC.Server.Handlers
import           Network.GRPC.Server.Wai       (ServiceHandler)
import           Network.GRPC.Server.Wai       as Wai
import           Network.Wai                   (Application)
import           Network.Wai.Handler.Warp      (Port, Settings, run, runSettings)
import           Network.Wai.Handler.WarpTLS   (TLSSettings, runTLS)

import           Mu.Adapter.ProtoBuf.Via
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

-- | Run a Mu 'Server' on the given port.
runGRpcApp
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers methods handlers )
  => Port -> ServerIO ('Service name anns methods) handlers
  -> IO ()
runGRpcApp port svr = run port (gRpcApp svr)

-- | Run a Mu 'Server' using the given 'Settings'.
--
--   Go to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppSettings
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers methods handlers )
  => Settings -> ServerIO ('Service name anns methods) handlers
  -> IO ()
runGRpcAppSettings st svr = runSettings st (gRpcApp svr)

-- | Run a Mu 'Server' using the given 'TLSSettings' and 'Settings'.
--
--   Go to 'Network.Wai.Handler.WarpTLS' to declare 'TLSSettings'
--   and to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppTLS
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers methods handlers )
  => TLSSettings -> Settings
  -> ServerIO ('Service name anns methods) handlers
  -> IO ()
runGRpcAppTLS tls st svr = runTLS tls st (gRpcApp svr)

-- | Turn a Mu 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
gRpcApp
  :: (KnownName name, KnownName (FindPackageName anns), GRpcMethodHandlers methods handlers)
  => ServerIO ('Service name anns methods) handlers
  -> Application
gRpcApp svr = Wai.grpcApp [uncompressed, gzip]
                          (gRpcServiceHandlers svr)

gRpcServiceHandlers
  :: forall name anns methods handlers.
     (KnownName name, KnownName (FindPackageName anns), GRpcMethodHandlers methods handlers)
  => ServerIO ('Service name anns methods) handlers
  -> [ServiceHandler]
gRpcServiceHandlers (Server svr) = gRpcMethodHandlers packageName serviceName svr
  where packageName = BS.pack (nameVal (Proxy @(FindPackageName anns)))
        serviceName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandlers (ms :: [Method mnm]) (hs :: [Type]) where
  gRpcMethodHandlers :: ByteString -> ByteString
                     -> HandlersIO ms hs -> [ServiceHandler]

instance GRpcMethodHandlers '[] '[] where
  gRpcMethodHandlers _ _ H0 = []
instance (KnownName name, GRpcMethodHandler args r h, GRpcMethodHandlers rest hs)
         => GRpcMethodHandlers ('Method name anns args r ': rest) (h ': hs) where
  gRpcMethodHandlers p s (h :<|>: rest)
    = gRpcMethodHandler (Proxy @args) (Proxy @r) (RPC p s methodName) h
      : gRpcMethodHandlers p s rest
    where methodName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandler args r h where
  gRpcMethodHandler :: Proxy args -> Proxy r -> RPC -> h -> ServiceHandler

instance GRpcMethodHandler '[ ] 'RetNothing (IO ()) where
  gRpcMethodHandler _ _ rpc h
    = unary @_ @() @() rpc (\_ _ -> h)

instance (ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ ] ('RetSingle rref) (IO r) where
  gRpcMethodHandler _ _ rpc h
    = unary @_ @() @(ViaProtoBufTypeRef rref r)
            rpc (\_ _ -> ViaProtoBufTypeRef <$> h)

instance (ProtoBufTypeRef vref v)
         => GRpcMethodHandler '[ 'ArgSingle vref ] 'RetNothing (v -> IO ()) where
  gRpcMethodHandler _ _ rpc h
    = unary @_ @(ViaProtoBufTypeRef vref v) @()
            rpc (\_ -> h . unViaProtoBufTypeRef)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgSingle vref ] ('RetSingle rref)
                              (v -> IO r) where
  gRpcMethodHandler _ _ rpc h
    = unary @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
            rpc (\_ -> (ViaProtoBufTypeRef <$>) . h . unViaProtoBufTypeRef)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgStream vref ] ('RetSingle rref)
                              (ConduitT () v IO () -> IO r) where
  gRpcMethodHandler _ _ rpc h
    = clientStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                   rpc cstream
    where cstream :: req -> IO ((), ClientStream (ViaProtoBufTypeRef vref v) (ViaProtoBufTypeRef rref r) ())
          cstream _ = do
            -- Create a new TMChan
            chan <- newTMChanIO :: IO (TMChan v)
            let producer = sourceTMChan @IO chan
            -- Start executing the handler in another thread
            promise <- async (ViaProtoBufTypeRef <$> h producer)
            -- Build the actual handler
            let cstreamHandler _ (ViaProtoBufTypeRef newInput)
                  = atomically (writeTMChan chan newInput)
                cstreamFinalizer _
                  = atomically (closeTMChan chan) >> wait promise
            -- Return the information
            return ((), ClientStream cstreamHandler cstreamFinalizer)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgSingle vref ] ('RetStream rref)
                              (v -> ConduitT r Void IO () -> IO ()) where
  gRpcMethodHandler _ _ rpc h
    = serverStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                   rpc sstream
    where sstream :: req -> ViaProtoBufTypeRef vref v
                  -> IO ((), ServerStream (ViaProtoBufTypeRef rref r) ())
          sstream _ (ViaProtoBufTypeRef v) = do
            -- Variable to connect input and output
            var <- newEmptyTMVarIO :: IO (TMVar (Maybe r))
            -- Start executing the handler
            promise <- async (ViaProtoBufTypeRef <$> h v (toTMVarConduit var))
              -- Return the information
            let readNext _
                  = do nextOutput <- atomically $ takeTMVar var
                       case nextOutput of
                         Just o  -> return $ Just ((), ViaProtoBufTypeRef o)
                         Nothing -> do cancel promise
                                       return Nothing
            return ((), ServerStream readNext)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgStream vref ] ('RetStream rref)
                              (ConduitT () v IO () -> ConduitT r Void IO () -> IO ()) where
  gRpcMethodHandler _ _ rpc h
    = generalStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                    rpc bdstream
    where bdstream :: req -> IO ( (), IncomingStream (ViaProtoBufTypeRef vref v) ()
                                , (), OutgoingStream (ViaProtoBufTypeRef rref r) () )
          bdstream _ = do
            -- Create a new TMChan and a new variable
            chan <- newTMChanIO :: IO (TMChan v)
            let producer = sourceTMChan @IO chan
            var <- newEmptyTMVarIO :: IO (TMVar (Maybe r))
            -- Start executing the handler
            promise <- async (h producer (toTMVarConduit var))
            -- Build the actual handler
            let cstreamHandler _ (ViaProtoBufTypeRef newInput)
                  = atomically (writeTMChan chan newInput)
                cstreamFinalizer _
                  = atomically (closeTMChan chan) >> wait promise
                readNext _
                  = do nextOutput <- atomically $ tryTakeTMVar var
                       case nextOutput of
                         Just (Just o) ->
                           return $ Just ((), ViaProtoBufTypeRef o)
                         Just Nothing  -> do
                           cancel promise
                           return Nothing
                         Nothing -> -- no new elements to output
                           readNext ()
            return ((), IncomingStream cstreamHandler cstreamFinalizer, (), OutgoingStream readNext)

toTMVarConduit :: TMVar (Maybe r) -> ConduitT r Void IO ()
toTMVarConduit var = do
  x <- await
  liftIO $ atomically $ putTMVar var x
  toTMVarConduit var
