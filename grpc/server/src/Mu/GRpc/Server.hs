{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
-- | Execute a Mu 'Server' using gRPC as transport layer
module Mu.GRpc.Server
( -- * Run a 'Server' directly
  runGRpcApp, runGRpcAppTrans
, runGRpcAppSettings, Settings
, runGRpcAppTLS, TLSSettings
  -- * Convert a 'Server' into a WAI application
, gRpcApp
  -- * Raise errors as exceptions in IO
, raiseErrors, liftServerConduit
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Monad.Except
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as BS
import           Data.Conduit
import           Data.Conduit.TMChan
import           Data.Kind
import           Data.Proxy
import           Network.GRPC.HTTP2.Encoding   (gzip, uncompressed)
import           Network.GRPC.HTTP2.Proto3Wire
import           Network.GRPC.HTTP2.Types      (GRPCStatus (..), GRPCStatusCode (..))
import           Network.GRPC.Server.Handlers
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
     , GRpcMethodHandlers ServerErrorIO methods handlers )
  => Port
  -> ServerT Maybe ('Service name anns methods) ServerErrorIO handlers
  -> IO ()
runGRpcApp port = runGRpcAppTrans port id

-- | Run a Mu 'Server' on the given port.
runGRpcAppTrans
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers m methods handlers )
  => Port
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT Maybe ('Service name anns methods) m handlers
  -> IO ()
runGRpcAppTrans port f svr = run port (gRpcAppTrans f svr)

-- | Run a Mu 'Server' using the given 'Settings'.
--
--   Go to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppSettings
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers m methods handlers )
  => Settings
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT Maybe ('Service name anns methods) m handlers
  -> IO ()
runGRpcAppSettings st f svr = runSettings st (gRpcAppTrans f svr)

-- | Run a Mu 'Server' using the given 'TLSSettings' and 'Settings'.
--
--   Go to 'Network.Wai.Handler.WarpTLS' to declare 'TLSSettings'
--   and to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppTLS
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers m methods handlers )
  => TLSSettings -> Settings
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT Maybe ('Service name anns methods) m handlers
  -> IO ()
runGRpcAppTLS tls st f svr = runTLS tls st (gRpcAppTrans f svr)

-- | Turn a Mu 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
gRpcApp
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers ServerErrorIO methods handlers )
  => ServerT Maybe ('Service name anns methods) ServerErrorIO handlers
  -> Application
gRpcApp = gRpcAppTrans id

-- | Turn a Mu 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
gRpcAppTrans
  :: ( KnownName name, KnownName (FindPackageName anns)
     , GRpcMethodHandlers m methods handlers )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT Maybe ('Service name anns methods) m handlers
  -> Application
gRpcAppTrans f svr = Wai.grpcApp [uncompressed, gzip]
                                 (gRpcServiceHandlers f svr)

gRpcServiceHandlers
  :: forall name anns methods handlers m.
     (KnownName name, KnownName (FindPackageName anns), GRpcMethodHandlers m methods handlers)
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT Maybe ('Service name anns methods) m handlers
  -> [ServiceHandler]
gRpcServiceHandlers f (Server svr) = gRpcMethodHandlers f packageName serviceName svr
  where packageName = BS.pack (nameVal (Proxy @(FindPackageName anns)))
        serviceName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandlers (m :: Type -> Type) (ms :: [Method mnm]) (hs :: [Type]) where
  gRpcMethodHandlers :: (forall a. m a -> ServerErrorIO a)
                     -> ByteString -> ByteString
                     -> HandlersT Maybe ms m hs -> [ServiceHandler]

instance GRpcMethodHandlers m '[] '[] where
  gRpcMethodHandlers _ _ _ H0 = []
instance (KnownName name, GRpcMethodHandler m args r h, GRpcMethodHandlers m rest hs)
         => GRpcMethodHandlers m ('Method name anns args r ': rest) (h ': hs) where
  gRpcMethodHandlers f p s (h :<|>: rest)
    = gRpcMethodHandler f (Proxy @args) (Proxy @r) (RPC p s methodName) h
      : gRpcMethodHandlers f p s rest
    where methodName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandler m args r h where
  gRpcMethodHandler :: (forall a. m a -> ServerErrorIO a)
                    -> Proxy args -> Proxy r -> RPC -> h -> ServiceHandler

liftServerConduit
  :: MonadIO m
  => ConduitT a b ServerErrorIO r -> ConduitT a b m r
liftServerConduit = transPipe raiseErrors

raiseErrors :: MonadIO m => ServerErrorIO a -> m a
raiseErrors h
  = liftIO $ do
      h' <- runExceptT h
      case h' of
        Right r -> return r
        Left (ServerError code msg)
          -> closeEarly $ GRPCStatus (serverErrorToGRpcError code)
                                     (BS.pack msg)
  where
    serverErrorToGRpcError :: ServerErrorCode -> GRPCStatusCode
    serverErrorToGRpcError Unknown         = UNKNOWN
    serverErrorToGRpcError Unavailable     = UNAVAILABLE
    serverErrorToGRpcError Unimplemented   = UNIMPLEMENTED
    serverErrorToGRpcError Unauthenticated = UNAUTHENTICATED
    serverErrorToGRpcError Internal        = INTERNAL
    serverErrorToGRpcError NotFound        = NOT_FOUND
    serverErrorToGRpcError Invalid         = INVALID_ARGUMENT

instance GRpcMethodHandler m '[ ] 'RetNothing (m ()) where
  gRpcMethodHandler f _ _ rpc h
    = unary @_ @() @() rpc (\_ _ -> raiseErrors (f h))

instance (ProtoBufTypeRef rref r)
         => GRpcMethodHandler m '[ ] ('RetSingle rref) (m r) where
  gRpcMethodHandler f _ _ rpc h
    = unary @_ @() @(ViaProtoBufTypeRef rref r)
            rpc (\_ _ -> ViaProtoBufTypeRef <$> raiseErrors (f h))

instance (ProtoBufTypeRef rref r, MonadIO m)
         => GRpcMethodHandler m '[ ] ('RetStream rref)
                              (ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ rpc h
    = serverStream @_ @() @(ViaProtoBufTypeRef rref r) rpc sstream
    where sstream :: req -> ()
                  -> IO ((), ServerStream (ViaProtoBufTypeRef rref r) ())
          sstream _ _ = do
            -- Variable to connect input and output
            var <- newEmptyTMVarIO :: IO (TMVar (Maybe r))
            -- Start executing the handler
            promise <- async (raiseErrors $ ViaProtoBufTypeRef <$> f (h (toTMVarConduit var)))
              -- Return the information
            let readNext _
                  = do nextOutput <- atomically $ takeTMVar var
                       case nextOutput of
                         Just o  -> return $ Just ((), ViaProtoBufTypeRef o)
                         Nothing -> do cancel promise
                                       return Nothing
            return ((), ServerStream readNext)

instance (ProtoBufTypeRef vref v)
         => GRpcMethodHandler m '[ 'ArgSingle vref ] 'RetNothing (v -> m ()) where
  gRpcMethodHandler f _ _ rpc h
    = unary @_ @(ViaProtoBufTypeRef vref v) @()
            rpc (\_ -> raiseErrors . f . h . unViaProtoBufTypeRef)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler m '[ 'ArgSingle vref ] ('RetSingle rref) (v -> m r) where
  gRpcMethodHandler f _ _ rpc h
    = unary @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
            rpc (\_ -> (ViaProtoBufTypeRef <$>) . raiseErrors . f . h . unViaProtoBufTypeRef)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r, MonadIO m)
         => GRpcMethodHandler m '[ 'ArgStream vref ] ('RetSingle rref)
                              (ConduitT () v m () -> m r) where
  gRpcMethodHandler f _ _ rpc h
    = clientStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                   rpc cstream
    where cstream :: req
                  -> IO ((), ClientStream (ViaProtoBufTypeRef vref v)
                        (ViaProtoBufTypeRef rref r) ())
          cstream _ = do
            -- Create a new TMChan
            chan <- newTMChanIO :: IO (TMChan v)
            let producer = sourceTMChan @m chan
            -- Start executing the handler in another thread
            promise <- async (raiseErrors $ ViaProtoBufTypeRef <$> f (h producer))
            -- Build the actual handler
            let cstreamHandler _ (ViaProtoBufTypeRef newInput)
                  = atomically (writeTMChan chan newInput)
                cstreamFinalizer _
                  = atomically (closeTMChan chan) >> wait promise
            -- Return the information
            return ((), ClientStream cstreamHandler cstreamFinalizer)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r, MonadIO m)
         => GRpcMethodHandler m '[ 'ArgSingle vref ] ('RetStream rref)
                              (v -> ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ rpc h
    = serverStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                   rpc sstream
    where sstream :: req -> ViaProtoBufTypeRef vref v
                  -> IO ((), ServerStream (ViaProtoBufTypeRef rref r) ())
          sstream _ (ViaProtoBufTypeRef v) = do
            -- Variable to connect input and output
            var <- newEmptyTMVarIO :: IO (TMVar (Maybe r))
            -- Start executing the handler
            promise <- async (raiseErrors $ ViaProtoBufTypeRef <$> f (h v (toTMVarConduit var)))
              -- Return the information
            let readNext _
                  = do nextOutput <- atomically $ takeTMVar var
                       case nextOutput of
                         Just o  -> return $ Just ((), ViaProtoBufTypeRef o)
                         Nothing -> do cancel promise
                                       return Nothing
            return ((), ServerStream readNext)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r, MonadIO m)
         => GRpcMethodHandler m '[ 'ArgStream vref ] ('RetStream rref)
                              (ConduitT () v m () -> ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ rpc h
    = generalStream @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                    rpc bdstream
    where bdstream :: req -> IO ( (), IncomingStream (ViaProtoBufTypeRef vref v) ()
                                , (), OutgoingStream (ViaProtoBufTypeRef rref r) () )
          bdstream _ = do
            -- Create a new TMChan and a new variable
            chan <- newTMChanIO :: IO (TMChan v)
            let producer = sourceTMChan @m chan
            var <- newEmptyTMVarIO :: IO (TMVar (Maybe r))
            -- Start executing the handler
            promise <- async (raiseErrors $ f $ h producer (toTMVarConduit var))
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

toTMVarConduit :: MonadIO m => TMVar (Maybe r) -> ConduitT r Void m ()
toTMVarConduit var = do
  x <- await
  liftIO $ atomically $ putTMVar var x
  toTMVarConduit var
