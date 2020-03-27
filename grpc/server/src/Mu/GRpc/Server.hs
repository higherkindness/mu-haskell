{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Execute a Mu 'Server' using gRPC as transport layer

This module allows you to server a Mu 'Server'
as a WAI 'Application' using gRPC as transport layer.

The simples way is to use 'runGRpcApp', all other
variants provide more control over the settings.
-}
module Mu.GRpc.Server
( -- * Supported messaging formats
  GRpcMessageProtocol(..)
, msgProtoBuf, msgAvro
  -- * Run a 'Server' directly
, runGRpcApp, runGRpcAppTrans
, runGRpcAppSettings, Settings
, runGRpcAppTLS, TLSSettings
  -- * Convert a 'Server' into a WAI application
, gRpcApp, gRpcAppTrans
  -- * Raise errors as exceptions in IO
, raiseErrors, liftServerConduit
  -- * Re-export useful instances
, module Avro
) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM             (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Exception
import           Control.Monad.Except
import           Data.Avro
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Conduit
import           Data.Conduit.TMChan
import           Data.Kind
import           Data.Proxy
import           Network.GRPC.HTTP2.Encoding        (GRPCInput, GRPCOutput, gzip, uncompressed)
import           Network.GRPC.HTTP2.Types           (GRPCStatus (..), GRPCStatusCode (..))
import           Network.GRPC.Server.Handlers.Trans
import           Network.GRPC.Server.Wai            as Wai
import           Network.Wai                        (Application)
import           Network.Wai.Handler.Warp           (Port, Settings, run, runSettings)
import           Network.Wai.Handler.WarpTLS        (TLSSettings, runTLS)

import           Mu.Adapter.ProtoBuf.Via
import           Mu.GRpc.Avro
import qualified Mu.GRpc.Avro                       as Avro
import           Mu.GRpc.Bridge
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

-- | Run a Mu 'Server' on the given port.
runGRpcApp
  :: ( KnownName name, GRpcServiceHandlers protocol ServerErrorIO chn services handlers )
  => Proxy protocol
  -> Port
  -> ServerT chn ('Package ('Just name) services) ServerErrorIO handlers
  -> IO ()
runGRpcApp protocol port = runGRpcAppTrans protocol port id

-- | Run a Mu 'Server' on the given port.
runGRpcAppTrans
  :: ( KnownName name, GRpcServiceHandlers protocol m chn services handlers )
  => Proxy protocol
  -> Port
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn ('Package ('Just name) services) m handlers
  -> IO ()
runGRpcAppTrans protocol port f svr = run port (gRpcAppTrans protocol f svr)

-- | Run a Mu 'Server' using the given 'Settings'.
--
--   Go to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppSettings
  :: ( KnownName name, GRpcServiceHandlers protocol m chn services handlers )
  => Proxy protocol
  -> Settings
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn ('Package ('Just name) services) m handlers
  -> IO ()
runGRpcAppSettings protocol st f svr = runSettings st (gRpcAppTrans protocol f svr)

-- | Run a Mu 'Server' using the given 'TLSSettings' and 'Settings'.
--
--   Go to 'Network.Wai.Handler.WarpTLS' to declare 'TLSSettings'
--   and to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGRpcAppTLS
  :: ( KnownName name, GRpcServiceHandlers protocol m chn services handlers )
  => Proxy protocol
  -> TLSSettings -> Settings
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn ('Package ('Just name) services) m handlers
  -> IO ()
runGRpcAppTLS protocol tls st f svr = runTLS tls st (gRpcAppTrans protocol f svr)

-- | Turn a Mu 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
gRpcApp
  :: ( KnownName name, GRpcServiceHandlers protocol ServerErrorIO chn services handlers )
  => Proxy protocol
  -> ServerT chn ('Package ('Just name) services) ServerErrorIO handlers
  -> Application
gRpcApp protocol = gRpcAppTrans protocol id

-- | Turn a Mu 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
gRpcAppTrans
  :: ( KnownName name, GRpcServiceHandlers protocol m chn services handlers )
  => Proxy protocol
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn ('Package ('Just name) services) m handlers
  -> Application
gRpcAppTrans protocol f svr
  = Wai.grpcApp [uncompressed, gzip]
                (gRpcServerHandlers protocol f svr)

gRpcServerHandlers
  :: forall name services handlers m protocol chn.
     ( KnownName name, GRpcServiceHandlers protocol m chn services handlers )
  => Proxy protocol
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn ('Package ('Just name) services) m handlers
  -> [ServiceHandler]
gRpcServerHandlers pr f (Services svr) = gRpcServiceHandlers f pr packageName svr
  where packageName = BS.pack (nameVal (Proxy @name))

class GRpcServiceHandlers (p :: GRpcMessageProtocol) (m :: Type -> Type)
                          (chn :: ServiceChain snm)
                          (ss :: [Service snm mnm anm]) (hs :: [[Type]]) where
  gRpcServiceHandlers :: (forall a. m a -> ServerErrorIO a)
                      -> Proxy p -> ByteString
                      -> ServicesT chn ss m hs -> [ServiceHandler]

instance GRpcServiceHandlers p m chn '[] '[] where
  gRpcServiceHandlers _ _ _ S0 = []
instance ( KnownName name, GRpcMethodHandlers p m chn (MappingRight chn name) methods h
         , GRpcServiceHandlers p m chn rest hs )
         => GRpcServiceHandlers p m chn ('Service name anns methods ': rest) (h ': hs) where
  gRpcServiceHandlers f pr packageName (svr :<&>: rest)
    =  gRpcMethodHandlers f pr packageName serviceName svr
    ++ gRpcServiceHandlers f pr packageName rest
    where serviceName = BS.pack (nameVal (Proxy @name))


class GRpcMethodHandlers (p :: GRpcMessageProtocol) (m :: Type -> Type)
                         (chn :: ServiceChain snm) (inh :: Type)
                         (ms :: [Method snm mnm anm]) (hs :: [Type]) where
  gRpcMethodHandlers :: (forall a. m a -> ServerErrorIO a)
                     -> Proxy p -> ByteString -> ByteString
                     -> HandlersT chn inh ms m hs -> [ServiceHandler]

instance GRpcMethodHandlers p m chn inh '[] '[] where
  gRpcMethodHandlers _ _ _ _ H0 = []
instance ( KnownName name, MkRPC p
         , GRpcMethodHandler p m args r h
         , GRpcMethodHandlers p m chn () rest hs)
         => GRpcMethodHandlers p m chn () ('Method name anns args r ': rest) (h ': hs) where
  gRpcMethodHandlers f pr p s (h :<||>: rest)
    = gRpcMethodHandler f pr (Proxy @args) (Proxy @r) (mkRPC pr p s methodName) (h ())
      : gRpcMethodHandlers f pr p s rest
    where methodName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandler p m args r h where
  gRpcMethodHandler :: (forall a. m a -> ServerErrorIO a)
                    -> Proxy p -> Proxy args -> Proxy r
                    -> RPCTy p -> h -> ServiceHandler

-- | Turns a 'Conduit' working on 'ServerErrorIO'
--   into any other base monad which supports 'IO',
--   by raising any error as an exception.
--
--   This function is useful to interoperate with
--   libraries which generate 'Conduit's with other
--   base monads, such as @persistent@.
liftServerConduit
  :: MonadIO m
  => ConduitT a b ServerErrorIO r -> ConduitT a b m r
liftServerConduit = transPipe raiseErrors

-- | Raises errors from 'ServerErrorIO' as exceptions
--   in a monad which supports 'IO'.
--
--   This function is useful to interoperate with other
--   libraries which cannot handle the additional error
--   layer. In particular, with Conduit, as witnessed
--   by 'liftServerConduit'.
raiseErrors :: MonadIO m => ServerErrorIO a -> m a
raiseErrors h
  = liftIO $ do
      h' <- runExceptT h
      case h' of
        Right r -> pure r
        Left (ServerError code msg)
          -> closeEarly $ GRPCStatus (serverErrorToGRpcError code)
                                     (BS.pack msg)
    `catches`
    [ Handler (\(e :: GRPCStatus) -> throwIO e)
    , Handler (\(e :: SomeException) -> closeEarly $ GRPCStatus INTERNAL (BS.pack $ show e))
    ]

  where
    serverErrorToGRpcError :: ServerErrorCode -> GRPCStatusCode
    serverErrorToGRpcError Unknown         = UNKNOWN
    serverErrorToGRpcError Unavailable     = UNAVAILABLE
    serverErrorToGRpcError Unimplemented   = UNIMPLEMENTED
    serverErrorToGRpcError Unauthenticated = UNAUTHENTICATED
    serverErrorToGRpcError Internal        = INTERNAL
    serverErrorToGRpcError NotFound        = NOT_FOUND
    serverErrorToGRpcError Invalid         = INVALID_ARGUMENT

-----
-- IMPLEMENTATION OF THE METHODS
-----

-- These type classes allow us to abstract over
-- the choice of message protocol (PB or Avro)

class GRPCOutput (RPCTy p) (GRpcOWTy p ref r)
      => GRpcOutputWrapper (p :: GRpcMessageProtocol) (ref :: TypeRef snm) (r :: Type) where
  type GRpcOWTy p ref r :: Type
  buildGRpcOWTy :: Proxy p -> Proxy ref -> r -> GRpcOWTy p ref r

instance ToProtoBufTypeRef ref r
         => GRpcOutputWrapper 'MsgProtoBuf ref r where
  type GRpcOWTy 'MsgProtoBuf ref r = ViaToProtoBufTypeRef ref r
  buildGRpcOWTy _ _ = ViaToProtoBufTypeRef

instance forall (sch :: Schema') sty (r :: Type).
         ( ToSchema sch sty r
         , ToAvro (WithSchema sch sty r)
         , HasAvroSchema (WithSchema sch sty r) )
         => GRpcOutputWrapper 'MsgAvro ('SchemaRef sch sty) r where
  type GRpcOWTy 'MsgAvro ('SchemaRef sch sty) r = ViaToAvroTypeRef ('SchemaRef sch sty) r
  buildGRpcOWTy _ _ = ViaToAvroTypeRef

class GRPCInput (RPCTy p) (GRpcIWTy p ref r)
      => GRpcInputWrapper (p :: GRpcMessageProtocol) (ref :: TypeRef snm) (r :: Type) where
  type GRpcIWTy p ref r :: Type
  unGRpcIWTy :: Proxy p -> Proxy ref -> GRpcIWTy p ref r -> r

instance FromProtoBufTypeRef ref r
         => GRpcInputWrapper 'MsgProtoBuf ref r where
  type GRpcIWTy 'MsgProtoBuf ref r = ViaFromProtoBufTypeRef ref r
  unGRpcIWTy _ _ = unViaFromProtoBufTypeRef

instance forall (sch :: Schema') sty (r :: Type).
         ( FromSchema sch sty r
         , FromAvro (WithSchema sch sty r)
         , HasAvroSchema (WithSchema sch sty r) )
         => GRpcInputWrapper 'MsgAvro ('SchemaRef sch sty) r where
  type GRpcIWTy 'MsgAvro ('SchemaRef sch sty) r = ViaFromAvroTypeRef ('SchemaRef sch sty) r
  unGRpcIWTy _ _ = unViaFromAvroTypeRef

---

instance (MonadIO m, GRPCInput (RPCTy p) (), GRPCOutput (RPCTy p) ())
         => GRpcMethodHandler p m '[ ] 'RetNothing (m ()) where
  gRpcMethodHandler f _ _ _ rpc h
    = unary @m @_ @() @() (raiseErrors . f) rpc (\_ _ -> h)

-----

instance (MonadIO m, GRPCInput (RPCTy p) (), GRpcOutputWrapper p rref r)
         => GRpcMethodHandler p m '[ ] ('RetSingle rref) (m r) where
  gRpcMethodHandler f _ _ _ rpc h
    = unary @m @_ @() @(GRpcOWTy p rref r)
            (raiseErrors . f) rpc (\_ _ -> buildGRpcOWTy (Proxy @p) (Proxy @rref) <$> h)

-----

instance (MonadIO m, GRPCInput (RPCTy p) (), GRpcOutputWrapper p rref r, MonadIO m)
         => GRpcMethodHandler p m '[ ] ('RetStream rref)
                              (ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ _ rpc h
    = serverStream @m @_ @() @(GRpcOWTy p rref r) (raiseErrors . f) rpc sstream
    where sstream :: req -> ()
                  -> m ((), ServerStream m (GRpcOWTy p rref r) ())
          sstream _ _ = do
            -- Variable to connect input and output
            var <- liftIO newEmptyTMVarIO :: m (TMVar (Maybe r))
            -- Start executing the handler
            liftIO $ withAsync (raiseErrors $ f (h (toTMVarConduit var))) $ \promise -> do
              -- Return the information
              let readNext _
                    = do nextOutput <- liftIO $ atomically $ takeTMVar var
                         case nextOutput of
                           Just o  -> pure $ Just ((), buildGRpcOWTy (Proxy @p) (Proxy @rref) o)
                           Nothing -> do liftIO $ cancel promise
                                         pure Nothing
              pure ((), ServerStream readNext)

-----

instance (MonadIO m, GRpcInputWrapper p vref v, GRPCOutput (RPCTy p) ())
         => GRpcMethodHandler p m '[ 'ArgSingle aname anns vref ] 'RetNothing (v -> m ()) where
  gRpcMethodHandler f _ _ _ rpc h
    = unary @m @_ @(GRpcIWTy p vref v) @()
            (raiseErrors . f) rpc (\_ -> h . unGRpcIWTy (Proxy @p) (Proxy @vref))

-----

instance (MonadIO m, GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r)
         => GRpcMethodHandler p m '[ 'ArgSingle aname anns vref ] ('RetSingle rref) (v -> m r) where
  gRpcMethodHandler f _ _ _ rpc h
    = unary @m @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
            (raiseErrors . f) rpc
            (\_ -> (buildGRpcOWTy (Proxy @p) (Proxy @rref) <$>)
                   . h
                   . unGRpcIWTy (Proxy @p) (Proxy @vref))

-----

instance (MonadIO m, GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r, MonadIO m)
         => GRpcMethodHandler p m '[ 'ArgStream aname anns vref ] ('RetSingle rref)
                              (ConduitT () v m () -> m r) where
  gRpcMethodHandler f _ _ _ rpc h
    = clientStream @m @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
                   (raiseErrors . f) rpc cstream
    where cstream :: req
                  -> m ((), ClientStream m (GRpcIWTy p vref v)
                        (GRpcOWTy p rref r) ())
          cstream _ = do
            -- Create a new TMChan
            chan <- liftIO newTMChanIO :: m (TMChan v)
            let producer = sourceTMChan @m chan
            -- Start executing the handler in another thread
            liftIO $ withAsync (raiseErrors $ buildGRpcOWTy (Proxy @p) (Proxy @rref) <$> f (h producer))
                   $ \promise -> do
              -- Build the actual handler
              let cstreamHandler _ newInput
                    = liftIO $ atomically $
                        writeTMChan chan (unGRpcIWTy (Proxy @p) (Proxy @vref) newInput)
                  cstreamFinalizer _
                    = liftIO $ atomically (closeTMChan chan) >> wait promise
              -- Return the information
              pure ((), ClientStream cstreamHandler cstreamFinalizer)

-----

instance (GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r, MonadIO m)
         => GRpcMethodHandler p m '[ 'ArgSingle aname anns vref ] ('RetStream rref)
                              (v -> ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ _ rpc h
    = serverStream @m @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
                   (raiseErrors . f) rpc sstream
    where sstream :: req -> GRpcIWTy p vref v
                  -> m ((), ServerStream m (GRpcOWTy p rref r) ())
          sstream _ v = do
            -- Variable to connect input and output
            var <- liftIO newEmptyTMVarIO :: m (TMVar (Maybe r))
            -- Start executing the handler
            let v' = unGRpcIWTy (Proxy @p) (Proxy @vref) v
            liftIO $ withAsync (raiseErrors $ f (h v' (toTMVarConduit var))) $ \promise -> do
              -- Return the information
              let readNext _
                    = do nextOutput <- liftIO $ atomically $ takeTMVar var
                         case nextOutput of
                           Just o  -> pure $ Just ((), buildGRpcOWTy (Proxy @p) (Proxy @rref) o)
                           Nothing -> do liftIO $ cancel promise
                                         pure Nothing
              pure ((), ServerStream readNext)

-----

instance (GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r, MonadIO m)
         => GRpcMethodHandler p m '[ 'ArgStream aname anns vref ] ('RetStream rref)
                              (ConduitT () v m () -> ConduitT r Void m () -> m ()) where
  gRpcMethodHandler f _ _ _ rpc h
    = generalStream @m @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
                    (raiseErrors . f) rpc bdstream
    where bdstream :: req -> m ( (), IncomingStream m (GRpcIWTy p vref v) ()
                               , (), OutgoingStream m (GRpcOWTy p rref r) () )
          bdstream _ = do
            -- Create a new TMChan and a new variable
            chan <- liftIO newTMChanIO :: m (TMChan v)
            let producer = sourceTMChan @m chan
            var <- liftIO newEmptyTMVarIO :: m (TMVar (Maybe r))
            -- Start executing the handler
            liftIO $ withAsync (raiseErrors $ f $ h producer (toTMVarConduit var)) $ \promise -> do
              -- Build the actual handler
              let cstreamHandler _ newInput
                    = liftIO $ atomically $
                        writeTMChan chan (unGRpcIWTy (Proxy @p) (Proxy @vref) newInput)
                  cstreamFinalizer _
                    = liftIO $ atomically (closeTMChan chan) >> wait promise
                  readNext _
                    = do nextOutput <- liftIO $ atomically $ tryTakeTMVar var
                         case nextOutput of
                           Just (Just o) ->
                             pure $ Just ((), buildGRpcOWTy (Proxy @p) (Proxy @rref) o)
                           Just Nothing  -> do
                             liftIO $ cancel promise
                             pure Nothing
                           Nothing -> -- no new elements to output
                             readNext ()
              pure ((), IncomingStream cstreamHandler cstreamFinalizer, (), OutgoingStream readNext)

-----

toTMVarConduit :: MonadIO m => TMVar (Maybe r) -> ConduitT r Void m ()
toTMVarConduit var = do
  x <- await
  liftIO $ atomically $ putTMVar var x
  toTMVarConduit var
