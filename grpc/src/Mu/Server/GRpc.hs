{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances,
             TypeApplications, TypeOperators,
             ScopedTypeVariables,
             TupleSections #-}
{-# OPTIONS_GHC -fprint-explicit-foralls -fprint-explicit-kinds #-}
module Mu.Server.GRpc where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar
import Control.Monad.IO.Class
import Data.Conduit
import Data.Conduit.TMChan
import Data.Kind
import Data.Proxy
import Network.GRPC.HTTP2.Encoding (uncompressed, gzip)
import Network.GRPC.Server.Wai (ServiceHandler)
import Network.GRPC.Server.Handlers.NoLens
import Network.GRPC.Server.Wai as Wai
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, Settings, run, runSettings)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS)

import Mu.Rpc
import Mu.Server
import Mu.Schema

import Mu.GRpc.Shared

runGRpcApp
  :: (KnownName name, GRpcMethodHandlers methods handlers)
  => Port -> ByteString -> ServerIO ('Service name methods) handlers
  -> IO ()
runGRpcApp port pkgName svr = run port (gRpcApp pkgName svr)

runGRpcAppSettings
  :: (KnownName name, GRpcMethodHandlers methods handlers)
  => Settings -> ByteString -> ServerIO ('Service name methods) handlers
  -> IO ()
runGRpcAppSettings st pkgName svr = runSettings st (gRpcApp pkgName svr)

runGRpcAppTLS
  :: (KnownName name, GRpcMethodHandlers methods handlers)
  => TLSSettings -> Settings
  -> ByteString -> ServerIO ('Service name methods) handlers
  -> IO ()
runGRpcAppTLS tls st pkgName svr = runTLS tls st (gRpcApp pkgName svr)

gRpcApp
  :: (KnownName name, GRpcMethodHandlers methods handlers)
  => ByteString -> ServerIO ('Service name methods) handlers
  -> Application
gRpcApp p svr = Wai.grpcApp [uncompressed, gzip]
                            (gRpcServiceHandlers p svr)

gRpcServiceHandlers
  :: forall name methods handlers.
     (KnownName name, GRpcMethodHandlers methods handlers)
  => ByteString -> ServerIO ('Service name methods) handlers
  -> [ServiceHandler]
gRpcServiceHandlers p (Server svr) = gRpcMethodHandlers p serviceName svr
  where serviceName = BS.pack (nameVal (Proxy @name))
        
class GRpcMethodHandlers (ms :: [Method mnm]) (hs :: [Type]) where
  gRpcMethodHandlers :: ByteString -> ByteString
                     -> HandlersIO ms hs -> [ServiceHandler]

instance GRpcMethodHandlers '[] '[] where
  gRpcMethodHandlers _ _ H0 = []
instance (KnownName name, GRpcMethodHandler args r h, GRpcMethodHandlers rest hs)
         => GRpcMethodHandlers ('Method name args r ': rest) (h ': hs) where
  gRpcMethodHandlers p s (h :<|>: rest)
    = gRpcMethodHandler (Proxy @args) (Proxy @r) (RPC p s methodName) h
      : gRpcMethodHandlers p s rest
    where methodName = BS.pack (nameVal (Proxy @name))

class GRpcMethodHandler args r h where
  gRpcMethodHandler :: Proxy args -> Proxy r -> RPC -> h -> ServiceHandler

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgSingle vref ] ('RetSingle rref)
                              (v -> IO r) where
  gRpcMethodHandler _ _ rpc h
    = unary (fromProtoBufTypeRef (Proxy @vref), toProtoBufTypeRef (Proxy @rref))
            rpc (\_req -> h)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgStream vref ] ('RetSingle rref)
                              (ConduitT () v IO () -> IO r) where
  gRpcMethodHandler _ _ rpc h
    = clientStream (fromProtoBufTypeRef (Proxy @vref), toProtoBufTypeRef (Proxy @rref))
                   rpc cstream
    where cstream :: req -> IO ((), ClientStream v r ())
          cstream _ = do
            -- Create a new TMChan
            chan <- newTMChanIO
            let producer = sourceTMChan @IO chan
            -- Start executing the handler in another thread
            promise <- async (h producer)
            -- Build the actual handler
            let cstreamHandler _ newInput
                  = atomically (writeTMChan chan newInput)
                cstreamFinalizer _
                  = atomically (closeTMChan chan) >> wait promise
            -- Return the information
            return ((), ClientStream cstreamHandler cstreamFinalizer)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgSingle vref ] ('RetStream rref)
                              (v -> ConduitT r Void IO () -> IO ()) where
  gRpcMethodHandler _ _ rpc h
    = serverStream (fromProtoBufTypeRef (Proxy @vref), toProtoBufTypeRef (Proxy @rref))
                   rpc sstream
    where sstream :: req -> v -> IO ((), ServerStream r ())
          sstream _ v = do
            -- Variable to connect input and output
            var <- newEmptyTMVarIO
            -- Start executing the handler
            promise <- async (h v (toTMVarConduit var))
              -- Return the information
            let readNext _
                  = do nextOutput <- atomically $ takeTMVar var
                       case nextOutput of
                         Just o  -> return $ Just ((), o)
                         Nothing -> do cancel promise
                                       return Nothing
            return ((), ServerStream readNext)

instance (ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodHandler '[ 'ArgStream vref ] ('RetStream rref)
                              (ConduitT () v IO () -> ConduitT r Void IO () -> IO ()) where
  gRpcMethodHandler _ _ rpc h
    = bidiStream (fromProtoBufTypeRef (Proxy @vref), toProtoBufTypeRef (Proxy @rref))
                 rpc bdstream
    where bdstream :: req -> IO ((), BiDiStream v r ())
          bdstream _ = do
            -- Create a new TMChan and a new variable
            chan <- newTMChanIO
            let producer = sourceTMChan @IO chan
            var <- newEmptyTMVarIO
            -- Start executing the handler
            promise <- async (h producer (toTMVarConduit var))
            -- Build the actual handler
            let cstreamHandler _ newInput
                  = atomically (writeTMChan chan newInput)
                cstreamFinalizer _
                  = atomically (closeTMChan chan) >> wait promise
                readNext _
                  = do nextOutput <- atomically $ tryTakeTMVar var
                       case nextOutput of
                         Just (Just o) ->
                           return $ WriteOutput () o
                         Just Nothing  -> do
                           cancel promise
                           return $ Abort
                         Nothing -> do -- no new elements to output
                           return $ WaitInput cstreamHandler cstreamFinalizer
            return ((), BiDiStream readNext)

toTMVarConduit :: TMVar (Maybe r) -> ConduitT r Void IO ()
toTMVarConduit var = do x <- await
                        liftIO $ atomically $ putTMVar var x
                        toTMVarConduit var