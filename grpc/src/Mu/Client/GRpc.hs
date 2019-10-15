{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables, TypeApplications,
             TypeOperators, DeriveFunctor,
             AllowAmbiguousTypes,
             TupleSections, UndecidableInstances #-}
module Mu.Client.GRpc (
  GrpcClient
, GrpcClientConfig
, grpcClientConfigSimple
, setupGrpcClient'
, GRpcMethodCall
, (:-->:)
, gRpcCall
, GRpcServiceMethodCall
, gRpcServiceMethodCall
, CompressMode(..)
, GRpcReply(..)
) where

import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TMVar
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.TMChan
import Network.HTTP2 (ErrorCode)
import Network.HTTP2.Client (ClientIO, TooMuchConcurrency, ClientError, runExceptT)
import Network.GRPC.HTTP2.Proto3Wire
import Network.GRPC.Client (RawReply, CompressMode(..), StreamDone(..),
                            IncomingEvent(..),OutgoingEvent(..))
import Network.GRPC.Client.Helpers

import Mu.Rpc
import Mu.Schema

import Mu.GRpc.Shared

setupGrpcClient' :: GrpcClientConfig -> IO (Either ClientError GrpcClient)
setupGrpcClient' = runExceptT . setupGrpcClient

-- | Call a method from a `mu-rpc` definition.
--   This method is thought to be used with `TypeApplications`:
--   > gRpcCall @"packageName" @ServiceDeclaration @"method" 
gRpcCall :: forall s methodName h.
            (GRpcServiceMethodCall s (s :-->: methodName) h)
         => GrpcClient -> h
gRpcCall = gRpcServiceMethodCall (Proxy @s) (Proxy @(s :-->: methodName))

class GRpcServiceMethodCall (s :: Service snm mnm) (m :: Method mnm) h where
  gRpcServiceMethodCall :: Proxy s -> Proxy m -> GrpcClient -> h
instance (KnownName serviceName, KnownName (FindPackageName anns), GRpcMethodCall m h)
         => GRpcServiceMethodCall ('Service serviceName anns methods) m h where
  gRpcServiceMethodCall _ = gRpcMethodCall pkgName svrName
    where pkgName = BS.pack (nameVal (Proxy @(FindPackageName anns)))
          svrName = BS.pack (nameVal (Proxy @serviceName))

data GRpcReply a
  = GRpcTooMuchConcurrency TooMuchConcurrency
  | GRpcErrorCode ErrorCode
  | GRpcErrorString String
  | GRpcClientError ClientError
  | GRpcOk a
  deriving (Show, Functor)

buildGRpcReply1 :: Either TooMuchConcurrency (RawReply a) -> GRpcReply a
buildGRpcReply1 (Left tmc) = GRpcTooMuchConcurrency tmc
buildGRpcReply1 (Right (Left ec)) = GRpcErrorCode ec
buildGRpcReply1 (Right (Right (_, _, Left es))) = GRpcErrorString es
buildGRpcReply1 (Right (Right (_, _, Right r))) = GRpcOk r

buildGRpcReply2 :: Either TooMuchConcurrency (r, (RawReply a)) -> GRpcReply a
buildGRpcReply2 (Left tmc) = GRpcTooMuchConcurrency tmc
buildGRpcReply2 (Right (_, (Left ec))) = GRpcErrorCode ec
buildGRpcReply2 (Right (_, (Right (_, _, Left es)))) = GRpcErrorString es
buildGRpcReply2 (Right (_, (Right (_, _, Right r)))) = GRpcOk r

buildGRpcReply3 :: Either TooMuchConcurrency v -> GRpcReply ()
buildGRpcReply3 (Left tmc) = GRpcTooMuchConcurrency tmc
buildGRpcReply3 (Right _)  = GRpcOk ()

simplifyResponse :: ClientIO (GRpcReply a) -> IO (GRpcReply a)
simplifyResponse reply = do
  r <- runExceptT reply
  case r of
    Left e  -> return $ GRpcClientError e
    Right v -> return v

class GRpcMethodCall method h where
  gRpcMethodCall :: ByteString -> ByteString -> Proxy method -> GrpcClient -> h

instance (KnownName name)
         => GRpcMethodCall ('Method name anns '[ ] 'RetNothing)
                           (IO (GRpcReply ())) where
  gRpcMethodCall pkgName srvName _ client
    = simplifyResponse $ 
      buildGRpcReply1 <$>
      rawUnary rpc client ()
    where methodName = BS.pack (nameVal (Proxy @name))
          rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef rref r)
         => GRpcMethodCall ('Method name anns '[ ] ('RetSingle rref))
                           (IO (GRpcReply r)) where
  gRpcMethodCall pkgName srvName _ client
    = fmap (fmap unViaProtoBufTypeRef) $
      simplifyResponse $ 
      buildGRpcReply1 <$>
      rawUnary @_ @_ @(ViaProtoBufTypeRef rref _)rpc client ()
    where methodName = BS.pack (nameVal (Proxy @name))
          rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef vref v)
         => GRpcMethodCall ('Method name anns '[ 'ArgSingle vref ] 'RetNothing)
                           (v -> IO (GRpcReply ())) where
  gRpcMethodCall pkgName srvName _ client x
    = simplifyResponse $ 
      buildGRpcReply1 <$>
      rawUnary @_ @(ViaProtoBufTypeRef vref _) rpc client (ViaProtoBufTypeRef x)
    where methodName = BS.pack (nameVal (Proxy @name))
          rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodCall ('Method name anns '[ 'ArgSingle vref ] ('RetSingle rref))
                           (v -> IO (GRpcReply r)) where
  gRpcMethodCall pkgName srvName _ client x
    = fmap (fmap unViaProtoBufTypeRef) $
      simplifyResponse $ 
      buildGRpcReply1 <$>
      rawUnary @_ @(ViaProtoBufTypeRef vref _) @(ViaProtoBufTypeRef rref _)
               rpc client (ViaProtoBufTypeRef x)
    where methodName = BS.pack (nameVal (Proxy @name))
          rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodCall ('Method name anns '[ 'ArgStream vref ] ('RetSingle rref))
                           (CompressMode -> IO (ConduitT v Void IO (GRpcReply r))) where
  gRpcMethodCall pkgName srvName _ client compress
    = do -- Create a new TMChan
         chan <- newTMChanIO :: IO (TMChan v)
         -- Start executing the client in another thread
         promise <- async $ 
            fmap (fmap unViaProtoBufTypeRef) $
            simplifyResponse $ 
            buildGRpcReply2 <$>
            rawStreamClient @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r) rpc client ()
                            (\_ -> do nextVal <- liftIO $ atomically $ readTMChan chan
                                      case nextVal of
                                        Nothing -> return ((), Left StreamDone)
                                        Just v  -> return ((), Right (compress, ViaProtoBufTypeRef v)))
         -- This conduit feeds information to the other thread
         let go = do x <- await
                     case x of
                       Just v  -> do liftIO $ atomically $ writeTMChan chan v
                                     go
                       Nothing -> do liftIO $ atomically $ closeTMChan chan
                                     liftIO $ wait promise
         return go 
      where methodName = BS.pack (nameVal (Proxy @name))
            rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodCall ('Method name anns '[ 'ArgSingle vref ] ('RetStream rref))
                           (v -> IO (ConduitT () (GRpcReply r) IO ())) where
  gRpcMethodCall pkgName srvName _ client x
    = do -- Create a new TMChan
         chan <- newTMChanIO :: IO (TMChan r)
         var  <- newEmptyTMVarIO  -- if full, this means an error
         -- Start executing the client in another thread
         _ <- async $ do
            v <- simplifyResponse $ 
                 buildGRpcReply3 <$>
                 rawStreamServer @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                                 rpc client () (ViaProtoBufTypeRef x)
                                 (\_ _ (ViaProtoBufTypeRef newVal) -> liftIO $ atomically $ 
                                   -- on the first iteration, say that everything is OK
                                   tryPutTMVar var (GRpcOk ()) >> writeTMChan chan newVal)
            case v of
              GRpcOk () -> liftIO $ atomically $ closeTMChan chan
              _ -> liftIO $ atomically $ putTMVar var v
         -- This conduit feeds information to the other thread
         let go = do firstResult <- liftIO $ atomically $ takeTMVar var
                     case firstResult of
                       GRpcOk _ -> -- no error, everything is fine
                         sourceTMChan chan .| C.map GRpcOk
                       e -> yield $ (\_ -> error "this should never happen") <$> e
         return go
      where methodName = BS.pack (nameVal (Proxy @name))
            rpc = RPC pkgName srvName methodName

instance (KnownName name, ProtoBufTypeRef vref v, ProtoBufTypeRef rref r)
         => GRpcMethodCall ('Method name anns '[ 'ArgStream vref ] ('RetStream rref))
                           (CompressMode -> IO (ConduitT v (GRpcReply r) IO ())) where
  gRpcMethodCall pkgName srvName _ client compress
    = do -- Create a new TMChan
         inchan <- newTMChanIO :: IO (TMChan (GRpcReply r))
         outchan <- newTMChanIO :: IO (TMChan v)
         var <- newEmptyTMVarIO  -- if full, this means an error
         -- Start executing the client in another thread
         _ <- async $ do
            v <- simplifyResponse $ 
                 buildGRpcReply3 <$>
                 rawGeneralStream
                   @_ @(ViaProtoBufTypeRef vref v) @(ViaProtoBufTypeRef rref r)
                   rpc client
                   () (\_ ievent -> do -- on the first iteration, say that everything is OK
                        _ <- liftIO $ atomically $ tryPutTMVar var (GRpcOk ())
                        case ievent of
                          RecvMessage o -> liftIO $ atomically $ writeTMChan inchan (GRpcOk $ unViaProtoBufTypeRef o)
                          Invalid e -> liftIO $ atomically $ writeTMChan inchan (GRpcErrorString (show e))
                          _ -> return () )
                   () (\_ -> do
                        nextVal <- liftIO $ atomically $ readTMChan outchan
                        case nextVal of
                          Nothing -> return ((), Finalize)
                          Just v  -> return ((), SendMessage compress (ViaProtoBufTypeRef v)))
            case v of
              GRpcOk () -> liftIO $ atomically $ closeTMChan inchan
              _ -> liftIO $ atomically $ putTMVar var v
         -- This conduit feeds information to the other thread
         let go = do err <- liftIO $ atomically $ takeTMVar var
                     case err of
                       GRpcOk _ -> go2
                       e  -> yield $ (\_ -> error "this should never happen") <$> e
             go2 = do nextOut <- await
                      case nextOut of
                        Just v  -> do liftIO $ atomically $ writeTMChan outchan v
                                      go2
                        Nothing -> do r <- liftIO $ atomically $ tryReadTMChan inchan
                                      case r of
                                        Nothing -> return () -- both are empty, end
                                        Just Nothing -> go2
                                        Just (Just nextIn) -> yield nextIn >> go2
         return go
      where methodName = BS.pack (nameVal (Proxy @name))
            rpc = RPC pkgName srvName methodName