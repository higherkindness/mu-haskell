{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language DeriveFunctor         #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
-- | Client for gRPC services defined using Mu 'Service'
module Mu.GRpc.Client.Internal where

import           Control.Concurrent.Async
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMVar
import           Control.Monad.IO.Class
import           Data.Avro
import qualified Data.ByteString.Char8         as BS
import           Data.Conduit
import qualified Data.Conduit.Combinators      as C
import           Data.Conduit.TMChan
import           Data.Kind
import           GHC.TypeLits
import           Network.GRPC.Client           (CompressMode (..), IncomingEvent (..),
                                                OutgoingEvent (..), RawReply, StreamDone (..))
import           Network.GRPC.Client.Helpers
import           Network.GRPC.HTTP2.Encoding   (GRPCInput, GRPCOutput)
import           Network.HTTP2                 (ErrorCode)
import           Network.HTTP2.Client          (ClientError, ClientIO, TooMuchConcurrency,
                                                runExceptT)

import           Mu.Adapter.ProtoBuf.Via
import           Mu.GRpc.Avro
import           Mu.GRpc.Bridge
import           Mu.Rpc
import           Mu.Schema

setupGrpcClient' :: GrpcClientConfig -> IO (Either ClientError GrpcClient)
setupGrpcClient' = runExceptT . setupGrpcClient

class GRpcServiceMethodCall (p :: GRpcMessageProtocol)
                            (pkg :: snm) (s :: snm) (m :: Method snm mnm anm) h where
  gRpcServiceMethodCall :: Proxy p -> Proxy pkg -> Proxy s -> Proxy m -> GrpcClient -> h
instance ( KnownName serviceName, KnownName pkg, KnownName mname
         , GRpcMethodCall p ('Method mname manns margs mret) h, MkRPC p )
         => GRpcServiceMethodCall p pkg serviceName ('Method mname manns margs mret) h where
  gRpcServiceMethodCall pro _ _ = gRpcMethodCall @p rpc
    where pkgName = BS.pack (nameVal (Proxy @pkg))
          svrName = BS.pack (nameVal (Proxy @serviceName))
          metName = BS.pack (nameVal (Proxy @mname))
          rpc = mkRPC pro pkgName svrName metName

data GRpcReply a
  = GRpcTooMuchConcurrency TooMuchConcurrency
  | GRpcErrorCode ErrorCode
  | GRpcErrorString String
  | GRpcClientError ClientError
  | GRpcOk a
  deriving (Show, Functor)

buildGRpcReply1 :: Either TooMuchConcurrency (RawReply a) -> GRpcReply a
buildGRpcReply1 (Left tmc)                      = GRpcTooMuchConcurrency tmc
buildGRpcReply1 (Right (Left ec))               = GRpcErrorCode ec
buildGRpcReply1 (Right (Right (_, _, Left es))) = GRpcErrorString es
buildGRpcReply1 (Right (Right (_, _, Right r))) = GRpcOk r

buildGRpcReply2 :: Either TooMuchConcurrency (r, RawReply a) -> GRpcReply a
buildGRpcReply2 (Left tmc)                         = GRpcTooMuchConcurrency tmc
buildGRpcReply2 (Right (_, Left ec))               = GRpcErrorCode ec
buildGRpcReply2 (Right (_, Right (_, _, Left es))) = GRpcErrorString es
buildGRpcReply2 (Right (_, Right (_, _, Right r))) = GRpcOk r

buildGRpcReply3 :: Either TooMuchConcurrency v -> GRpcReply ()
buildGRpcReply3 (Left tmc) = GRpcTooMuchConcurrency tmc
buildGRpcReply3 (Right _)  = GRpcOk ()

simplifyResponse :: ClientIO (GRpcReply a) -> IO (GRpcReply a)
simplifyResponse reply = do
  r <- runExceptT reply
  pure $ case r of
    Left e  -> GRpcClientError e
    Right v -> v

-- These type classes allow us to abstract over
-- the choice of message protocol (PB or Avro)

class GRPCInput (RPCTy p) (GRpcIWTy p ref r)
      => GRpcInputWrapper (p :: GRpcMessageProtocol) (ref :: TypeRef snm) (r :: Type) where
  type GRpcIWTy p ref r :: Type
  buildGRpcIWTy :: Proxy p -> Proxy ref -> r -> GRpcIWTy p ref r

instance ToProtoBufTypeRef ref r
         => GRpcInputWrapper 'MsgProtoBuf ref r where
  type GRpcIWTy 'MsgProtoBuf ref r = ViaToProtoBufTypeRef ref r
  buildGRpcIWTy _ _ = ViaToProtoBufTypeRef

instance forall (sch :: Schema') (sty :: Symbol) (r :: Type).
         ( ToSchema sch sty r
         , ToAvro (WithSchema sch sty r)
         , HasAvroSchema (WithSchema sch sty r) )
         => GRpcInputWrapper 'MsgAvro ('SchemaRef sch sty) r where
  type GRpcIWTy 'MsgAvro ('SchemaRef sch sty) r = ViaToAvroTypeRef ('SchemaRef sch sty) r
  buildGRpcIWTy _ _ = ViaToAvroTypeRef

class GRPCOutput (RPCTy p) (GRpcOWTy p ref r)
      => GRpcOutputWrapper (p :: GRpcMessageProtocol) (ref :: TypeRef snm) (r :: Type) where
  type GRpcOWTy p ref r :: Type
  unGRpcOWTy :: Proxy p -> Proxy ref -> GRpcOWTy p ref r -> r

instance FromProtoBufTypeRef ref r
         => GRpcOutputWrapper 'MsgProtoBuf ref r where
  type GRpcOWTy 'MsgProtoBuf ref r = ViaFromProtoBufTypeRef ref r
  unGRpcOWTy _ _ = unViaFromProtoBufTypeRef

instance forall (sch :: Schema') (sty :: Symbol) (r :: Type).
         ( FromSchema sch sty r
         , FromAvro (WithSchema sch sty r)
         , HasAvroSchema (WithSchema sch sty r) )
         => GRpcOutputWrapper 'MsgAvro ('SchemaRef sch sty) r where
  type GRpcOWTy 'MsgAvro ('SchemaRef sch sty) r = ViaFromAvroTypeRef ('SchemaRef sch sty) r
  unGRpcOWTy _ _ = unViaFromAvroTypeRef

-- -----------------------------
-- IMPLEMENTATION OF THE METHODS
-- -----------------------------

class GRpcMethodCall (p :: GRpcMessageProtocol) (method :: Method') h where
  gRpcMethodCall :: RPCTy p -> Proxy method -> GrpcClient -> h

instance ( KnownName name
         , GRPCInput (RPCTy p) (), GRPCOutput (RPCTy p) ()
         , handler ~ IO (GRpcReply ()) )
         => GRpcMethodCall p ('Method name anns '[ ] 'RetNothing) handler where
  gRpcMethodCall rpc _ client
    = simplifyResponse $
      buildGRpcReply1 <$>
      rawUnary rpc client ()

instance ( KnownName name
         , GRPCInput (RPCTy p) (), GRpcOutputWrapper p rref r
         , handler ~ IO (GRpcReply r) )
         => GRpcMethodCall p ('Method name anns '[ ] ('RetSingle rref)) handler where
  gRpcMethodCall rpc _ client
    = fmap (fmap (unGRpcOWTy (Proxy @p) (Proxy @rref))) $
      simplifyResponse $
      buildGRpcReply1 <$>
      rawUnary @_ @() @(GRpcOWTy p rref r) rpc client ()

instance ( KnownName name
         , GRPCInput (RPCTy p) (), GRpcOutputWrapper p rref r
         , handler ~ IO (ConduitT () (GRpcReply r) IO ()) )
         => GRpcMethodCall p ('Method name anns '[ ] ('RetStream rref)) handler where
  gRpcMethodCall rpc _ client
    = do -- Create a new TMChan
         chan <- newTMChanIO :: IO (TMChan r)
         var  <- newEmptyTMVarIO  -- if full, this means an error
         -- Start executing the client in another thread
         _ <- async $ do
            v <- simplifyResponse $
                 buildGRpcReply3 <$>
                 rawStreamServer @_ @() @(GRpcOWTy p rref r)
                                 rpc client () ()
                                 (\_ _ newVal -> liftIO $ atomically $ do
                                   -- on the first iteration, say that everything is OK
                                   _ <- tryPutTMVar var (GRpcOk ())
                                   writeTMChan chan (unGRpcOWTy (Proxy @p) (Proxy @rref) newVal))
            case v of
              GRpcOk () -> liftIO $ atomically $ closeTMChan chan
              _         -> liftIO $ atomically $ putTMVar var v
         -- This conduit feeds information to the other thread
         let go = do firstResult <- liftIO $ atomically $ takeTMVar var
                     case firstResult of
                       GRpcOk _ -> -- no error, everything is fine
                         sourceTMChan chan .| C.map GRpcOk
                       e -> yield $ (\_ -> error "this should never happen") <$> e
         pure go

instance ( KnownName name
         , GRpcInputWrapper p vref v, GRPCOutput (RPCTy p) ()
         , handler ~ (v -> IO (GRpcReply ())) )
         => GRpcMethodCall p ('Method name anns '[ 'ArgSingle aname aanns vref ]
                                      'RetNothing) handler where
  gRpcMethodCall rpc _ client x
    = simplifyResponse $
      buildGRpcReply1 <$>
      rawUnary @_ @(GRpcIWTy p vref v) @() rpc client (buildGRpcIWTy (Proxy @p) (Proxy @vref) x)

instance ( KnownName name
         , GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r
         , handler ~ (v -> IO (GRpcReply r)) )
         => GRpcMethodCall p ('Method name anns '[ 'ArgSingle aname aanns vref ]
                                      ('RetSingle rref)) handler where
  gRpcMethodCall rpc _ client x
    = fmap (fmap (unGRpcOWTy (Proxy @p) (Proxy @rref))) $
      simplifyResponse $
      buildGRpcReply1 <$>
      rawUnary @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
               rpc client (buildGRpcIWTy (Proxy @p) (Proxy @vref) x)

instance ( KnownName name
         , GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r
         , handler ~ (CompressMode -> IO (ConduitT v Void IO (GRpcReply r))) )
         => GRpcMethodCall p ('Method name anns '[ 'ArgStream aname aanns vref ]
                                      ('RetSingle rref)) handler where
  gRpcMethodCall rpc _ client compress
    = do -- Create a new TMChan
         chan <- newTMChanIO :: IO (TMChan v)
         -- Start executing the client in another thread
         promise <- async $
            fmap (fmap (unGRpcOWTy (Proxy @p) (Proxy @rref))) $
            simplifyResponse $
            buildGRpcReply2 <$>
            rawStreamClient @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r) rpc client ()
                            (\_ -> do nextVal <- liftIO $ atomically $ readTMChan chan
                                      case nextVal of
                                        Nothing -> pure ((), Left StreamDone)
                                        Just v  -> pure ((), Right (compress, buildGRpcIWTy (Proxy @p) (Proxy @vref) v)))
         -- This conduit feeds information to the other thread
         let go = do x <- await
                     case x of
                       Just v  -> do liftIO $ atomically $ writeTMChan chan v
                                     go
                       Nothing -> do liftIO $ atomically $ closeTMChan chan
                                     liftIO $ wait promise
         pure go

instance ( KnownName name
         , GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r
         , handler ~ (v -> IO (ConduitT () (GRpcReply r) IO ())) )
         => GRpcMethodCall p ('Method name anns '[ 'ArgSingle aname aanns vref ]
                                      ('RetStream rref)) handler where
  gRpcMethodCall rpc _ client x
    = do -- Create a new TMChan
         chan <- newTMChanIO :: IO (TMChan r)
         var  <- newEmptyTMVarIO  -- if full, this means an error
         -- Start executing the client in another thread
         _ <- async $ do
            v <- simplifyResponse $
                 buildGRpcReply3 <$>
                 rawStreamServer @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
                                 rpc client () (buildGRpcIWTy (Proxy @p) (Proxy @vref) x)
                                 (\_ _ newVal -> liftIO $ atomically $ do
                                   -- on the first iteration, say that everything is OK
                                   _ <- tryPutTMVar var (GRpcOk ())
                                   writeTMChan chan (unGRpcOWTy (Proxy @p) (Proxy @rref) newVal))
            case v of
              GRpcOk () -> liftIO $ atomically $ closeTMChan chan
              _         -> liftIO $ atomically $ putTMVar var v
         -- This conduit feeds information to the other thread
         let go = do firstResult <- liftIO $ atomically $ takeTMVar var
                     case firstResult of
                       GRpcOk _ -> -- no error, everything is fine
                         sourceTMChan chan .| C.map GRpcOk
                       e -> yield $ (\_ -> error "this should never happen") <$> e
         pure go

instance ( KnownName name
         , GRpcInputWrapper p vref v, GRpcOutputWrapper p rref r
         , handler ~ (CompressMode -> IO (ConduitT v (GRpcReply r) IO ())) )
         => GRpcMethodCall p ('Method name anns '[ 'ArgStream aname aans vref ]
                                      ('RetStream rref)) handler where
  gRpcMethodCall rpc _ client compress
    = do -- Create a new TMChan
         inchan <- newTMChanIO :: IO (TMChan (GRpcReply r))
         outchan <- newTMChanIO :: IO (TMChan v)
         var <- newEmptyTMVarIO  -- if full, this means an error
         -- Start executing the client in another thread
         _ <- async $ do
            v <- simplifyResponse $
                 buildGRpcReply3 <$>
                 rawGeneralStream
                   @_ @(GRpcIWTy p vref v) @(GRpcOWTy p rref r)
                   rpc client
                   () (\_ ievent -> do -- on the first iteration, say that everything is OK
                        _ <- liftIO $ atomically $ tryPutTMVar var (GRpcOk ())
                        case ievent of
                          RecvMessage o -> liftIO $ atomically $ writeTMChan inchan (GRpcOk $ unGRpcOWTy(Proxy @p) (Proxy @rref) o)
                          Invalid e -> liftIO $ atomically $ writeTMChan inchan (GRpcErrorString (show e))
                          _ -> pure () )
                   () (\_ -> do
                        nextVal <- liftIO $ atomically $ readTMChan outchan
                        case nextVal of
                          Nothing -> pure ((), Finalize)
                          Just v  -> pure ((), SendMessage compress (buildGRpcIWTy (Proxy @p) (Proxy @vref) v)))
            case v of
              GRpcOk () -> liftIO $ atomically $ closeTMChan inchan
              _         -> liftIO $ atomically $ putTMVar var v
         -- This conduit feeds information to the other thread
         let go = do err <- liftIO $ atomically $ takeTMVar var
                     case err of
                       GRpcOk _ -> go2
                       e        -> yield $ (\_ -> error "this should never happen") <$> e
             go2 = do nextOut <- await
                      case nextOut of
                        Just v  -> do liftIO $ atomically $ writeTMChan outchan v
                                      go2
                        Nothing -> do r <- liftIO $ atomically $ tryReadTMChan inchan
                                      case r of
                                        Nothing            -> pure () -- both are empty, end
                                        Just Nothing       -> go2
                                        Just (Just nextIn) -> yield nextIn >> go2
         pure go
