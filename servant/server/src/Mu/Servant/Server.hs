{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Mu.Servant.Server where

import Conduit
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LB8
import Data.Conduit.Internal (ConduitT (..), Pipe (..))
import Data.Kind
import GHC.Generics
import Generics.Generic.Aeson
import Mu.Rpc
import Mu.Schema
import Mu.Server
import Servant
import Servant.Types.SourceT

toHandler :: ServerErrorIO a -> Handler a
toHandler = Handler . withExceptT convertServerError

convertServerError :: Mu.Server.ServerError -> Servant.ServerError
convertServerError (Mu.Server.ServerError code msg) = case code of
  Unknown -> err502 {errBody = LB8.fromString msg}
  Unavailable -> err503 {errBody = LB8.fromString msg}
  Unimplemented -> err501 {errBody = LB8.fromString msg}
  Unauthenticated -> err401 {errBody = LB8.fromString msg}
  Internal -> err500 {errBody = LB8.fromString msg}
  Invalid -> err400 {errBody = LB8.fromString msg}
  NotFound -> err404 {errBody = LB8.fromString msg}

servantServerHandlers ::
  forall name services m chn handlers.
  ( ServantServiceHandlers
      ('Package ('Just name) services)
      m
      chn
      services
      handlers
  ) =>
  (forall a. m a -> Handler a) ->
  Mu.Server.ServerT chn () ('Package ('Just name) services) m handlers ->
  Servant.Server (ServicesAPI services handlers)
servantServerHandlers f (Services svcs) =
  servantServiceHandlers f (Proxy @('Package ('Just name) services)) svcs

type family PackageAPI (pkg :: Package snm mnm anm (TypeRef snm)) handlers where
  PackageAPI ('Package sname services) handlers = ServicesAPI services handlers

class
  ServantServiceHandlers
    (fullP :: Package snm mnm anm (TypeRef snm))
    (m :: Type -> Type)
    (chn :: ServiceChain snm)
    (ss :: [Service snm mnm anm (TypeRef snm)])
    (hss :: [[Type]]) where
  type ServicesAPI ss hss

  servantServiceHandlers ::
    (forall a. m a -> Handler a) ->
    Proxy fullP ->
    ServicesT chn info ss m hss ->
    Servant.Server (ServicesAPI ss hss)

instance ServantServiceHandlers fullP m chn '[] '[] where
  type ServicesAPI '[] '[] = EmptyAPI
  servantServiceHandlers _ _ S0 = emptyServer

instance
  ( ServantMethodHandlers
      fullP
      ('Service name methods)
      m
      chn
      (MappingRight chn name)
      methods
      hs,
    ServantServiceHandlers fullP m chn rest hss
  ) =>
  ServantServiceHandlers fullP m chn ('Service name methods ': rest) (hs ': hss)
  where
  type ServicesAPI ('Service name methods ': rest) (hs ': hss) = MethodsAPI methods hs :<|> ServicesAPI rest hss
  servantServiceHandlers f pfullP (svr :<&>: rest) =
    servantMethodHandlers
      f
      pfullP
      (Proxy @('Service name methods))
      svr
      :<|> servantServiceHandlers f pfullP rest

class
  ServantMethodHandlers
    (fullP :: Package snm mnm anm (TypeRef snm))
    (fullS :: Service snm mnm anm (TypeRef snm))
    (m :: Type -> Type)
    (chn :: ServiceChain snm)
    (inh :: Type)
    (ms :: [Method snm mnm anm (TypeRef snm)])
    (hs :: [Type]) where
  type MethodsAPI ms hs
  servantMethodHandlers ::
    (forall a. m a -> Handler a) ->
    Proxy fullP ->
    Proxy fullS ->
    HandlersT chn info inh ms m hs ->
    Servant.Server (MethodsAPI ms hs)

instance ServantMethodHandlers fullP fullS m chn inh '[] '[] where
  type MethodsAPI '[] '[] = EmptyAPI
  servantMethodHandlers _ _ _ H0 = emptyServer

instance
  ( ServantMethodHandler m args r h,
    ServantMethodHandlers fullP fullS m chn () rest hs
  ) =>
  ServantMethodHandlers fullP fullS m chn () ('Method name args r ': rest) (h ': hs)
  where
  type MethodsAPI ('Method name args r ': rest) (h ': hs) = MethodAPI args r h :<|> MethodsAPI rest hs
  servantMethodHandlers f pfullP pfullS (Hmore _ _ h rest) =
    servantMethodHandler
      f
      (Proxy @args)
      (Proxy @r)
      (h NoRpcInfo ())
      :<|> servantMethodHandlers f pfullP pfullS rest

class
  ServantMethodHandler
    (m :: Type -> Type)
    (args :: [Argument snm anm tyref])
    (r :: Return snm tyref)
    (h :: Type) where
  type MethodAPI args r h
  servantMethodHandler ::
    (forall a. m a -> Handler a) ->
    Proxy args ->
    Proxy r ->
    h ->
    Servant.Server (MethodAPI args r h)

instance ServantMethodHandler m '[] 'RetNothing (m ()) where
  type MethodAPI '[] 'RetNothing (m ()) = "post" :> Post '[JSON] ()
  servantMethodHandler f _ _ = f

instance ServantMethodHandler m '[] ('RetSingle rref) (m r) where
  type MethodAPI '[] ('RetSingle rref) (m r) = "postr" :> Post '[JSON] r
  servantMethodHandler f _ _ = f

instance (MonadServer m, Show r) => ServantMethodHandler m '[] ('RetStream rref) (ConduitT r Void m () -> m ()) where
  type
    MethodAPI '[] ('RetStream rref) (ConduitT r Void m () -> m ()) =
      "streampost" :> StreamPost NewlineFraming JSON (SourceIO (StreamResult r))
  servantMethodHandler f _ _ = liftIO . sinkToSource f

data StreamResult a = Error String | Result a
  deriving (Generic, Show)

instance ToJSON a => ToJSON (StreamResult a) where
  toJSON = gtoJson

sinkToSource ::
  forall r m.
  (MonadServer m, Show r) =>
  (forall a. m a -> Handler a) ->
  (ConduitT r Void m () -> m ()) ->
  IO (SourceIO (StreamResult r))
sinkToSource f sink = do
  var <- newEmptyMVar :: IO (MVar (Maybe r))
  forwarder <- liftIO $ async $ do
    e <- runHandler . f . sink $ toMVarConduit var 0
    -- signal that the conduit finished
    putMVar var Nothing
    pure e
  let step :: StepT IO (StreamResult r)
      step = Effect $ do
        nextOutput <- takeMVar var
        case nextOutput of
          Just r -> pure $ Yield (Result r) step
          Nothing -> do
            -- waiting on this thread should get us sync and async exceptions
            res <- wait forwarder
            case res of
              Left err -> do
                let streamErr = LB8.toString $ errBody err
                pure $ Yield (Mu.Servant.Server.Error streamErr) Stop
              Right () -> pure Stop
  pure $ fromStepT step

toMVarConduit :: MonadServer m => MVar (Maybe r) -> Int -> ConduitT r Void m ()
toMVarConduit var n = do
  x <- await
  case x of
    Nothing -> pure ()
    Just _ -> do
      liftIO $ putMVar var x
      toMVarConduit var (n + 1)

instance
  (ServantMethodHandler m rest r mr) =>
  ServantMethodHandler m ('ArgSingle anm aref ': rest) r (t -> mr)
  where
  type MethodAPI ('ArgSingle anm aref ': rest) r (t -> mr) = "reqbody" :> ReqBody '[JSON] t :> MethodAPI rest r mr
  servantMethodHandler pm _ pr h t = servantMethodHandler pm (Proxy @rest) pr (h t)

instance
  (Show t, MonadServer m, ServantMethodHandler m rest r mr) =>
  ServantMethodHandler m ('ArgStream anm aref ': rest) r (ConduitT () t m () -> mr)
  where
  type
    MethodAPI ('ArgStream anm aref ': rest) r (ConduitT () t m () -> mr) =
      "streambody" :> StreamBody NewlineFraming JSON (SourceIO t) :> MethodAPI rest r mr
  servantMethodHandler pm _ pr h =
    servantMethodHandler pm (Proxy @rest) pr . h . sourceToSource

sourceToSource :: (MonadServer m, Show t) => SourceIO t -> ConduitT () t m ()
sourceToSource (SourceT src) = ConduitT (PipeM (liftIO $ src (pure . go)) >>=)
  where
    go :: (MonadServer m, Show t) => StepT IO t -> Pipe i i t u m ()
    go Stop = Done ()
    go (Skip s) = go s
    go (Yield t s) = HaveOutput (go s) t
    go (Effect m) = PipeM (liftIO $ go <$> m)
    go (Servant.Types.SourceT.Error msg) =
      PipeM (throwError $ Mu.Server.ServerError Invalid ("error reading stream: " ++ msg))
