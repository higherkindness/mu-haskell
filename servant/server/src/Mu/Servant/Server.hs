{-# LANGUAGE DataKinds #-}
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
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Except
import Data.ByteString.Lazy.UTF8
import Data.Kind
import GHC.TypeLits
import Mu.Rpc
import Mu.Schema
import Mu.Server
import Servant
import Servant.Types.SourceT

toHandler :: ServerErrorIO a -> Handler a
toHandler = Handler . withExceptT convertServerError

convertServerError :: Mu.Server.ServerError -> Servant.ServerError
convertServerError (Mu.Server.ServerError code msg) = case code of
  Unknown -> err502 {errBody = fromString msg}
  Unavailable -> err503 {errBody = fromString msg}
  Unimplemented -> err501 {errBody = fromString msg}
  Unauthenticated -> err401 {errBody = fromString msg}
  Internal -> err500 {errBody = fromString msg}
  Invalid -> err400 {errBody = fromString msg}
  NotFound -> err404 {errBody = fromString msg}

type CannotConvertToServantAPI mname args ret =
  'Text "cannot convert" ':<>: 'ShowType mname ':<>: 'Text "to a Servant API"
    ':$$: 'Text "args:" ':<>: 'ShowType args
    ':$$: 'Text "ret:" ':<>: 'ShowType ret

type CannotConstructServiceAPI srv = 'Text "cannot construct service API from" ':$$: 'ShowType srv

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
  type MethodAPI '[] 'RetNothing (m ()) = Post '[JSON] ()
  servantMethodHandler f _ _ = f

instance ServantMethodHandler m '[] ('RetSingle rref) (m r) where
  type MethodAPI '[] ('RetSingle rref) (m r) = Post '[JSON] r
  servantMethodHandler f _ _ = f

instance MonadIO m => ServantMethodHandler m '[] ('RetStream rref) (ConduitT r Void m () -> m ()) where
  type
    MethodAPI '[] ('RetStream rref) (ConduitT r Void m () -> m ()) =
      StreamPost NewlineFraming JSON (SourceIO r)
  servantMethodHandler f _ _ = sinkToSource f

sinkToSource ::
  forall r m.
  MonadIO m =>
  (forall a. m a -> Handler a) ->
  (ConduitT r Void m () -> m ()) ->
  Handler (SourceIO r)
sinkToSource f sink = do
  var <- liftIO newEmptyTMVarIO :: Handler (TMVar (Maybe r))
  forwarder <- liftIO $ async (runHandler . f . sink . toTMVarConduit $ var)
  let step :: forall b. (StepT IO r -> IO b) -> IO b
      step k = do
        nextOutput <- liftIO $ atomically $ takeTMVar var
        case nextOutput of
          Just r -> step (k . Yield r)
          Nothing -> do
            _ <- liftIO $ cancel forwarder
            k Stop
  pure $ SourceT step

toTMVarConduit :: MonadIO m => TMVar (Maybe r) -> ConduitT r Void m ()
toTMVarConduit var = do
  x <- await
  liftIO $ atomically $ putTMVar var x
  toTMVarConduit var

instance
  (ServantMethodHandler m rest r mr) =>
  ServantMethodHandler m ('ArgSingle anm aref ': rest) r (t -> mr)
  where
  type MethodAPI ('ArgSingle anm aref ': rest) r (t -> mr) = ReqBody '[JSON] t :> MethodAPI rest r mr
  servantMethodHandler pm _ pr h t = servantMethodHandler pm (Proxy @rest) pr (h t)

instance
  (ServantMethodHandler m rest r mr) =>
  ServantMethodHandler m ('ArgStream anm aref ': rest) r (ConduitT () t m () -> mr)
  where
  type
    MethodAPI ('ArgStream anm aref ': rest) r (ConduitT () t m () -> mr) =
      StreamBody NewlineFraming JSON (SourceIO t) :> MethodAPI rest r mr
  servantMethodHandler pm _ pr h = servantMethodHandler pm (Proxy @rest) pr . h . sourceToSource

sourceToSource :: SourceIO t -> ConduitT () t m ()
sourceToSource = undefined
