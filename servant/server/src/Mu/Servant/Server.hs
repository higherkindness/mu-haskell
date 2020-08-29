{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import GHC.TypeLits
import Generics.Generic.Aeson
import Mu.Rpc
import Mu.Rpc.Annotations
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
  forall pname m chn ss handlers.
  ( ServantServiceHandlers
      ('Package pname ss)
      m
      chn
      ss
      handlers
  ) =>
  (forall a. m a -> Handler a) ->
  Mu.Server.ServerT chn () ('Package pname ss) m handlers ->
  Servant.Server (PackageAPI ('Package pname ss) handlers)
servantServerHandlers f (Services svcs) =
  servantServiceHandlers f (Proxy @('Package pname ss)) svcs

packageAPI :: Mu.Server.ServerT chn () pkg ServerErrorIO handlers -> Proxy (PackageAPI pkg handlers)
packageAPI _ = Proxy

type family PackageAPI (pkg :: Package snm mnm anm (TypeRef snm)) handlers where
  PackageAPI ('Package pnm ss) handlers = ServicesAPI ('Package pnm ss) ss handlers

class
  ServantServiceHandlers
    (pkg :: Package snm mnm anm (TypeRef snm))
    (m :: Type -> Type)
    (chn :: ServiceChain snm)
    (ss :: [Service snm mnm anm (TypeRef snm)])
    (hss :: [[Type]]) where
  type ServicesAPI pkg ss hss

  servantServiceHandlers ::
    (forall a. m a -> Handler a) ->
    Proxy pkg ->
    ServicesT chn info ss m hss ->
    Servant.Server (ServicesAPI pkg ss hss)

instance ServantServiceHandlers pkg m chn '[] '[] where
  type ServicesAPI pkg '[] '[] = EmptyAPI
  servantServiceHandlers _ _ S0 = emptyServer

instance
  ( ServantMethodHandlers
      pkg
      sname
      m
      chn
      (MappingRight chn sname)
      methods
      hs,
    ServantServiceHandlers pkg m chn rest hss
  ) =>
  ServantServiceHandlers pkg m chn ('Service sname methods ': rest) (hs ': hss)
  where
  type
    ServicesAPI pkg ('Service sname methods ': rest) (hs ': hss) =
      MethodsAPI pkg sname methods hs :<|> ServicesAPI pkg rest hss
  servantServiceHandlers f pkgP (svr :<&>: rest) =
    servantMethodHandlers f pkgP (Proxy @sname) svr
      :<|> servantServiceHandlers f pkgP rest

class
  ServantMethodHandlers
    (pkg :: Package snm mnm anm (TypeRef snm))
    (sname :: snm)
    (m :: Type -> Type)
    (chn :: ServiceChain snm)
    (inh :: Type)
    (ms :: [Method snm mnm anm (TypeRef snm)])
    (hs :: [Type]) where
  type MethodsAPI pkg sname ms hs
  servantMethodHandlers ::
    (forall a. m a -> Handler a) ->
    Proxy pkg ->
    Proxy sname ->
    HandlersT chn info inh ms m hs ->
    Servant.Server (MethodsAPI pkg sname ms hs)

instance ServantMethodHandlers pkg svc m chn inh '[] '[] where
  type MethodsAPI _ _ '[] '[] = EmptyAPI
  servantMethodHandlers _ _ _ H0 = emptyServer

instance
  ( ServantMethodHandler pkg sname m ('Method mname args r) h,
    ServantMethodHandlers pkg sname m chn () rest hs
  ) =>
  ServantMethodHandlers pkg sname m chn () ('Method mname args r ': rest) (h ': hs)
  where
  type
    MethodsAPI pkg sname ('Method mname args r ': rest) (h ': hs) =
      MethodAPI pkg sname ('Method mname args r) h :<|> MethodsAPI pkg sname rest hs
  servantMethodHandlers f pkgP snameP (Hmore _ _ h rest) =
    servantMethodHandler f pkgP snameP (Proxy @('Method mname args r)) (h NoRpcInfo ())
      :<|> servantMethodHandlers f pkgP snameP rest

class
  ServantMethodHandler
    (pkg :: Package snm mnm anm (TypeRef snm))
    (sname :: snm)
    (m :: Type -> Type)
    (method :: Method snm mnm anm tyref)
    (h :: Type) where
  type MethodAPI pkg sname method h
  servantMethodHandler ::
    (forall a. m a -> Handler a) ->
    Proxy pkg ->
    Proxy sname ->
    Proxy method ->
    h ->
    Servant.Server (MethodAPI pkg sname method h)

instance
  (Server (PrefixRoute (RouteFor pkg sname mname) (Post '[JSON] ())) ~ Handler ()) =>
  ServantMethodHandler pkg (sname :: Symbol) m ('Method (mname :: Symbol) '[] 'RetNothing) (m ())
  where
  type
    MethodAPI pkg sname ('Method mname '[] 'RetNothing) (m ()) =
      PrefixRoute (RouteFor pkg sname mname) (Post '[JSON] ())
  servantMethodHandler f _ _ _ = f

instance
  (Server (PrefixRoute (RouteFor pkg sname mname) (Post '[JSON] r)) ~ Handler r) =>
  ServantMethodHandler pkg sname m ('Method mname '[] ('RetSingle rref)) (m r)
  where
  type
    MethodAPI pkg sname ('Method mname '[] ('RetSingle rref)) (m r) =
      PrefixRoute (RouteFor pkg sname mname) (Post '[JSON] r)
  servantMethodHandler f _ _ _ = f

instance
  ( MonadServer m,
    Server
      (PrefixRoute (RouteFor pkg sname mname) (StreamPost NewlineFraming JSON (SourceIO (StreamResult r))))
      ~ Handler (SourceIO (StreamResult r))
  ) =>
  ServantMethodHandler pkg sname m ('Method mname '[] ('RetStream rref)) (ConduitT r Void m () -> m ())
  where
  type
    MethodAPI pkg sname ('Method mname '[] ('RetStream rref)) (ConduitT r Void m () -> m ()) =
      PrefixRoute (RouteFor pkg sname mname) (StreamPost NewlineFraming JSON (SourceIO (StreamResult r)))
  servantMethodHandler f _ _ _ = liftIO . sinkToSource f

data StreamResult a = Error String | Result a
  deriving (Generic, Show)

instance ToJSON a => ToJSON (StreamResult a) where
  toJSON = gtoJson

sinkToSource ::
  forall r m.
  (MonadServer m) =>
  (forall a. m a -> Handler a) ->
  (ConduitT r Void m () -> m ()) ->
  IO (SourceIO (StreamResult r))
sinkToSource f sink = do
  var <- newEmptyMVar :: IO (MVar (Maybe r))
  forwarder <- liftIO $ async $ do
    e <- runHandler . f . sink $ toMVarConduit var
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

toMVarConduit :: MonadServer m => MVar (Maybe r) -> ConduitT r Void m ()
toMVarConduit var = do
  x <- await
  case x of
    Nothing -> pure ()
    Just _ -> do
      liftIO $ putMVar var x
      toMVarConduit var

instance
  (ServantMethodHandler pkg sname m ('Method mname rest r) mr) =>
  ServantMethodHandler pkg sname m ('Method mname ('ArgSingle anm aref ': rest) r) (t -> mr)
  where
  type
    MethodAPI pkg sname ('Method mname ('ArgSingle anm aref ': rest) r) (t -> mr) =
      ReqBody '[JSON] t :> MethodAPI pkg sname ('Method mname rest r) mr
  servantMethodHandler f pkgP snameP _ h t =
    servantMethodHandler f pkgP snameP (Proxy @('Method mname rest r)) (h t)

instance
  (MonadServer m, ServantMethodHandler pkg sname m ('Method mname rest r) mr) =>
  ServantMethodHandler pkg sname m ('Method mname ('ArgStream anm aref ': rest) r) (ConduitT () t m () -> mr)
  where
  type
    MethodAPI pkg sname ('Method mname ('ArgStream anm aref ': rest) r) (ConduitT () t m () -> mr) =
      StreamBody NewlineFraming JSON (SourceIO t) :> MethodAPI pkg sname ('Method mname rest r) mr
  servantMethodHandler f pkgP snameP _ h =
    servantMethodHandler f pkgP snameP (Proxy @('Method mname rest r)) . h . sourceToSource

sourceToSource :: (MonadServer m) => SourceIO t -> ConduitT () t m ()
sourceToSource (SourceT src) = ConduitT (PipeM (liftIO $ src (pure . go)) >>=)
  where
    go :: (MonadServer m) => StepT IO t -> Pipe i i t u m ()
    go Stop = Done ()
    go (Skip s) = go s
    go (Yield t s) = HaveOutput (go s) t
    go (Effect m) = PipeM (liftIO $ go <$> m)
    go (Servant.Types.SourceT.Error msg) =
      PipeM (throwError $ Mu.Server.ServerError Invalid ("error reading stream: " ++ msg))

type ServantRoute = [Symbol]

type family RouteFor (pkg :: Package snm mnm anm tyref) (s :: Symbol) (m :: Symbol) :: ServantRoute where
  RouteFor pkg s m =
    Concat
      (FromMaybe '[s] (GetServiceAnnotationMay (AnnotatedPackage ServantRoute pkg) s))
      (FromMaybe '[m] (GetMethodAnnotationMay (AnnotatedPackage ServantRoute pkg) s m))

type family FromMaybe (a :: k) (ma :: Maybe k) :: k where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just a) = a

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
  Concat '[] bs = bs
  Concat (a ': as) bs = a ': Concat as bs

type family PrefixRoute (prefix :: ServantRoute) route where
  PrefixRoute '[] route = route
  PrefixRoute (p ': rest) route = p :> PrefixRoute rest route
