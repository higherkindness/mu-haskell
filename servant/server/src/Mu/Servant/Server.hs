{-# language DataKinds             #-}
{-# language DeriveGeneric         #-}
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
{-# OPTIONS_GHC -Wall #-}

module Mu.Servant.Server where

import           Conduit
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LB8
import           Data.Conduit.Internal     (ConduitT (..), Pipe (..))
import           Data.Kind
import           Generics.Generic.Aeson
import           GHC.Generics
import           GHC.TypeLits
import           Mu.Rpc
import           Mu.Rpc.Annotations
import           Mu.Schema
import           Mu.Schema.Annotations
import           Mu.Server
import           Servant
import           Servant.Types.SourceT

toHandler :: ServerErrorIO a -> Handler a
toHandler = Handler . withExceptT convertServerError

convertServerError :: Mu.Server.ServerError -> Servant.ServerError
convertServerError (Mu.Server.ServerError code msg) = case code of
  Unknown         -> err502 {errBody = LB8.fromString msg}
  Unavailable     -> err503 {errBody = LB8.fromString msg}
  Unimplemented   -> err501 {errBody = LB8.fromString msg}
  Unauthenticated -> err401 {errBody = LB8.fromString msg}
  Internal        -> err500 {errBody = LB8.fromString msg}
  Invalid         -> err400 {errBody = LB8.fromString msg}
  NotFound        -> err404 {errBody = LB8.fromString msg}

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
    (pkg :: Package Symbol Symbol anm (TypeRef Symbol))
    (sname :: Symbol)
    (m :: Type -> Type)
    (chn :: ServiceChain snm)
    (inh :: Type)
    (ms :: [Method snm Symbol anm (TypeRef snm)])
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
  ( ServantMethodHandler httpMethod httpStatus m args ret h,
    ServantMethodHandlers pkg sname m chn () rest hs,
    HttpMethodFor pkg sname mname ~ httpMethod,
    HttpStatusFor pkg sname mname ~ httpStatus,
    Server (MethodAPI pkg sname ('Method mname args ret) h) ~ Server (HandlerAPI httpMethod httpStatus args ret h)
  ) =>
  ServantMethodHandlers pkg sname m chn () ('Method mname args ret ': rest) (h ': hs)
  where
  type
    MethodsAPI pkg sname ('Method mname args ret ': rest) (h ': hs) =
      MethodAPI pkg sname ('Method mname args ret) h
        :<|> MethodsAPI pkg sname rest hs
  servantMethodHandlers f pkgP snameP (Hmore _ _ h rest) =
    servantMethodHandler
      f
      (Proxy @httpMethod)
      (Proxy @httpStatus)
      (Proxy @args)
      (Proxy @ret)
      (h NoRpcInfo ())
      :<|> servantMethodHandlers f pkgP snameP rest

type family MethodAPI pkg sname method h where
  MethodAPI pkg sname ('Method mname args ret) h =
    PrefixRoute (RouteFor pkg sname mname)
      ( HandlerAPI
          (HttpMethodFor pkg sname mname)
          (HttpStatusFor pkg sname mname)
          args
          ret
          h
      )

data ServantAPIAnnotations
  = ServantAPIAnnotations
      { method      :: ServantMethod,
        status      :: ServantStatus,
        contentType :: Type
      }

newtype ServantUnaryContentTypes = ServantUnaryContentTypes [Type]

data ServantStreamContentType
  = ServantStreamContentType
      { framing           :: Type,
        streamContentType :: Type
      }

type family HttpMethodFor pkg sname mname :: ServantMethod where
  HttpMethodFor pkg sname mname =
    FromMaybe 'POST (GetMethodAnnotationMay (AnnotatedPackage ServantMethod pkg) sname mname)

type family HttpStatusFor pkg sname mname :: ServantStatus where
  HttpStatusFor pkg sname mname =
    FromMaybe 200 (GetMethodAnnotationMay (AnnotatedPackage ServantStatus pkg) sname mname)

type family UnaryContentTypesFor (tyRef :: TypeRef sname) :: [Type] where
  UnaryContentTypesFor ('SchemaRef schema typeName) =
    UnwrapServantUnaryContentType (GetTypeAnnotation (AnnotatedSchema ServantUnaryContentTypes schema) typeName)

type family UnwrapServantUnaryContentType (sctype :: ServantUnaryContentTypes) :: [Type] where
  UnwrapServantUnaryContentType ('ServantUnaryContentTypes ctype) = ctype

type family StreamContentTypeFor (tyRef :: TypeRef sname) :: Type where
  StreamContentTypeFor ('SchemaRef schema typeName) =
    StreamContentType (GetTypeAnnotation (AnnotatedSchema ServantStreamContentType schema) typeName)

type family StreamContentType (sct :: ServantStreamContentType) where
  StreamContentType ('ServantStreamContentType _ ctype) = ctype

type family StreamFramingFor (tyRef :: TypeRef sname) :: Type where
  StreamFramingFor ('SchemaRef schema typeName) =
    StreamFraming (GetTypeAnnotation (AnnotatedSchema ServantStreamContentType schema) typeName)

type family StreamFraming (sct :: ServantStreamContentType) where
  StreamFraming ('ServantStreamContentType framing _) = framing

type ServantMethod = StdMethod

type ServantStatus = Nat

class
  ServantMethodHandler
    (httpMethod :: ServantMethod)
    (httpStatus :: ServantStatus)
    (m :: Type -> Type)
    (args :: [Argument snm anm (TypeRef snm)])
    (ret :: Return snm (TypeRef snm))
    (h :: Type) where
  type
    HandlerAPI
      httpMethod
      httpStatus
      args
      ret
      h
  servantMethodHandler ::
    (forall a. m a -> Handler a) ->
    Proxy httpMethod ->
    Proxy httpStatus ->
    Proxy args ->
    Proxy ret ->
    h ->
    Servant.Server (HandlerAPI httpMethod httpStatus args ret h)

instance ServantMethodHandler httpMethod httpStatus m '[] 'RetNothing (m ()) where
  type
    HandlerAPI httpMethod httpStatus '[] 'RetNothing (m ()) =
      Verb httpMethod httpStatus '[] NoContent
  servantMethodHandler f _ _ _ _ = fmap (const NoContent) . f

instance ServantMethodHandler httpMethod httpStatus m '[] ('RetSingle rref) (m r) where
  type
    HandlerAPI httpMethod httpStatus '[] ('RetSingle rref) (m r) =
      Verb httpMethod httpStatus (UnaryContentTypesFor rref) r
  servantMethodHandler f _ _ _ _ = f

instance
  (MonadServer m) =>
  ServantMethodHandler httpMethod httpStatus m '[] ('RetStream rref) (ConduitT r Void m () -> m ())
  where
  type
    HandlerAPI httpMethod httpStatus '[] ('RetStream rref) (ConduitT r Void m () -> m ()) =
      Stream httpMethod httpStatus (StreamFramingFor rref) (StreamContentTypeFor rref) (SourceIO (StreamResult r))
  servantMethodHandler f _ _ _ _ = liftIO . sinkToSource f

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
  (ServantMethodHandler httpMethod httpStatus m rest ret h) =>
  ServantMethodHandler httpMethod httpStatus m ('ArgSingle anm aref ': rest) ret (t -> h)
  where
  type
    HandlerAPI httpMethod httpStatus ('ArgSingle anm aref ': rest) ret (t -> h) =
      ReqBody (UnaryContentTypesFor aref) t :> HandlerAPI httpMethod httpStatus rest ret h
  servantMethodHandler f mP sP _ retP h t =
    servantMethodHandler f mP sP (Proxy @rest) retP (h t)

instance
  (MonadServer m, ServantMethodHandler httpMethod httpStatus m rest ret h) =>
  ServantMethodHandler httpMethod httpStatus m ('ArgStream anm aref ': rest) ret (ConduitT () t m () -> h)
  where
  type
    HandlerAPI httpMethod httpStatus ('ArgStream anm aref ': rest) ret (ConduitT () t m () -> h) =
      StreamBody (StreamFramingFor aref) (StreamContentTypeFor aref) (SourceIO t)
        :> HandlerAPI httpMethod httpStatus rest ret h
  servantMethodHandler f mP sP _ retP h =
    servantMethodHandler f mP sP (Proxy @rest) retP . h . sourceToSource

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
