{-# language ConstraintKinds       #-}
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

{-|
Description : Execute a Mu 'Server' using Servant

This module allows you to serve a Mu 'Server'
as an OpenAPI / Swagger / REST end-point.
In particular, it translates to the kind of
type-level APIs used by Servant.
-}
module Mu.Servant.Server (
  -- * Convert Mu to Servant
  servantServerHandlers,
  servantServerHandlersExtra,
  toHandler,
  packageAPI,
  swagger,
  -- * Required annotations
  ServantRoute(..),
  DefaultServantContentTypes,
  ServantContentTypes(..),
  ServantStreamContentType(..),
  -- Reexports
  StdMethod(..),
  module Servant.API
) where

import           Conduit
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LB8
import           Data.Conduit.Internal     (ConduitT (..), Pipe (..))
import           Data.Kind
import           Data.Swagger              (Swagger, ToSchema (..))
import           Generics.Generic.Aeson
import           GHC.Generics
import           GHC.TypeLits
import           GHC.Types                 (Any)
import           Mu.Rpc
import           Mu.Rpc.Annotations
import           Mu.Schema
import           Mu.Schema.Annotations
import           Mu.Server
import           Servant
import           Servant.API
import           Servant.Swagger
import           Servant.Types.SourceT

-- | Reinterprets a Mu server action as a Servant handler.
toHandler :: ServerErrorIO a -> Handler a
toHandler = Handler . withExceptT convertServerError

-- | Translates a Mu `Mu.Server.ServerError` into a Servant `Servant.ServerError`.
convertServerError :: Mu.Server.ServerError -> Servant.ServerError
convertServerError (Mu.Server.ServerError code msg) = case code of
  Unknown         -> err502 {errBody = LB8.fromString msg}
  Unavailable     -> err503 {errBody = LB8.fromString msg}
  Unimplemented   -> err501 {errBody = LB8.fromString msg}
  Unauthenticated -> err401 {errBody = LB8.fromString msg}
  Internal        -> err500 {errBody = LB8.fromString msg}
  Invalid         -> err400 {errBody = LB8.fromString msg}
  NotFound        -> err404 {errBody = LB8.fromString msg}

-- | Converts a Mu server into Servant server
--   by running all Mu handler actions in the `Handler` type.
--   This version assumes /no/ additional routes
--   in the Servant server when compared to Mu's.
servantServerHandlers ::
  forall pname m chn ss handlers.
  ( ServantServiceHandlers
      ('Package pname ss)
      m
      chn
      ss
      handlers
  , ExtraFor ('Package pname ss) ~ EmptyAPI
  )
  => (forall a. m a -> Handler a) -- ^ how to turn the inner Mu monad into 'Handler', use 'toHandler' (or a composition with it) in most cases
  -> Mu.Server.ServerT chn () ('Package pname ss) m handlers  -- ^ server to be converted
  -> Servant.Server (PackageAPI ('Package pname ss) handlers)
servantServerHandlers f (Services svcs) =
  emptyServer :<|> servantServiceHandlers f (Proxy @('Package pname ss)) svcs

-- | Converts a Mu server into Servant server
--   by running all Mu handler actions in the `Handler` type.
--   This version should be used when additional
--   routes have been added in the Servant version.
servantServerHandlersExtra ::
  forall pname m chn ss handlers.
  ( ServantServiceHandlers
      ('Package pname ss)
      m
      chn
      ss
      handlers
  )
  => (forall a. m a -> Handler a) -- ^ how to turn the inner Mu monad into 'Handler', use 'toHandler' (or a composition with it) in most cases
  -> Server (ExtraFor ('Package pname ss)) -- ^ additional handler for the extra route
  -> Mu.Server.ServerT chn () ('Package pname ss) m handlers  -- ^ server to be converted
  -> Servant.Server (PackageAPI ('Package pname ss) handlers)
servantServerHandlersExtra f extra (Services svcs) =
  extra :<|> servantServiceHandlers f (Proxy @('Package pname ss)) svcs

-- | Converts the information from a Mu server
--   into a 'Swagger' document.
swagger :: forall pname ss handlers chn m.
           HasSwagger (ServicesAPI ('Package pname ss) ss handlers)
        => Mu.Server.ServerT chn () ('Package pname ss) m handlers
        -> Swagger
swagger _ = toSwagger (Proxy @(ServicesAPI ('Package pname ss) ss handlers))

-- | Obtains a Servant API 'Proxy' value for use
--   with functions like 'serve' and 'layout'.
packageAPI :: Mu.Server.ServerT chn t pkg s handlers -> Proxy (PackageAPI pkg handlers)
packageAPI _ = Proxy

type family PackageAPI (pkg :: Package snm mnm anm (TypeRef snm)) handlers where
  PackageAPI ('Package pnm ss) handlers = PackageAPI' (ExtraFor ('Package pnm ss)) ('Package pnm ss) handlers

type family PackageAPI' (extra :: Type) (pkg :: Package snm mnm anm (TypeRef snm)) handlers where
  PackageAPI' extra ('Package pnm ss) handlers = extra :<|> ServicesAPI ('Package pnm ss) ss handlers

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

instance
  ServantMethodHandlers pkg svc m chn inh '[] '[] where
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

class
  ServantMethodHandler
    (httpMethod :: StdMethod)
    (httpStatus :: Nat)
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
      -- according to https://github.com/haskell-servant/servant/issues/683
      -- we always need a content type for NoContent
      Verb httpMethod httpStatus '[JSON] NoContent
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

-- | represents a single element that will be streamed from the server to the client. That element will either be a `Result` containing a return value, or an `Error` indicating that something went wrong. Without this wrapper, server streams that encountered an error after the response headers have been sent would simply terminate without communicating to the client that anything went wrong.
data StreamResult a = Error String | Result a
  deriving (Generic, Show)

instance Data.Swagger.ToSchema a => Data.Swagger.ToSchema (StreamResult a)
instance ToJSON a => ToJSON (StreamResult a) where
  toJSON = gtoJson

-- converts a conduit sink into a Servant SourceIO for interoperating with server streaming handlers
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

-- converts a Servant SourceIO into a conduit for interoperating with client streaming handlers
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

-- | ServantRoute represents the URL path components of a route. It is used as an `AnnotatedPackage` domain to override the default path for a `Method`. When used in an `AnnService`, the specified `TopLevelRoute` is used as a prefix for all `Method`s in that `Service`.
-- 1. List of components for the route,
-- 2. HTTP method which must be used,
-- 3. HTTP status code of a successful HTTP response from a specific `Method`. Use 200 for the usual status code.
data ServantRoute
  = ServantAdditional Type
  | ServantTopLevelRoute [Symbol]
  | ServantRoute [Symbol] StdMethod Nat

type family Assert (err :: Constraint) (break :: k1) (a :: k2) :: k2 where
  -- these cases exist to force evaluation of the "break" parameter when it either has kind [RpcAnnotation ...] or [Annotation ...]
  Assert _ '[ 'AnnSchema a, 'AnnSchema a ] _ = Any
  Assert _ '[ 'AnnPackage a, 'AnnPackage a ] _ = Any
  -- this case should be used whenever "break" is not stuck
  Assert _ _ a = a

-- a helper type synonym used to provide better errors when a required AnnotatedPackage instance doesn't exist
type WithAnnotatedPackageInstance domain pkg a =
  Assert (NoPackageAnnotations domain pkg) (AnnotatedPackage domain pkg) a

-- a helper type synonym used to provide better errors when a required AnnotatedSchema instance doesn't exist
type WithAnnotatedSchemaInstance domain sch a =
  Assert (NoSchemaAnnotations domain sch) (AnnotatedSchema domain sch) a


-- a helper type family for generating custom error messages about missing AnnotatedPackage instances
type family NoPackageAnnotations domain pkg :: Constraint where
  NoPackageAnnotations domain ('Package ('Just pname) _)
    = TypeError (
        'Text "Missing required AnnotatedPackage " ':<>: 'ShowType domain ':<>: 'Text " type instance" ':$$:
        'Text "for " ':<>: 'ShowType pname ':<>: 'Text " package"
      )
  NoPackageAnnotations domain pkg
    = TypeError (
        'Text "Missing required AnnotatedPackage " ':<>: 'ShowType domain ':<>: 'Text " type instance" ':$$:
        'Text "for unnamed package: " ':$$: 'ShowType pkg
      )

-- a helper type family for generating custom error messages about missing AnnotatedSchema instances
type family NoSchemaAnnotations domain sch :: Constraint where
  NoSchemaAnnotations domain sch
    = TypeError (
        'Text "Missing required AnnotatedSchema " ':<>: 'ShowType domain ':<>: 'Text " type instance" ':$$:
        'Text "for schema:" ':$$: 'ShowType sch
      )

-- used to construct a route for a specific method m of service s in package pkg from the @AnnotatedPackage ServantRoute pkg@ instance, along with a custom error message
type family RouteFor (pkg :: Package snm mnm anm tyref) (s :: Symbol) (m :: Symbol) :: [Symbol] where
  RouteFor pkg s m =
    WithAnnotatedPackageInstance ServantRoute pkg (
      Concat
        (UnwrapServantRoute (FromMaybe ('ServantRoute '[s] Any Any) (GetServiceAnnotationMay (AnnotatedPackage ServantRoute pkg) s)))
        (UnwrapServantRoute (FromMaybe ('ServantRoute '[m] Any Any) (GetMethodAnnotationMay (AnnotatedPackage ServantRoute pkg) s m)))
    )

type family UnwrapServantRoute s where
  UnwrapServantRoute ('ServantTopLevelRoute s) = s
  UnwrapServantRoute ('ServantRoute s _ _)     = s

type family ExtraFor (pkg :: Package snm mnm anm tyref) :: Type where
  ExtraFor pkg =
    WithAnnotatedPackageInstance ServantRoute pkg
      (UnwrapServantExtra (FromMaybe ('ServantAdditional EmptyAPI) (GetPackageAnnotationMay (AnnotatedPackage ServantRoute pkg))))

type family UnwrapServantExtra s where
  UnwrapServantExtra ('ServantAdditional e) = e

type family FromMaybe (a :: k) (ma :: Maybe k) :: k where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just a) = a

type family Concat (as :: [k]) (bs :: [k]) :: [k] where
  Concat '[] bs = bs
  Concat (a ': as) bs = a ': Concat as bs

type family PrefixRoute (prefix :: [Symbol]) route where
  PrefixRoute '[] route = route
  PrefixRoute (p ': rest) route = p :> PrefixRoute rest route

-- | ServantContentTypes represents that acceptable content types that can be used when a message in encoded:
-- 1. in a unary (non-streaming) HTTP request\/response body,
-- 2. encoded in a streaming HTTP request\/response body.
-- It is used as an `AnnotatedSchema` domain.
data ServantContentTypes
  = ServantContentTypes
      { unary  :: [Type]
      , stream :: Maybe ServantStreamContentType
      }

type DefaultServantContentTypes
  = 'ServantContentTypes '[JSON] ('Just ('ServantStreamContentType NewlineFraming JSON))

data ServantStreamContentType
  = ServantStreamContentType
      { framing           :: Type,
        streamContentType :: Type
      }

-- extracts a StdMethod from a ServantMethod annotation of a given method, defaulting to POST if such an annotation doesn't exist
type family HttpMethodFor pkg sname mname :: StdMethod where
  HttpMethodFor pkg sname mname =
    WithAnnotatedPackageInstance ServantRoute pkg (
      UnwrapServantMethod (FromMaybe ('ServantRoute Any 'POST Any) (GetMethodAnnotationMay (AnnotatedPackage ServantRoute pkg) sname mname))
    )

type family UnwrapServantMethod m where
  UnwrapServantMethod ('ServantRoute _ m _) = m

-- extracts the HTTP status code from the ServantStatus annotation of a given method, or defaults to 200 if such an annotation doesn't exist
type family HttpStatusFor pkg sname mname :: Nat where
  HttpStatusFor pkg sname mname =
    WithAnnotatedPackageInstance ServantRoute pkg (
      UnwrapServantStatus (FromMaybe ('ServantRoute Any Any 200) (GetMethodAnnotationMay (AnnotatedPackage ServantRoute pkg) sname mname))
    )

type family UnwrapServantStatus s where
  UnwrapServantStatus ('ServantRoute _ _ s) = s

-- extracts a list of content types from a ServantUnaryContentTypes annotation of a given method
type family UnaryContentTypesFor (tyRef :: TypeRef sname) :: [Type] where
  UnaryContentTypesFor ('SchemaRef schema typeName) =
    WithAnnotatedSchemaInstance ServantContentTypes schema (
      UnwrapServantUnaryContentType (GetTypeAnnotation (AnnotatedSchema ServantContentTypes schema) typeName)
    )

type family UnwrapServantUnaryContentType (sctype :: ServantContentTypes) :: [Type] where
  UnwrapServantUnaryContentType ('ServantContentTypes ctype stype) = ctype

-- extracts a content type from a ServantStreamContentType annotation of a given method
type family StreamContentTypeFor (tyRef :: TypeRef sname) :: Type where
  StreamContentTypeFor ('SchemaRef schema typeName) =
    WithAnnotatedSchemaInstance ServantContentTypes schema (
      StreamContentType (GetTypeAnnotation (AnnotatedSchema ServantContentTypes schema) typeName)
    )

type family StreamContentType (sct :: ServantContentTypes) where
  StreamContentType ('ServantContentTypes _ 'Nothing)
    = TypeError ('Text "missing stream content type")
  StreamContentType ('ServantContentTypes _ ('Just ('ServantStreamContentType _ ctype))) = ctype

-- extracts a framing from a ServantStreamContentType annotation of a given method
type family StreamFramingFor (tyRef :: TypeRef sname) :: Type where
  StreamFramingFor ('SchemaRef schema typeName) =
    WithAnnotatedSchemaInstance ServantContentTypes schema (
      StreamFraming (GetTypeAnnotation (AnnotatedSchema ServantContentTypes schema) typeName)
    )

type family StreamFraming (sct :: ServantContentTypes) where
  StreamFraming ('ServantContentTypes _ 'Nothing)
    = TypeError ('Text "missing stream content type")
  StreamFraming ('ServantContentTypes _ ('Just ('ServantStreamContentType framing _))) = framing
