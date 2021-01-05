{-# language ConstraintKinds       #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLists       #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language RankNTypes            #-}
{-# language ScopedTypeVariables   #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fprint-explicit-foralls #-}
module Mu.GraphQL.Query.Run (
  GraphQLApp
, runPipeline
, runSubscriptionPipeline
, runDocument
, runQuery
, runSubscription
-- * Typeclass to be able to run query handlers
, RunQueryFindHandler
) where

import           Control.Concurrent.STM.TMQueue
import           Control.Monad.Except           (MonadError, runExceptT)
import           Control.Monad.Writer
import qualified Data.Aeson                     as Aeson
import qualified Data.Aeson.Types               as Aeson
import           Data.Conduit
import           Data.Conduit.Combinators       (sinkList, yieldMany)
import           Data.Conduit.TQueue
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Typeable
import           GHC.TypeLits
import qualified Language.GraphQL.AST           as GQL
import           Network.HTTP.Types.Header
import           Unsafe.Coerce                  (unsafeCoerce)

import           Mu.GraphQL.Query.Definition
import qualified Mu.GraphQL.Query.Introspection as Intro
import           Mu.GraphQL.Query.Parse
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

data GraphQLError
  = GraphQLError ServerError [T.Text]

type GraphQLApp p qr mut sub m chn hs
  = (ParseTypedDoc p qr mut sub, RunDocument p qr mut sub m chn hs)

runPipeline
  :: forall qr mut sub p m chn hs. GraphQLApp p qr mut sub m chn hs
  => (forall a. m a -> ServerErrorIO a)
  -> RequestHeaders
  -> ServerT chn GQL.Field p m hs
  -> Proxy qr -> Proxy mut -> Proxy sub
  -> Maybe T.Text -> VariableMapC -> [GQL.Definition]
  -> IO Aeson.Value
runPipeline f req svr _ _ _ opName vmap doc
  = case parseDoc @qr @mut @sub opName vmap doc of
      Left e -> pure $ singleErrValue e
      Right (d :: Document p qr mut sub) -> do
        (data_, errors) <- runWriterT (runDocument f req svr d)
        case errors of
          [] -> pure $ Aeson.object [ ("data", data_) ]
          _  -> pure $ Aeson.object [ ("data", data_), ("errors", Aeson.listValue errValue errors) ]

runSubscriptionPipeline
  :: forall qr mut sub p m chn hs. GraphQLApp p qr mut sub m chn hs
  => (forall a. m a -> ServerErrorIO a)
  -> RequestHeaders
  -> ServerT chn GQL.Field p m hs
  -> Proxy qr -> Proxy mut -> Proxy sub
  -> Maybe T.Text -> VariableMapC -> [GQL.Definition]
  -> ConduitT Aeson.Value Void IO ()
  -> IO ()
runSubscriptionPipeline f req svr _ _ _ opName vmap doc sink
  = case parseDoc @qr @mut @sub opName vmap doc of
      Left e
        -> yieldSingleError e sink
      Right (d :: Document p qr mut sub)
        -> runDocumentSubscription f req svr d sink

singleErrValue :: T.Text -> Aeson.Value
singleErrValue e
  = Aeson.object [ ("errors", Aeson.Array [
                       Aeson.object [ ("message", Aeson.String e) ] ])]

errValue :: GraphQLError -> Aeson.Value
errValue (GraphQLError (ServerError _ msg) path)
  = Aeson.object [
      ("message", Aeson.String $ T.pack msg)
    , ("path", Aeson.toJSON path)
    ]

yieldSingleError :: Monad m
                 => T.Text -> ConduitM Aeson.Value Void m () -> m ()
yieldSingleError e sink =
  runConduit $ yieldMany ([singleErrValue e] :: [Aeson.Value]) .| sink

yieldError :: Monad m
           => ServerError -> [T.Text]
           -> ConduitM Aeson.Value Void m () -> m ()
yieldError e path sink = do
  let val = Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ]
  runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink

class RunDocument (p :: Package')
                  (qr :: Maybe Symbol)
                  (mut :: Maybe Symbol)
                  (sub :: Maybe Symbol)
                  m chn hs where
  runDocument ::
       (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m hs
    -> Document p qr mut sub
    -> WriterT [GraphQLError] IO Aeson.Value
  runDocumentSubscription ::
       (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m hs
    -> Document p qr mut sub
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  , KnownSymbol mut
  , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
  , MappingRight chn mut ~ ()
  , KnownSymbol sub
  , RunQueryFindHandler m p hs chn ss (LookupService ss sub) hs
  , MappingRight chn sub ~ ()
  , Intro.Introspect p ('Just qr) ('Just mut) ('Just sub)
  ) => RunDocument p ('Just qr) ('Just mut) ('Just sub) m chn hs where
  runDocument f req svr d
    = let i = Intro.introspect (Proxy @p) (Proxy @('Just qr)) (Proxy @('Just mut)) (Proxy @('Just sub))
      in case d of
           QueryDoc q
             -> runQuery f req i svr [] () q
           MutationDoc q
             -> runQuery f req i svr [] () q
           SubscriptionDoc _
             -> pure $ singleErrValue "cannot execute subscriptions in this wire"
  runDocumentSubscription f req svr (SubscriptionDoc d)
    = runSubscription f req svr [] () d
  runDocumentSubscription f req svr d = yieldDocument f req svr d

instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  , KnownSymbol mut
  , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
  , MappingRight chn mut ~ ()
  , Intro.Introspect p ('Just qr) ('Just mut) 'Nothing
  ) => RunDocument p ('Just qr) ('Just mut) 'Nothing m chn hs where
  runDocument f req svr d
    = let i = Intro.introspect (Proxy @p) (Proxy @('Just qr)) (Proxy @('Just mut)) (Proxy @'Nothing)
      in case d of
           QueryDoc q
             -> runQuery f req i svr [] () q
           MutationDoc q
             -> runQuery f req i svr [] () q
  runDocumentSubscription = yieldDocument

instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  , KnownSymbol sub
  , RunQueryFindHandler m p hs chn ss (LookupService ss sub) hs
  , MappingRight chn sub ~ ()
  , Intro.Introspect p ('Just qr) 'Nothing ('Just sub)
  ) => RunDocument p ('Just qr) 'Nothing ('Just sub) m chn hs where
  runDocument f req svr d
    = let i = Intro.introspect (Proxy @p) (Proxy @('Just qr)) (Proxy @'Nothing) (Proxy @('Just sub))
      in case d of
           QueryDoc q
             -> runQuery f req i svr [] () q
           SubscriptionDoc _
             -> pure $ singleErrValue "cannot execute subscriptions in this wire"
  runDocumentSubscription f req svr (SubscriptionDoc d)
    = runSubscription f req svr [] () d
  runDocumentSubscription f req svr d = yieldDocument f req svr d

instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  , Intro.Introspect p ('Just qr) 'Nothing 'Nothing
  ) => RunDocument p ('Just qr) 'Nothing 'Nothing m chn hs where
  runDocument f req svr d
    = let i = Intro.introspect (Proxy @p) (Proxy @('Just qr)) (Proxy @'Nothing) (Proxy @'Nothing)
      in case d of
           QueryDoc q
             -> runQuery f req i svr [] () q
  runDocumentSubscription = yieldDocument

instance
  ( TypeError ('Text "you need to have a query in your schema")
  ) => RunDocument p 'Nothing mut sub m chn hs where
  runDocument _ = error "this should never be called"
  runDocumentSubscription _ = error "this should never be called"

yieldDocument ::
     forall p qr mut sub m chn hs.
     RunDocument p qr mut sub m chn hs
  => (forall a. m a -> ServerErrorIO a)
  -> RequestHeaders
  -> ServerT chn GQL.Field p m hs
  -> Document p qr mut sub
  -> ConduitT Aeson.Value Void IO ()
  -> IO ()
yieldDocument f req svr doc sink = do
  (data_, errors) <- runWriterT (runDocument @p @qr @mut @sub @m @chn @hs f req svr doc)
  let (val :: Aeson.Value)
        = case errors of
            [] -> Aeson.object [ ("data", data_) ]
            _  -> Aeson.object [ ("data", data_), ("errors", Aeson.listValue errValue errors) ]
  runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink

runQuery
  :: forall m p s pname ss hs sname ms chn inh.
     ( RunQueryFindHandler m p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname ms
     , inh ~ MappingRight chn sname )
  => (forall a. m a -> ServerErrorIO a)
  -> RequestHeaders
  -> Intro.Schema -> ServerT chn GQL.Field p m hs
  -> [T.Text]
  -> inh
  -> ServiceQuery p s
  -> WriterT [GraphQLError] IO Aeson.Value
runQuery f req sch whole@(Services ss) path = runQueryFindHandler f req sch whole path ss

runSubscription
  :: forall m p s pname ss hs sname ms chn inh.
     ( RunQueryFindHandler m p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname ms
     , inh ~ MappingRight chn sname )
  => (forall a. m a -> ServerErrorIO a)
  -> RequestHeaders
  -> ServerT chn GQL.Field p m hs
  -> [T.Text]
  -> inh
  -> OneMethodQuery p s
  -> ConduitT Aeson.Value Void IO ()
  -> IO ()
runSubscription f req whole@(Services ss) path
  = runSubscriptionFindHandler f req whole path ss

class RunQueryFindHandler m p whole chn ss s hs where
  runQueryFindHandler
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn (ServiceName s) )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> Intro.Schema -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> ServicesT chn GQL.Field ss m hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value
  runSubscriptionFindHandler
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn (ServiceName s) )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> ServicesT chn GQL.Field ss m hs
    -> inh
    -> OneMethodQuery p s
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

class RunQueryOnFoundHandler m p whole chn (s :: Service snm mnm anm (TypeRef snm)) hs where
  type ServiceName s :: snm
  runQueryOnFoundHandler
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn (ServiceName s) )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> Intro.Schema -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> ServiceT chn GQL.Field s m hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value
  runSubscriptionOnFoundHandler
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn (ServiceName s) )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> ServiceT chn GQL.Field s m hs
    -> inh
    -> OneMethodQuery p s
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler m p whole chn '[] s '[] where
  runQueryFindHandler _ = error "this should never be called"
  runSubscriptionFindHandler _ = error "this should never be called"
instance {-# OVERLAPPABLE #-}
         RunQueryFindHandler m p whole chn ss s hs
         => RunQueryFindHandler m p whole chn (other ': ss) s (h ': hs) where
  runQueryFindHandler f req sch whole path (_ :<&>: that)
    = runQueryFindHandler f req sch whole path that
  runSubscriptionFindHandler f req whole path (_ :<&>: that)
    = runSubscriptionFindHandler f req whole path that
instance {-# OVERLAPS #-}
         (RunQueryOnFoundHandler m p whole chn s h)
         => RunQueryFindHandler m p whole chn (s ': ss) s (h ': hs) where
  runQueryFindHandler f req sch whole path (s :<&>: _)
    = runQueryOnFoundHandler f req sch whole path s
  runSubscriptionFindHandler f req whole path (s :<&>: _)
    = runSubscriptionOnFoundHandler f req whole path s

instance ( KnownName sname, RunMethod m p whole chn ('Service sname ms) ms h )
         => RunQueryOnFoundHandler m p whole chn ('Service sname ms) h where
  type ServiceName ('Service sname ms) = sname
  runQueryOnFoundHandler f req sch whole path (ProperSvc this) inh (ServiceQuery queries)
    = Aeson.object . catMaybes <$> mapM runOneQuery queries
    where
      -- if we include the signature we have to write
      -- an explicit type signature for 'runQueryFindHandler'
      runOneQuery (OneMethodQuery nm args)
        = runMethod f req whole (Proxy @('Service sname ms)) path nm inh this args
      -- handle __typename
      runOneQuery (TypeNameQuery nm)
        = let realName = fromMaybe "__typename" nm
          in pure $ Just (realName, Aeson.String $ T.pack $ nameVal (Proxy @sname))
      -- handle __schema
      runOneQuery (SchemaQuery nm ss)
        = do let realName = fromMaybe "__schema" nm
             Just . (realName, ) <$> runIntroSchema path sch ss
      -- handle __type
      runOneQuery (TypeQuery nm ty ss)
        = do let realName = fromMaybe "__schema" nm
             res <- runIntroType path sch (Intro.TypeRef ty) ss
             case res of
               Just val -> pure $ Just (realName, val)
               Nothing  -> do tell [GraphQLError
                                     (ServerError Invalid
                                       $ "cannot find type '" <> T.unpack ty <> "'")
                                    path]
                              pure $ Just (realName, Aeson.Null)
  -- subscriptions should only have one element
  runSubscriptionOnFoundHandler f req whole path (ProperSvc this) inh (OneMethodQuery nm args) sink
    = runMethodSubscription f req whole (Proxy @('Service sname ms)) path nm inh this args sink
  runSubscriptionOnFoundHandler _ _ _ _ _ _ (TypeNameQuery nm) sink
    = let realName = fromMaybe "__typename" nm
          o = Aeson.object [(realName, Aeson.String $ T.pack $ nameVal (Proxy @sname))]
      in runConduit $ yieldMany ([o] :: [Aeson.Value]) .| sink
  runSubscriptionOnFoundHandler _ _ _ _ _ _ _ sink
    = runConduit $ yieldMany
                   ([singleErrValue "__schema and __type are not supported in subscriptions"]
                      :: [Aeson.Value])
                   .| sink

instance ( KnownName sname, RunUnion m p whole chn elts )
         => RunQueryOnFoundHandler m p whole chn ('OneOf sname elts) h where
  type ServiceName ('OneOf sname elts) = sname
  runQueryOnFoundHandler f req sch whole path (OneOfSvc this) inh (OneOfQuery queries)
    = do res <- liftIO $ runExceptT $ f $ this inh
         case res of
          Left e  -> tell [GraphQLError e path] >> pure Aeson.Null
          Right x -> runUnion f req sch whole path queries x

class RunUnion m p whole chn elts where
  runUnion
    :: (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> Intro.Schema -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> NP (ChosenOneOfQuery p) elts
    -> UnionChoice chn elts
    -> WriterT [GraphQLError] IO Aeson.Value

instance RunUnion m p whole chn '[] where
  runUnion _ = error "this should never happen"
instance forall m p pname s sname whole ss chn elts ms.
         ( RunQueryFindHandler m p whole chn ss s whole
         , p ~ 'Package pname ss
         , s ~ LookupService ss sname
         , s ~ 'Service sname ms
         , RunUnion m p whole chn elts )
         => RunUnion m p whole chn (sname ': elts) where
  runUnion f req sch whole path
           (ChosenOneOfQuery (Proxy :: Proxy sname) q :* rest)
           choice@(UnionChoice (Proxy :: Proxy other) v)
    = case eqT @sname @other of
        Nothing   -> runUnion f req sch whole path rest (unsafeCoerce choice)
        Just Refl -> runQuery @m @('Package pname ss) @(LookupService ss sname) @pname @ss @whole @sname @ms f req sch whole path v q

class RunMethod m p whole chn s ms hs where
  runMethod
    :: ( p ~ 'Package pname wholess
       , s ~ 'Service sname allMs
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> Proxy s -> [T.Text] -> Maybe T.Text -> inh
    -> HandlersT chn GQL.Field inh ms m hs
    -> NS (ChosenMethodQuery p) ms
    -> WriterT [GraphQLError] IO (Maybe (T.Text, Aeson.Value))
  runMethodSubscription
    :: ( p ~ 'Package pname wholess
       , s ~ 'Service sname allMs
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> Proxy s -> [T.Text] -> Maybe T.Text -> inh
    -> HandlersT chn GQL.Field inh ms m hs
    -> NS (ChosenMethodQuery p) ms
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance RunMethod m p whole chn s '[] '[] where
  runMethod _ = error "this should never be called"
  runMethodSubscription _ = error "this should never be called"
instance ( RunMethod m p whole chn s ms hs
         , KnownName mname
         , RunHandler m p whole chn args r h
         , ReflectRpcInfo p s ('Method mname args r) )
         => RunMethod m p whole chn s ('Method mname args r ': ms) (h ': hs) where
  -- handle normal methods
  runMethod f req whole _ path nm inh (h :<||>: _) (Z (ChosenMethodQuery fld args ret))
    = ((realName ,) <$>) <$> runHandler f req whole (path ++ [realName]) (h rpcInfo inh) args ret
    where realName = fromMaybe (T.pack $ nameVal (Proxy @mname)) nm
          rpcInfo = reflectRpcInfo (Proxy @p) (Proxy @s) (Proxy @('Method mname args r)) req fld
  runMethod f req whole p path nm inh (_ :<||>: r) (S cont)
    = runMethod f req whole p path nm inh r cont
  runMethod _ _ _ _ _ _ _ _ _ = error "this should never happen"
  -- handle subscriptions
  runMethodSubscription f req whole _ path nm inh (h :<||>: _) (Z (ChosenMethodQuery fld args ret)) sink
    = runHandlerSubscription f req whole (path ++ [realName]) (h rpcInfo inh) args ret sink
    where realName = fromMaybe (T.pack $ nameVal (Proxy @mname)) nm
          rpcInfo = reflectRpcInfo (Proxy @p) (Proxy @s) (Proxy @('Method mname args r)) req fld
  runMethodSubscription f req whole p path nm inh (_ :<||>: r) (S cont) sink
    = runMethodSubscription f req whole p path nm inh r cont sink
  runMethodSubscription _ _ _ _ _ _ _ _ _ _ = error "this should never happen"

class Handles chn args r m h
      => RunHandler m p whole chn args r h where
  runHandler
    :: (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> h
    -> NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
  runHandlerSubscription
    :: (forall a. m a -> ServerErrorIO a)
    -> RequestHeaders
    -> ServerT chn GQL.Field p m whole
    -> [T.Text]
    -> h
    -> NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance (ArgumentConversion chn ref t, RunHandler m p whole chn rest r h)
         => RunHandler m p whole chn ('ArgSingle aname ref ': rest) r (t -> h) where
  runHandler f req whole path h (ArgumentValue one :* rest)
    = runHandler f req whole path (h (convertArg (Proxy @chn) one)) rest
  runHandlerSubscription f req whole path h (ArgumentValue one :* rest)
    = runHandlerSubscription f req whole path (h (convertArg (Proxy @chn) one)) rest
instance ( MonadError ServerError m
         , FromRef chn ref t
         , ArgumentConversion chn ('ListRef ref) [t]
         , RunHandler m p whole chn rest r h )
         => RunHandler m p whole chn ('ArgStream aname ref ': rest) r (ConduitT () t m () -> h) where
  runHandler f req whole path h (ArgumentStream lst :* rest)
    = let converted :: [t] = convertArg (Proxy @chn) lst
      in runHandler f req whole path (h (yieldMany converted)) rest
  runHandlerSubscription f req whole path h (ArgumentStream lst :* rest) sink
    = let converted :: [t] = convertArg (Proxy @chn) lst
      in runHandlerSubscription f req whole path (h (yieldMany converted)) rest sink
instance (MonadError ServerError m)
         => RunHandler m p whole chn '[] 'RetNothing (m ()) where
  runHandler f _req _ path h Nil _ = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right _ -> pure $ Just Aeson.Null
      Left e  -> tell [GraphQLError e path] >> pure Nothing
  runHandlerSubscription f _req _ path h Nil _ sink = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right _ -> runConduit $ yieldMany ([] :: [Aeson.Value]) .| sink
      Left e  -> yieldError e path sink
instance (MonadError ServerError m, ResultConversion m p whole chn r l)
         => RunHandler m p whole chn '[] ('RetSingle r) (m l) where
  runHandler f req whole path h Nil (RSingle q) = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right v -> convertResult f req whole path q v
      Left e  -> tell [GraphQLError e path] >> pure Nothing
  runHandlerSubscription f req whole path h Nil (RSingle q) sink = do
    res <- liftIO $ runExceptT (f h)
    val <- case res of
      Right v -> do
        (data_, errors) <- runWriterT (convertResult f req whole path q v)
        case errors of
          [] -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_) ]
          _  -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_)
                                    , ("errors", Aeson.listValue errValue errors) ]
      Left e -> pure $ Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ]
    runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink
instance (MonadIO m, MonadError ServerError m, ResultConversion m p whole chn r l)
         => RunHandler m p whole chn '[] ('RetStream r) (ConduitT l Void m () -> m ()) where
  runHandler f req whole path h Nil (RStream q) = do
    queue <- liftIO newTMQueueIO
    res <- liftIO $ runExceptT $ f $ h (sinkTMQueue queue)
    case res of
      Right _ -> do
        info <- runConduit $ sourceTMQueue queue .| sinkList
        Just . Aeson.toJSON . catMaybes <$> traverse (convertResult f req whole path q) info
      Left e  -> tell [GraphQLError e []] >> pure Nothing
  runHandlerSubscription f req whole path h Nil (RStream q) sink = do
    res <- liftIO $ runExceptT $ f $ h
      (transPipe liftIO (mapInputM convert (error "this should not be called") sink))
    case res of
      Right _ -> return ()
      Left e  -> yieldError e path sink
    where
      convert :: l -> IO Aeson.Value
      convert v = do
        (data_, errors) <- runWriterT (convertResult f req whole path q v)
        case errors of
          [] -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_) ]
          _  -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_)
                                    , ("errors", Aeson.listValue errValue errors) ]

class FromRef chn ref t
      => ArgumentConversion chn ref t where
  convertArg :: Proxy chn -> ArgumentValue' p ref -> t
instance ArgumentConversion chn ('PrimitiveRef s) s where
  convertArg _ (ArgPrimitive x) = x
instance FromSchema sch sty t
         => ArgumentConversion chn ('SchemaRef sch sty) t where
  convertArg _ (ArgSchema x) = fromSchema x
instance ArgumentConversion chn ref t
         => ArgumentConversion chn ('ListRef ref) [t] where
  convertArg p (ArgList x) = convertArg p <$> x
instance ArgumentConversion chn ref t
         => ArgumentConversion chn ('OptionalRef ref) (Maybe t) where
  convertArg p (ArgOptional x) = convertArg p <$> x

class ToRef chn r l => ResultConversion m p whole chn r l where
  convertResult :: (forall a. m a -> ServerErrorIO a)
                -> RequestHeaders
                -> ServerT chn GQL.Field p m whole
                -> [T.Text]
                -> ReturnQuery' p r
                -> l -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance Aeson.ToJSON t => ResultConversion m p whole chn ('PrimitiveRef t) t where
  convertResult _ _ _ _ RetPrimitive = pure . Just . Aeson.toJSON
instance ( ToSchema sch l r
         , RunSchemaQuery sch (sch :/: l) )
         => ResultConversion m p whole chn ('SchemaRef sch l) r where
  convertResult _ _ _ _ (RetSchema r) t
    = pure $ Just $ runSchemaQuery (toSchema' @_ @_ @sch @r t) r
instance ( MappingRight chn ref ~ t
         , MappingRight chn sname ~ t
         , LookupService ss ref ~ 'Service sname ms
         , RunQueryFindHandler m ('Package pname ss) whole chn ss ('Service sname ms) whole)
         => ResultConversion m ('Package pname ss) whole chn ('ObjectRef ref) t where
  convertResult f req whole path (RetObject q) h
    = Just <$> runQuery @m @('Package pname ss) @(LookupService ss ref) f req
                        (error "cannot inspect schema inside a field")
                        whole path h q
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('OptionalRef r) (Maybe s) where
  convertResult _ _ _ _ _ Nothing
    = pure Nothing
  convertResult f req whole path (RetOptional q) (Just x)
    = convertResult f req whole path q x
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('ListRef r) [s] where
  convertResult f req whole path (RetList q) xs
    = Just . Aeson.toJSON . catMaybes <$> mapM (convertResult f req whole path q) xs

class RunSchemaQuery sch r where
  runSchemaQuery
    :: Term sch r
    -> SchemaQuery sch r
    -> Aeson.Value
instance ( Aeson.ToJSON (Term sch ('DEnum name choices)) )
         => RunSchemaQuery sch ('DEnum name choices) where
  runSchemaQuery t _ = Aeson.toJSON t
instance ( KnownName rname, RunSchemaField sch fields )
         => RunSchemaQuery sch ('DRecord rname fields) where
  runSchemaQuery (TRecord args) (QueryRecord rs)
    = Aeson.object $ mapMaybe runOneQuery rs
    where
      runOneQuery (OneFieldQuery nm choice)
        = let (val, fname) = runSchemaField args choice
              realName = fromMaybe fname nm
          in (realName,) <$> val
      runOneQuery (TypeNameFieldQuery nm)
        = let realName = fromMaybe "__typename" nm
          -- add the 'R' because it's on return position
          in pure (realName, Aeson.String $ T.pack $ nameVal (Proxy @rname) ++ "R")


class RunSchemaField sch args where
  runSchemaField
    :: NP (Field sch) args
    -> NS (ChosenFieldQuery sch) args
    -> (Maybe Aeson.Value, T.Text)

instance RunSchemaField sch '[] where
  runSchemaField = error "this should never be called"
instance (KnownName fname, RunSchemaType sch t, RunSchemaField sch fs)
         => RunSchemaField sch ('FieldDef fname t ': fs) where
  runSchemaField (Field x :* _) (Z (ChosenFieldQuery c))
    = (runSchemaType x c, T.pack $ nameVal (Proxy @fname))
  runSchemaField (_ :* xs) (S rest)
    = runSchemaField xs rest

class RunSchemaType sch t where
  runSchemaType
    :: FieldValue sch t
    -> ReturnSchemaQuery sch t
    -> Maybe Aeson.Value
instance ( Aeson.ToJSON t )
         => RunSchemaType sch ('TPrimitive t) where
  runSchemaType (FPrimitive x) _
    = Just $ Aeson.toJSON x
instance RunSchemaType sch r
         => RunSchemaType sch ('TList r) where
  runSchemaType (FList xs) (RetSchList r)
    = Just . Aeson.toJSON $ mapMaybe (`runSchemaType` r) xs
instance RunSchemaType sch r
         => RunSchemaType sch ('TOption r) where
  runSchemaType (FOption xs) (RetSchOptional r)
    = xs >>= flip runSchemaType r
instance RunSchemaQuery sch (sch :/: l)
         => RunSchemaType sch ('TSchematic l) where
  runSchemaType (FSchematic t) (RetSchSchema r)
    = Just $ runSchemaQuery t r


runIntroSchema
  :: [T.Text] -> Intro.Schema -> [GQL.Selection]
  -> WriterT [GraphQLError] IO Aeson.Value
runIntroSchema path s@(Intro.Schema qr mut sub ts) ss
  = do things <- catMaybes <$> traverse runOne ss
       pure $ Aeson.object things
  where
    runOne (GQL.FieldSelection (GQL.Field alias nm _ _ innerss _))
      = let realName :: T.Text = fromMaybe nm alias
            path' = path ++ [realName]
        in fmap (realName,) <$> case nm of
             "description"
               -> pure $ Just Aeson.Null
             "directives"
               -> pure $ Just $ Aeson.Array []
             "queryType"
               -> case qr >>= flip HM.lookup ts of
                    Nothing -> pure Nothing
                    Just ty -> runIntroType path' s ty innerss
             "mutationType"
               -> case mut >>= flip HM.lookup ts of
                    Nothing -> pure Nothing
                    Just ty -> runIntroType path' s ty innerss
             "subscriptionType"
               -> case sub >>= flip HM.lookup ts of
                    Nothing -> pure Nothing
                    Just ty -> runIntroType path' s ty innerss
             "types"
               -> do tys <- catMaybes <$> mapM (\t -> runIntroType path' s t innerss) (HM.elems ts)
                     pure $ Just $ Aeson.toJSON tys
             _ -> do tell [GraphQLError
                             (ServerError Invalid
                               $ "field '" <> T.unpack nm <> "' was not found on type '__Schema'")
                             path]
                     pure Nothing
    -- we do not support spreads here
    runOne _ = pure Nothing

runIntroType
  :: [T.Text] -> Intro.Schema -> Intro.Type -> [GQL.Selection]
  -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
runIntroType path s@(Intro.Schema _ _ _ ts) (Intro.TypeRef t) ss
  = case HM.lookup t ts of
      Nothing -> pure Nothing
      Just ty -> runIntroType path s ty ss
runIntroType path s (Intro.Type k tnm fs vals ofT) ss
  = do things <- catMaybes <$> traverse runOne ss
       pure $ Just $ Aeson.object things
  where
    runOne (GQL.FieldSelection (GQL.Field alias nm _ _ innerss _))
      = let realName :: T.Text = fromMaybe nm alias
            path' = path ++ [realName]
        in fmap (realName,) <$> case (nm, innerss) of
             ("kind", [])
               -> pure $ Just $ Aeson.String $ T.pack (show k)
             ("name", [])
               -> pure $ Just $ maybe Aeson.Null Aeson.String tnm
             ("description", [])
               -> pure $ Just Aeson.Null

             ("fields", _)
               -> case k of
                    Intro.OBJECT
                      -> do things <- mapM (\f -> runIntroFields path' f innerss) fs
                            pure $ Just $ Aeson.toJSON things
                    _ -> pure $ Just Aeson.Null
             ("inputFields", _)
               -> case k of
                    Intro.INPUT_OBJECT
                      -> do things <- mapM (\f -> runIntroFields path' f innerss) fs
                            pure $ Just $ Aeson.toJSON things
                    _ -> pure $ Just Aeson.Null
             ("enumValues", _)
               -> do things <- mapM (\e -> runIntroEnums path' e innerss) vals
                     pure $ Just $ Aeson.toJSON things

             ("ofType", _)
               -> case ofT of
                    Nothing -> pure $ Just Aeson.Null
                    Just o  -> runIntroType path' s o innerss

             -- unions and interfaces are not supported
             ("interfaces", _)
               -> pure $ Just $ Aeson.Array []
             ("possibleTypes", _)
               -> pure $ Just $ Aeson.Array []

             _ -> do tell [GraphQLError
                             (ServerError Invalid
                               $ "field '" <> T.unpack nm <> "' was not found on type '__Type'")
                             path]
                     pure Nothing
    -- we do not support spreads here
    runOne _ = pure Nothing

    runIntroFields
      :: [T.Text] -> Intro.Field -> [GQL.Selection]
      -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
    runIntroFields fpath fld fss
      = do things <- catMaybes <$> traverse (runIntroField fpath fld) fss
           pure $ Just $ Aeson.object things

    runIntroField fpath (Intro.Field fnm fargs fty)
                  (GQL.FieldSelection (GQL.Field alias nm _ _ innerss _))
      = let realName :: T.Text = fromMaybe nm alias
            fpath' = fpath ++ [realName]
        in fmap (realName,) <$> case (nm, innerss) of
          ("name", [])
            -> pure $ Just $ Aeson.String fnm
          ("description", [])
            -> pure $ Just Aeson.Null
          ("isDeprecated", [])
            -> pure $ Just $ Aeson.Bool False
          ("deprecationReason", [])
            -> pure $ Just Aeson.Null

          -- this is used by __InputValue,
          -- which is required when the field
          -- is inside an INPUT_OBJECT
          ("defaultValue", [])
            -> pure $ Just Aeson.Null

          ("type", _)
            -> runIntroType fpath' s fty innerss
          ("args", _)
               -> do things <- mapM (\i -> runIntroInputs fpath' i innerss) fargs
                     pure $ Just $ Aeson.toJSON things

          _ -> do tell [GraphQLError
                             (ServerError Invalid
                               $ "field '" <> T.unpack nm <> "' was not found on type '__Field'")
                             fpath]
                  pure Nothing
    -- we do not support spreads here
    runIntroField _ _ _ = pure Nothing

    runIntroEnums
      :: [T.Text] -> Intro.EnumValue -> [GQL.Selection]
      -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
    runIntroEnums epath enm ess
      = do things <- catMaybes <$> traverse (runIntroEnum epath enm) ess
           pure $ Just $ Aeson.object things

    runIntroEnum epath (Intro.EnumValue enm)
                 (GQL.FieldSelection (GQL.Field alias nm _ _ innerss _))
      = let realName :: T.Text = fromMaybe nm alias
        in fmap (realName,) <$> case (nm, innerss) of
          ("name", [])
            -> pure $ Just $ Aeson.String enm
          ("description", [])
            -> pure $ Just Aeson.Null
          ("isDeprecated", [])
            -> pure $ Just $ Aeson.Bool False
          ("deprecationReason", [])
            -> pure $ Just Aeson.Null

          _ -> do tell [GraphQLError
                             (ServerError Invalid
                               $ "field '" <> T.unpack nm <> "' was not found on type '__EnumValue'")
                             epath]
                  pure Nothing
    -- we do not support spreads here
    runIntroEnum _ _ _ = pure Nothing

    runIntroInputs
      :: [T.Text] -> Intro.Input -> [GQL.Selection]
      -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
    runIntroInputs ipath inm iss
      = do things <- catMaybes <$> traverse (runIntroInput ipath inm) iss
           pure $ Just $ Aeson.object things

    runIntroInput ipath (Intro.Input inm def ty)
                 (GQL.FieldSelection (GQL.Field alias nm _ _ innerss _))
      = let realName :: T.Text = fromMaybe nm alias
            ipath' = ipath ++ [realName]
        in fmap (realName,) <$> case (nm, innerss) of
          ("name", [])
            -> pure $ Just $ Aeson.String inm
          ("description", [])
            -> pure $ Just Aeson.Null
          ("defaultValue", [])
            -> pure $ Just $ maybe Aeson.Null Aeson.String def

          ("type", _)
            -> runIntroType ipath' s ty innerss

          _ -> do tell [GraphQLError
                             (ServerError Invalid
                               $ "field '" <> T.unpack nm <> "' was not found on type '__Field'")
                             ipath]
                  pure Nothing
    -- we do not support spreads here
    runIntroInput _ _ _ = pure Nothing
