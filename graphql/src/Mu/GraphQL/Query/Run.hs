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
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fprint-explicit-foralls #-}
module Mu.GraphQL.Query.Run (
  GraphQLApp
, runPipeline
, runDocument
, runQuery
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
import           Data.Conduit.Internal          (ConduitT (..), Pipe (..))
import           Data.Conduit.TQueue
import           Data.Maybe
import qualified Data.Text                      as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax  as GQL

import           Mu.GraphQL.Query.Definition
import           Mu.GraphQL.Query.Parse
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

data GraphQLError
  = GraphQLError ServerError [T.Text]

type GraphQLApp p qr mut m chn hs
  = (ParseTypedDoc p qr mut, RunDocument p qr mut m chn hs)

runPipeline
  :: forall qr mut p m chn hs. GraphQLApp p qr mut m chn hs
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> Proxy qr -> Proxy mut
  -> Maybe T.Text -> VariableMapC -> GQL.ExecutableDocument
  -> IO Aeson.Value
runPipeline f svr _ _ opName vmap doc
  = case parseDoc @qr @mut opName vmap doc of
      Left e ->
        pure $
          Aeson.object [
            ("errors", Aeson.Array [
              Aeson.object [ ("message", Aeson.String e) ] ])]
      Right (d :: Document p qr mut) -> do
        (data_, errors) <- runWriterT (runDocument f svr d)
        case errors of
          [] -> pure $ Aeson.object [ ("data", data_) ]
          _  -> pure $ Aeson.object [ ("data", data_), ("errors", Aeson.listValue errValue errors) ]

errValue :: GraphQLError -> Aeson.Value
errValue (GraphQLError (ServerError _ msg) path)
  = Aeson.object [
      ("message", Aeson.String $ T.pack msg)
    , ("path", Aeson.toJSON path)
    ]

class RunDocument (p :: Package') (qr :: Maybe Symbol) (mut :: Maybe Symbol) m chn hs where
  runDocument ::
       (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Document p qr mut
    -> WriterT [GraphQLError] IO Aeson.Value

instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  , KnownSymbol mut
  , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
  , MappingRight chn mut ~ ()
  ) => RunDocument p ('Just qr) ('Just mut) m chn hs where
  runDocument f svr (QueryDoc q)
    = runQuery f svr [] () q
  runDocument f svr (MutationDoc q)
    = runQuery f svr [] () q
instance
  ( p ~ 'Package pname ss
  , KnownSymbol qr
  , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
  , MappingRight chn qr ~ ()
  ) => RunDocument p ('Just qr) 'Nothing m chn hs where
  runDocument f svr (QueryDoc q)
    = runQuery f svr [] () q
instance
  ( p ~ 'Package pname ss
  , KnownSymbol mut
  , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
  , MappingRight chn mut ~ ()
  ) => RunDocument p 'Nothing ('Just mut) m chn hs where
  runDocument f svr (MutationDoc q)
    = runQuery f svr [] () q
instance
  RunDocument p 'Nothing 'Nothing m chn hs where
  runDocument _ = error "this should never be called"

runQuery
  :: forall m p s pname ss hs sname sanns ms chn inh.
     ( RunQueryFindHandler m p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname sanns ms
     , inh ~ MappingRight chn sname )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> [T.Text]
  -> inh
  -> ServiceQuery p s
  -> WriterT [GraphQLError] IO Aeson.Value
runQuery f whole@(Services ss) path = runQueryFindHandler f whole path ss

runSubscription
  :: forall m p s pname ss hs sname sanns ms chn inh.
     ( RunQueryFindHandler m p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname sanns ms
     , inh ~ MappingRight chn sname )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> [T.Text]
  -> inh
  -> ServiceQuery p s
  -> ConduitT Aeson.Value Void IO ()
  -> IO ()
runSubscription f whole@(Services ss) path
  = runSubscriptionFindHandler f whole path ss

class RunQueryFindHandler m p whole chn ss s hs where
  runQueryFindHandler
    :: ( p ~  'Package pname wholess
       , s ~ 'Service sname sanns ms
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> [T.Text]
    -> ServicesT chn ss m hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value
  runSubscriptionFindHandler
    :: ( p ~  'Package pname wholess
       , s ~ 'Service sname sanns ms
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> [T.Text]
    -> ServicesT chn ss m hs
    -> inh
    -> ServiceQuery p s
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler m p whole chn '[] s '[] where
  runQueryFindHandler _ = error "this should never be called"
  runSubscriptionFindHandler _ = error "this should never be called"
instance {-# OVERLAPPABLE #-}
         RunQueryFindHandler m p whole chn ss s hs
         => RunQueryFindHandler m p whole chn (other ': ss) s (h ': hs) where
  runQueryFindHandler f whole path (_ :<&>: that)
    = runQueryFindHandler f whole path that
  runSubscriptionFindHandler f whole path (_ :<&>: that)
    = runSubscriptionFindHandler f whole path that
instance {-# OVERLAPS #-} (s ~ 'Service sname sanns ms, RunMethod m p whole chn sname ms h)
         => RunQueryFindHandler m p whole chn (s ': ss) s (h ': hs) where
  runQueryFindHandler f whole path (this :<&>: _) inh queries
    = Aeson.object . catMaybes <$> mapM runOneQuery queries
    where
      -- if we include the signature we have to write
      -- an explicit type signature for 'runQueryFindHandler'
      runOneQuery (OneMethodQuery nm args)
        = runMethod f whole (Proxy @sname) path nm inh this args
  -- subscriptions should only have one element
  runSubscriptionFindHandler f whole path (this :<&>: _) inh [OneMethodQuery nm args] sink
    = runMethodSubscription f whole (Proxy @sname) path nm inh this args sink
  runSubscriptionFindHandler _ _ path _ _ _ sink
    = let e = ServerError Invalid "you need exactly one field to subscribe"
          val = Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ]
      in runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink

class RunMethod m p whole chn sname ms hs where
  runMethod
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> Proxy sname -> [T.Text] -> Maybe T.Text -> inh
    -> HandlersT chn inh ms m hs
    -> NS (ChosenMethodQuery p) ms
    -> WriterT [GraphQLError] IO (Maybe (T.Text, Aeson.Value))
  runMethodSubscription
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> Proxy sname -> [T.Text] -> Maybe T.Text -> inh
    -> HandlersT chn inh ms m hs
    -> NS (ChosenMethodQuery p) ms
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance RunMethod m p whole chn s '[] '[] where
  runMethod _ = error "this should never be called"
  runMethodSubscription _ = error "this should never be called"
instance (RunMethod m p whole chn s ms hs, KnownName mname, RunHandler m p whole chn args r h)
         => RunMethod m p whole chn s ('Method mname anns args r ': ms) (h ': hs) where
  -- handle normal methods
  runMethod f whole _ path nm inh (h :<||>: _) (Z (ChosenMethodQuery args ret))
    = ((realName ,) <$>) <$> runHandler f whole (path ++ [realName]) (h inh) args ret
    where realName = fromMaybe (T.pack $ nameVal (Proxy @mname)) nm
  runMethod f whole p path nm inh (_ :<||>: r) (S cont)
    = runMethod f whole p path nm inh r cont
  -- handle subscriptions
  runMethodSubscription f whole _ path nm inh (h :<||>: _) (Z (ChosenMethodQuery args ret)) sink
    = runHandlerSubscription f whole (path ++ [realName]) (h inh) args ret sink
    where realName = fromMaybe (T.pack $ nameVal (Proxy @mname)) nm
  runMethodSubscription f whole p path nm inh (_ :<||>: r) (S cont) sink
    = runMethodSubscription f whole p path nm inh r cont sink

class Handles chn args r m h
      => RunHandler m p whole chn args r h where
  runHandler
    :: (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> [T.Text]
    -> h
    -> NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
  runHandlerSubscription
    :: (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> [T.Text]
    -> h
    -> NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> ConduitT Aeson.Value Void IO ()
    -> IO ()

instance (ArgumentConversion chn ref t, RunHandler m p whole chn rest r h)
         => RunHandler m p whole chn ('ArgSingle aname aanns ref ': rest) r (t -> h) where
  runHandler f whole path h (ArgumentValue one :* rest)
    = runHandler f whole path (h (convertArg (Proxy @chn) one)) rest
  runHandlerSubscription f whole path h (ArgumentValue one :* rest)
    = runHandlerSubscription f whole path (h (convertArg (Proxy @chn) one)) rest
instance ( MonadError ServerError m
         , FromRef chn ref t
         , ArgumentConversion chn ('ListRef ref) [t]
         , RunHandler m p whole chn rest r h )
         => RunHandler m p whole chn ('ArgStream aname aanns ref ': rest) r (ConduitT () t m () -> h) where
  runHandler f whole path h (ArgumentStream lst :* rest)
    = let converted :: [t] = convertArg (Proxy @chn) lst
      in runHandler f whole path (h (yieldMany converted)) rest
  runHandlerSubscription f whole path h (ArgumentStream lst :* rest) sink
    = let converted :: [t] = convertArg (Proxy @chn) lst
      in runHandlerSubscription f whole path (h (yieldMany converted)) rest sink
instance (MonadError ServerError m)
         => RunHandler m p whole chn '[] 'RetNothing (m ()) where
  runHandler f _ path h Nil _ = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right _ -> pure $ Just Aeson.Null
      Left e  -> tell [GraphQLError e path] >> pure Nothing
  runHandlerSubscription f _ path h Nil _ sink = do
    res <- liftIO $ runExceptT (f h)
    let vals = case res of
                 Right _ -> [] :: [Aeson.Value]
                 Left e -> [ Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ] ]
    runConduit $ yieldMany vals .| sink
instance (MonadError ServerError m, ResultConversion m p whole chn r l)
         => RunHandler m p whole chn '[] ('RetSingle r) (m l) where
  runHandler f whole path h Nil (RSingle q) = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right v -> convertResult f whole path q v
      Left e  -> tell [GraphQLError e path] >> pure Nothing
  runHandlerSubscription f whole path h Nil (RSingle q) sink = do
    res <- liftIO $ runExceptT (f h)
    val <- case res of
      Right v -> do
        (data_, errors) <- runWriterT (convertResult f whole path q v)
        case errors of
          [] -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_) ]
          _  -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_)
                                    , ("errors", Aeson.listValue errValue errors) ]
      Left e -> pure $ Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ]
    runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink
instance (MonadIO m, MonadError ServerError m, ResultConversion m p whole chn r l)
         => RunHandler m p whole chn '[] ('RetStream r) (ConduitT l Void m () -> m ()) where
  runHandler f whole path h Nil (RStream q) = do
    queue <- liftIO newTMQueueIO
    res <- liftIO $ runExceptT $ f $ h (sinkTMQueue queue)
    case res of
      Right _ -> do
        info <- runConduit $ sourceTMQueue queue .| sinkList
        Just . Aeson.toJSON . catMaybes <$> traverse (convertResult f whole path q) info
      Left e  -> tell [GraphQLError e []] >> pure Nothing
  runHandlerSubscription f whole path h Nil (RStream q) sink = do
    res <- liftIO $ runExceptT $ f $ h
      (transPipe liftIO (mapInputM convert (error "this should not be called") sink))
    case res of
      Right _ -> return ()
      Left e  -> do
        let val = Aeson.object [ ("errors", Aeson.listValue errValue [GraphQLError e path]) ]
        runConduit $ yieldMany ([val] :: [Aeson.Value]) .| sink
    where
      convert :: l -> IO Aeson.Value
      convert v = do
        (data_, errors) <- runWriterT (convertResult f whole path q v)
        case errors of
          [] -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_) ]
          _  -> pure $ Aeson.object [ ("data", fromMaybe Aeson.Null data_)
                                    , ("errors", Aeson.listValue errValue errors) ]

mapInputM :: Monad m
          => (i1 -> m i2) -- ^ map initial input to new input
          -> (i2 -> m (Maybe i1)) -- ^ map new leftovers to initial leftovers
          -> ConduitT i2 o m r
          -> ConduitT i1 o m r
mapInputM f f' (ConduitT c0) = ConduitT $ \rest -> let
    go (HaveOutput p o) = HaveOutput (go p) o
    go (NeedInput p c)  = NeedInput (\i -> PipeM $ go . p <$> f i) (go . c)
    go (Done r)         = rest r
    go (PipeM mp)       = PipeM $ fmap go mp
    go (Leftover p i)   = PipeM $ (\x -> maybe id (flip Leftover) x (go p)) <$> f' i
    in go (c0 Done)

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
                -> ServerT chn p m whole
                -> [T.Text]
                -> ReturnQuery' p r
                -> l -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance Aeson.ToJSON t => ResultConversion m p whole chn ('PrimitiveRef t) t where
  convertResult _ _ _ RetPrimitive = pure . Just . Aeson.toJSON
instance ( ToSchema sch l r
         , RunSchemaQuery sch (sch :/: l) )
         => ResultConversion m p whole chn ('SchemaRef sch l) r where
  convertResult _ _ _ (RetSchema r) t
    = pure $ Just $ runSchemaQuery (toSchema' @_ @_ @sch @r t) r
instance ( MappingRight chn ref ~ t
         , MappingRight chn sname ~ t
         , LookupService ss ref ~ 'Service sname sanns ms
         , RunQueryFindHandler m ('Package pname ss) whole chn ss ('Service sname sanns ms) whole)
         => ResultConversion m ('Package pname ss) whole chn ('ObjectRef ref) t where
  convertResult f whole path (RetObject q) h
    = Just <$> runQuery @m @('Package pname ss) @(LookupService ss ref) f whole path h q
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('OptionalRef r) (Maybe s) where
  convertResult _ _ _ _ Nothing
    = pure Nothing
  convertResult f whole path (RetOptional q) (Just x)
    = convertResult f whole path q x
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('ListRef r) [s] where
  convertResult f whole path (RetList q) xs
    = Just . Aeson.toJSON . catMaybes <$> mapM (convertResult f whole path q) xs

class RunSchemaQuery sch r where
  runSchemaQuery
    :: Term sch r
    -> SchemaQuery sch r
    -> Aeson.Value
instance ( Aeson.ToJSON (Term sch ('DEnum name choices)) )
         => RunSchemaQuery sch ('DEnum name choices) where
  runSchemaQuery t _ = Aeson.toJSON t
instance ( RunSchemaField sch fields )
         => RunSchemaQuery sch ('DRecord rname fields) where
  runSchemaQuery (TRecord args) (QueryRecord rs)
    = Aeson.object $ mapMaybe runOneQuery rs
    where
      runOneQuery (OneFieldQuery nm choice)
        = let (val, fname) = runSchemaField args choice
              realName = fromMaybe fname nm
          in (realName,) <$> val

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
