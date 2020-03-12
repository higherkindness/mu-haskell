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

import           Control.Monad.Except          (MonadError, runExceptT)
import           Control.Monad.Writer
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import           Data.Maybe
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL

import           Mu.GraphQL.Query.Definition
import           Mu.GraphQL.Query.Parse
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

data GraphQLError
  = GraphQLError ServerError [T.Text]

type GraphQLApp m p pname ss qmethods mmethods hs chn qr mut qanns manns =
  ( p ~ 'Package pname ss
    , KnownName qr
    , ParseMethod p qmethods
    , KnownName mut
    , ParseMethod p mmethods
    , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
    , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
    , MappingRight chn qr ~ ()
    , LookupService ss qr ~ 'Service qr qanns qmethods
    , LookupService ss mut ~ 'Service mut manns mmethods
    , MappingRight chn mut ~ ()
  )

runPipeline
  :: forall m qr mut (p :: Package') pname ss hs chn qanns qmethods manns mmethods.
     ( GraphQLApp m p pname ss qmethods mmethods hs chn qr mut qanns manns )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> Proxy qr -> Proxy mut
  -> Maybe T.Text -> VariableMapC -> GQL.ExecutableDocument
  -> IO Aeson.Value
runPipeline f svr _ _ opName vmap doc
  = case parseDoc opName vmap doc of
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
    where
      errValue :: GraphQLError -> Aeson.Value
      errValue (GraphQLError (ServerError _ msg) path)
        = Aeson.object [
            ("message", Aeson.String $ T.pack msg)
          , ("path", Aeson.toJSON path)
          ]

runDocument
  :: ( p ~ 'Package pname ss
     , RunQueryFindHandler m p hs chn ss (LookupService ss qr) hs
     , MappingRight chn qr ~ ()
     , RunQueryFindHandler m p hs chn ss (LookupService ss mut) hs
     , MappingRight chn mut ~ ()
     )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> Document p qr mut
  -> WriterT [GraphQLError] IO Aeson.Value
runDocument f svr (QueryDoc q)
  = runQuery f svr () q
runDocument f svr (MutationDoc q)
  = runQuery f svr () q

runQuery
  :: forall m p s pname ss hs sname sanns ms chn inh.
     ( RunQueryFindHandler m p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname sanns ms
     , inh ~ MappingRight chn sname )
  => (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> inh
  -> ServiceQuery p s
  -> WriterT [GraphQLError] IO Aeson.Value
runQuery f whole@(Services ss) = runQueryFindHandler f whole ss

class RunQueryFindHandler m p whole chn ss s hs where
  runQueryFindHandler
    :: ( p ~  'Package pname wholess
       , s ~ 'Service sname sanns ms
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> ServicesT chn ss m hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler m p whole chn '[] s '[] where
  runQueryFindHandler _ = error "this should never be called"
instance {-# OVERLAPPABLE #-}
         RunQueryFindHandler m p whole chn ss s hs
         => RunQueryFindHandler m p whole chn (other ': ss) s (h ': hs) where
  runQueryFindHandler f whole (_ :<&>: that) = runQueryFindHandler f whole that
instance {-# OVERLAPS #-} (s ~ 'Service sname sanns ms, RunMethod m p whole chn sname ms h)
         => RunQueryFindHandler m p whole chn (s ': ss) s (h ': hs) where
  runQueryFindHandler f whole (this :<&>: _) inh queries
    = Aeson.object . catMaybes <$> mapM runOneQuery queries
    where
      -- if we include the signature we have to write
      -- an explicit type signature for 'runQueryFindHandler'
      runOneQuery (OneMethodQuery nm args)
        = pass (do (val, methodName) <- runMethod f whole (Proxy @sname) inh this args
                   let realName = fromMaybe methodName nm
                       -- choose between given name,
                       -- or fallback to method name
                       newVal = fmap (realName,) val
                   pure (newVal, map (updateErrs realName)) )
        where -- add the additional path component to the errors
              updateErrs :: T.Text -> GraphQLError -> GraphQLError
              updateErrs methodName (GraphQLError err loc) = GraphQLError err (methodName : loc)

class RunMethod m p whole chn sname ms hs where
  runMethod
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn sname )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m whole
    -> Proxy sname -> inh
    -> HandlersT chn inh ms m hs
    -> NS (ChosenMethodQuery p) ms
    -> WriterT [GraphQLError] IO (Maybe Aeson.Value, T.Text)

instance RunMethod m p whole chn s '[] '[] where
  runMethod _ = error "this should never be called"
instance (RunMethod m p whole chn s ms hs, KnownName mname, RunHandler m p whole chn args r h)
         => RunMethod m p whole chn s ('Method mname anns args ('RetSingle r) ': ms) (h ': hs) where
  runMethod f whole _ inh (h :<||>: _) (Z (ChosenMethodQuery args ret))
    = (, T.pack $ nameVal (Proxy @mname)) <$> runHandler f whole (h inh) args ret
  runMethod f whole p inh (_ :<||>: r) (S cont)
    = runMethod f whole p inh r cont

class Handles chn args ('RetSingle r) m h
      => RunHandler m p whole chn args r h where
  runHandler :: (forall a. m a -> ServerErrorIO a)
             -> ServerT chn p m whole
             -> h
             -> NP (ArgumentValue p) args
             -> ReturnQuery p r
             -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance (ArgumentConversion chn ref t, RunHandler m p whole chn rest r h)
         => RunHandler m p whole chn ('ArgSingle aname aanns ref ': rest) r (t -> h) where
  runHandler f whole h (ArgumentValue one :* rest)
    = runHandler f whole (h (convertArg (Proxy @chn) one)) rest
instance (MonadError ServerError m, ResultConversion m p whole chn r l)
         => RunHandler m p whole chn '[] r (m l) where
  runHandler f whole h Nil q = do
    res <- liftIO $ runExceptT (f h)
    case res of
      Right v -> convertResult f whole q v
      Left e  -> tell [GraphQLError e []] >> pure Nothing

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
                -> ReturnQuery p r
                -> l -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance Aeson.ToJSON t => ResultConversion m p whole chn ('PrimitiveRef t) t where
  convertResult _ _ RetPrimitive = pure . Just . Aeson.toJSON
instance ( ToSchema sch l r
         , Aeson.ToJSON (Term sch (sch :/: l)) )
         => ResultConversion m p whole chn ('SchemaRef sch l) r where
  convertResult _ _ RetSchema = pure . Just . Aeson.toJSON . toSchema' @_ @_ @sch @r
instance ( MappingRight chn ref ~ t
         , MappingRight chn sname ~ t
         , LookupService ss ref ~ 'Service sname sanns ms
         , RunQueryFindHandler m ('Package pname ss) whole chn ss ('Service sname sanns ms) whole)
         => ResultConversion m ('Package pname ss) whole chn ('ObjectRef ref) t where
  convertResult f whole (RetObject q) h
    = Just <$> runQuery @m @('Package pname ss) @(LookupService ss ref) f whole h q
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('OptionalRef r) (Maybe s) where
  convertResult _ _ _ Nothing
    = pure Nothing
  convertResult f whole (RetOptional q) (Just x)
    = convertResult f whole q x
instance ResultConversion m p whole chn r s
        => ResultConversion m p whole chn ('ListRef r) [s] where
  convertResult f whole (RetList q) xs
    = Just . Aeson.toJSON . catMaybes <$> mapM (convertResult f whole q) xs
