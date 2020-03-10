{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLists       #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fprint-explicit-foralls #-}
module Mu.GraphQL.Query.Run (
  runPipeline
, runDocument
, runQuery
-- * Typeclass to be able to run query handlers
, RunQueryFindHandler
) where

import           Control.Monad.Except          (runExceptT)
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

runPipeline
  :: forall qr mut (p :: Package') pname ss hs chn qanns qmethods manns mmethods.
     ( p ~ 'Package pname ss
     , LookupService ss qr ~ 'Service qr qanns qmethods
     , ParseMethod p qmethods
     , LookupService ss mut ~ 'Service mut manns mmethods
     , ParseMethod p mmethods
     , RunQueryFindHandler p hs chn ss (LookupService ss qr) hs
     , MappingRight chn qr ~ ()
     , RunQueryFindHandler p hs chn ss (LookupService ss mut) hs
     , MappingRight chn mut ~ ()
     )
  => ServerT chn p ServerErrorIO hs
  -> Proxy qr -> Proxy mut
  -> Maybe T.Text -> VariableMapC -> GQL.ExecutableDocument
  -> IO Aeson.Value
runPipeline svr _ _ opName vmap doc
  = case parseDoc opName vmap doc of
      Nothing ->
        pure $
          Aeson.object [
            ("errors", Aeson.Array [
              Aeson.object [ ("message", Aeson.String "cannot parse document") ] ])]
      Just (d :: Document p qr mut) -> do
        (data_, errors) <- runWriterT (runDocument svr d)
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
     , RunQueryFindHandler p hs chn ss (LookupService ss qr) hs
     , MappingRight chn qr ~ ()
     , RunQueryFindHandler p hs chn ss (LookupService ss mut) hs
     , MappingRight chn mut ~ ()
     )
  => ServerT chn p ServerErrorIO hs
  -> Document p qr mut
  -> WriterT [GraphQLError] IO Aeson.Value
runDocument svr (QueryDoc q)
  = runQuery svr () q
runDocument svr (MutationDoc q)
  = runQuery svr () q

runQuery
  :: forall p s pname ss hs sname sanns ms chn inh.
     ( RunQueryFindHandler p hs chn ss s hs
     , p ~ 'Package pname ss
     , s ~ 'Service sname sanns ms
     , inh ~ MappingRight chn sname )
  => ServerT chn p ServerErrorIO hs
  -> inh
  -> ServiceQuery p s
  -> WriterT [GraphQLError] IO Aeson.Value
runQuery whole@(Services ss) = runQueryFindHandler whole ss

class RunQueryFindHandler p whole chn ss s hs where
  runQueryFindHandler
    :: ( p ~  'Package pname wholess
       , s ~ 'Service sname sanns ms
       , inh ~ MappingRight chn sname )
    => ServerT chn p ServerErrorIO whole
    -> ServicesT chn ss ServerErrorIO hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler p whole chn '[] s '[] where
  runQueryFindHandler = error "this should never be called"
instance {-# OVERLAPPABLE #-}
         RunQueryFindHandler p whole chn ss s hs
         => RunQueryFindHandler p whole chn (other ': ss) s (h ': hs) where
  runQueryFindHandler whole (_ :<&>: that) = runQueryFindHandler whole that
instance {-# OVERLAPS #-} (s ~ 'Service sname sanns ms, RunMethod p whole chn sname ms h)
         => RunQueryFindHandler p whole chn (s ': ss) s (h ': hs) where
  runQueryFindHandler whole (this :<&>: _) inh queries
    = Aeson.object . catMaybes <$> mapM runOneQuery queries
    where
      -- if we include the signature we have to write
      -- an explicit type signature for 'runQueryFindHandler'
      runOneQuery (OneMethodQuery nm args)
        = pass (do (val, methodName) <- runMethod whole (Proxy @sname) inh this args
                   let realName = fromMaybe methodName nm
                       -- choose between given name,
                       -- or fallback to method name
                       newVal = fmap (realName,) val
                   pure (newVal, map (updateErrs realName)) )
        where -- add the additional path component to the errors
              updateErrs :: T.Text -> GraphQLError -> GraphQLError
              updateErrs methodName (GraphQLError err loc) = GraphQLError err (methodName : loc)

class RunMethod p whole chn sname ms hs where
  runMethod
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn sname )
    => ServerT chn p ServerErrorIO whole
    -> Proxy sname -> inh
    -> HandlersT chn inh ms ServerErrorIO hs
    -> NS (ChosenMethodQuery p) ms
    -> WriterT [GraphQLError] IO (Maybe Aeson.Value, T.Text)

instance RunMethod p whole chn s '[] '[] where
  runMethod = error "this should never be called"
instance (RunMethod p whole chn s ms hs, KnownName mname, RunHandler p whole chn args r h)
         => RunMethod p whole chn s ('Method mname anns args ('RetSingle r) ': ms) (h ': hs) where
  runMethod whole _ inh (h :<||>: _) (Z (ChosenMethodQuery args ret))
    = (, T.pack $ nameVal (Proxy @mname)) <$> runHandler whole (h inh) args ret
  runMethod whole p inh (_ :<||>: r) (S cont)
    = runMethod whole p inh r cont

class Handles chn args ('RetSingle r) ServerErrorIO h
      => RunHandler p whole chn args r h where
  runHandler :: ServerT chn p ServerErrorIO whole
             -> h
             -> NP (ArgumentValue p) args
             -> ReturnQuery p r
             -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance (ArgumentConversion chn ref t, RunHandler p whole chn rest r h)
         => RunHandler p whole chn ('ArgSingle aname aanns ref ': rest) r (t -> h) where
  runHandler whole h (ArgumentValue one :* rest)
    = runHandler whole (h (convertArg (Proxy @chn) one)) rest
instance (ResultConversion p whole chn r l)
         => RunHandler p whole chn '[] r (ServerErrorIO l) where
  runHandler whole h Nil q = do
    res <- liftIO $ runExceptT h
    case res of
      Right v -> convertResult whole q v
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

class ToRef chn r l => ResultConversion p whole chn r l where
  convertResult :: ServerT chn p ServerErrorIO whole
                -> ReturnQuery p r
                -> l -> WriterT [GraphQLError] IO (Maybe Aeson.Value)

instance Aeson.ToJSON t => ResultConversion p whole chn ('PrimitiveRef t) t where
  convertResult _ RetPrimitive = pure . Just . Aeson.toJSON
instance ( ToSchema sch l r
         , Aeson.ToJSON (Term sch (sch :/: l)) )
         => ResultConversion p whole chn ('SchemaRef sch l) r where
  convertResult _ RetSchema = pure . Just . Aeson.toJSON . toSchema' @_ @_ @sch @r
instance ( MappingRight chn ref ~ t
         , MappingRight chn sname ~ t
         , LookupService ss ref ~ 'Service sname sanns ms
         , RunQueryFindHandler ('Package pname ss) whole chn ss ('Service sname sanns ms) whole)
         => ResultConversion ('Package pname ss) whole chn ('ObjectRef ref) t where
  convertResult whole (RetObject q) h
    = Just <$> runQuery @('Package pname ss) @(LookupService ss ref) whole h q
instance ResultConversion p whole chn r s
        => ResultConversion p whole chn ('OptionalRef r) (Maybe s) where
  convertResult _ _ Nothing
    = pure Nothing
  convertResult whole (RetOptional q) (Just x)
    = convertResult whole q x
instance ResultConversion p whole chn r s
        => ResultConversion p whole chn ('ListRef r) [s] where
  convertResult whole (RetList q) xs
    = Just . Aeson.toJSON . catMaybes <$> mapM (convertResult whole q) xs
