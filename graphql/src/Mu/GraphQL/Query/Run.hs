{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# OPTIONS_GHC -fprint-explicit-foralls -fprint-explicit-kinds #-}
module Mu.GraphQL.Query.Run where

import           Control.Monad.Writer
import qualified Data.Aeson                  as Aeson
import           Data.Functor.Identity
import           Data.Maybe
import qualified Data.Text                   as T
import           GHC.TypeLits
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

import           Mu.GraphQL.Query.Definition

data GraphQLError
  = GraphQLError ServerError [T.Text]

-- TODO: run the query
runQuery
  :: ( RunQueryFindHandler ss s
     , p ~ 'Package pname ss
     , s ~ 'Service sname sanns ms
     , inh ~ MappingRight chn sname )
  => ServerT Identity chn p ServerErrorIO hs
  -> inh
  -> ServiceQuery p s
  -> WriterT [GraphQLError] IO Aeson.Value
runQuery whole@(Services ss) = runQueryFindHandler whole ss

class RunQueryFindHandler ss s where
  runQueryFindHandler
    :: ( p ~  'Package pname wholess
       , s ~ 'Service sname sanns ms
       , inh ~ MappingRight chn sname )
    => ServerT Identity chn p ServerErrorIO whole
    -> ServicesT Identity chn ss ServerErrorIO hs
    -> inh
    -> ServiceQuery p s
    -> WriterT [GraphQLError] IO Aeson.Value

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler '[] s where
  runQueryFindHandler = error "this should never be called"
instance {-# OVERLAPPABLE #-}
         RunQueryFindHandler ss s
         => RunQueryFindHandler (other ': ss) s where
  runQueryFindHandler whole (_ :<&>: that) = runQueryFindHandler whole that
instance {-# OVERLAPS #-} (s ~ 'Service sname sanns ms, RunMethod sname ms)
         => RunQueryFindHandler (s ': ss) s where
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

class RunMethod sname ms where
  runMethod
    :: ( p ~ 'Package pname wholess
       , inh ~ MappingRight chn sname )
    => ServerT Identity chn p ServerErrorIO whole
    -> Proxy sname -> inh
    -> HandlersT Identity chn inh ms ServerErrorIO hs
    -> NS (ChosenMethodQuery p) ms
    -> WriterT [GraphQLError] IO (Maybe Aeson.Value, T.Text)

instance RunMethod s '[] where
  runMethod = error "this should never be called"
instance (RunMethod s ms, KnownName mname, RunHandler args r)
         => RunMethod s ('Method mname anns args ('RetSingle r) ': ms) where
  runMethod whole _ inh (h :<||>: _) (Z (ChosenMethodQuery args ret))
    = (, T.pack $ nameVal (Proxy @mname)) <$> runHandler whole (h inh) args ret
  runMethod whole p inh (_ :<||>: r) (S cont)
    = runMethod whole p inh r cont

class RunHandler args r where
  runHandler :: Handles Identity chn args ('RetSingle r) ServerErrorIO h
             => ServerT Identity chn p ServerErrorIO whole
             -> h
             -> NP (ArgumentValue p) args
             -> ReturnQuery p r
             -> WriterT [GraphQLError] IO (Maybe Aeson.Value)
