{-# language DataKinds             #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
module Mu.GraphQL.Query.Run where

import           Control.Monad.Writer
import qualified Data.Aeson                  as Aeson
import           Data.Functor.Identity
import           GHC.TypeLits
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

import           Mu.GraphQL.Query.Definition

-- TODO: run the query
runQuery
  :: RunQueryFindHandler ss s
  => ServerT Identity chn ('Package pname ss) ServerErrorIO hs
  -> ServiceQuery ('Package pname ss) s
  -> WriterT ServerError IO Aeson.Value
runQuery whole@(Services ss) = runQueryFindHandler whole ss

class RunQueryFindHandler ss s where
  runQueryFindHandler
    :: ServerT Identity chn ('Package pname wholess) ServerErrorIO whole
    -> ServicesT Identity chn ss ServerErrorIO hs
    -> ServiceQuery ('Package pname ss) s
    -> WriterT ServerError IO Aeson.Value

instance TypeError ('Text "Could not find handler for " ':<>: 'ShowType s)
         => RunQueryFindHandler '[] s where
  runQueryFindHandler = error "this should never be called"
instance {-# OVERLAPS #-}
         RunQueryFindHandler (s ': ss) s where
  runQueryFindHandler whole (this :<&>: _) = runQueryService whole this

runQueryService
  :: ServerT Identity chn ('Package pname ss) ServerErrorIO hss  -- the whole package
  -> HandlersT w chn (MappingRight chn sname) methods m hs  -- the one for the service
  -> ServiceQuery p ('Service sname sanns methods)
  -> WriterT ServerError IO Aeson.Value
runQueryService = undefined
