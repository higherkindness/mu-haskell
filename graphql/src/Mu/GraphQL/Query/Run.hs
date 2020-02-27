{-# language DataKinds           #-}
{-# language GADTs               #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators       #-}
module Mu.GraphQL.Query.Run where

import           Control.Monad.Writer
import qualified Data.Aeson                  as Aeson
import           Data.Functor.Identity
import           Mu.Server

import           Mu.GraphQL.Query.Definition

-- TODO: run the query
runQuery :: ServerT Identity chn p ServerErrorIO hs
         -> ServiceQuery p s
         -> WriterT ServerError IO Aeson.Value
runQuery = undefined
