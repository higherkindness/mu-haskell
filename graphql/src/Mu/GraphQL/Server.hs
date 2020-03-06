{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}

module Mu.GraphQL.Server where

import           Data.Functor.Identity
import           Data.Proxy
import           Mu.GraphQL.Query.Parse   (parseDoc, parseQuery)
import           Mu.GraphQL.Query.Run     (runPipeline)
import           Mu.Server
import           Network.Wai              (Application, queryString)
import           Network.Wai.Handler.Warp (Port, Settings, run, runSettings)

graphQLApp :: forall chn p hs qr mut.
       ServerT Identity chn p ServerErrorIO hs
    -> Proxy qr
    -> Proxy mut
    -> Application
graphQLApp server _ _ req res = do
  let query = queryString req
  -- let execDoc = parseDoc query
  -- let result = runPipeline server @qr @mut query _1 execDoc
  pure undefined

