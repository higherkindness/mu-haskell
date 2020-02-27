{-# language DataKinds           #-}
{-# language GADTs               #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators       #-}
module Mu.GraphQL.Query.Parse where

import           Data.Proxy
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Mu.Rpc

import           Mu.GraphQL.Query.Definition

-- TODO: turn Hasura's ExecutableDefinition into a service query
-- hint#1: start with the following function, and then move up
--         (OperationDefinition > ExecutableDefinition > ExecutableDocument)
-- hint#2: introduce a type class which matches on "methods"
parseQuery :: forall (p :: Package') (s :: Symbol) pname ss sname sanns methods.
              ( p ~ 'Package pname ss
              , LookupService ss s ~ 'Service sname sanns methods )
           => Proxy p
           -> Proxy s
           -> GQL.SelectionSet
           -> Maybe (ServiceQuery p (LookupService ss s))
parseQuery = undefined
