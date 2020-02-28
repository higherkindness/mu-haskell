{-# language DataKinds            #-}
{-# language FlexibleInstances    #-}
{-# language GADTs                #-}
{-# language PolyKinds            #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeApplications     #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
module Mu.GraphQL.Query.Parse where

import           Data.Proxy
import           Data.SOP.NS
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Mu.Rpc

import           Mu.GraphQL.Query.Definition
import           Mu.Schema

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

class ParseMethod (ms :: [Method Symbol Symbol]) where
  selectMethod :: T.Text -> NS Proxy ms

instance ParseMethod '[] where
  selectMethod = error "this should not be run"
instance (KnownSymbol mname, ParseMethod ms)
         => ParseMethod ('Method mname manns args ret ': ms) where
  selectMethod wanted
    | wanted == mname = Z Proxy
    | otherwise       = S (selectMethod wanted)
    where mname = T.pack $ nameVal (Proxy @mname)
