{-# language DataKinds           #-}
{-# language GADTs               #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators       #-}
module Mu.GraphQL.Query where

import           Control.Monad.Writer
import qualified Data.Aeson                    as Aeson
import           Data.Functor.Identity
import           Data.SOP.NP
import           Data.SOP.NS
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GraphQL
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

-- TODO: turn Hasura's ExecutableDefinition into a service query
-- hint#1: start with the following function, and then move up
--         (OperationDefinition > ExecutableDefinition > ExecutableDocument)
-- hint#2: introduce a type class which matches on "methods"
parseQuery :: forall (p :: Package') (s :: Symbol) pname ss sname sanns methods.
              ( p ~ 'Package pname ss
              , LookupService ss s ~ 'Service sname sanns methods )
           => Proxy p -> Proxy s
           -> GraphQL.SelectionSet
           -> Maybe (ServiceQuery p (LookupService ss s))
parseQuery = undefined

-- TODO: run the query
runQuery :: ServerT Identity chn p ServerErrorIO hs
         -> ServiceQuery p s
         -> WriterT ServerError IO Aeson.Value
runQuery = undefined

type ServiceQuery (p :: Package snm mnm) (s :: Service snm mnm)
  = [OneMethodQuery p s]

data OneMethodQuery (p :: Package snm mnm) (s :: Service snm mnm) where
  OneMethodQuery
    :: Maybe String
    -> NS (ChosenMethodQuery p) ms
    -> OneMethodQuery p ('Service nm anns ms)

data ChosenMethodQuery (p :: Package snm mnm) (m :: Method snm mnm) where
  ChosenMethodQuery
    :: NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> ChosenMethodQuery p ('Method mname anns args ('RetSingle r))

data ArgumentValue (p :: Package snm mnm) (a :: Argument snm) where
  ArgumentValue :: ArgumentValue' p r -> ArgumentValue p ('ArgSingle r)

data ArgumentValue' (p :: Package snm mnm) (r :: TypeRef snm) where
  ArgPrimitive :: t -> ArgumentValue' p ('PrimitiveRef t)
  ArgSchema    :: Term Identity sch (sch :/: sty)
               -> ArgumentValue' p ('SchemaRef sch sty)
  ArgList      :: [ArgumentValue' p r]
               -> ArgumentValue' p ('ListRef r)
  ArgOptional  :: Maybe (ArgumentValue' p r)
               -> ArgumentValue' p ('OptionalRef r)

data ReturnQuery (p :: Package snm mnm) (r :: TypeRef snm) where
  RetPrimitive :: ReturnQuery p ('PrimitiveRef t)
  RetSchema    :: ReturnQuery p ('SchemaRef sch sty)
  RetList      :: ReturnQuery p r -> ReturnQuery p ('ListRef r)
  RetOptional  :: ReturnQuery p r -> ReturnQuery p ('OptionalRef r)
  RetObject    :: ServiceQuery p (LookupService ss s)
               -> ReturnQuery ('Package pname ss) ('ObjectRef s)
