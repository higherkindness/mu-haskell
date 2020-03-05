{-# language DataKinds           #-}
{-# language GADTs               #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators       #-}
module Mu.GraphQL.Query.Definition where

import           Data.Functor.Identity
import           Data.SOP.NP
import           Data.SOP.NS
import           Data.Text
import           Mu.Rpc
import           Mu.Schema

data Document (p :: Package snm mnm) (qr :: snm) (mut :: snm) where
  QueryDoc    :: LookupService ss qr ~ 'Service qr qanns qms
              => ServiceQuery ('Package pname ss) (LookupService ss qr)
              -> Document ('Package pname ss) qr mut
  MutationDoc :: LookupService ss mut ~ 'Service mut manns mms
              => ServiceQuery ('Package pname ss) (LookupService ss mut)
              -> Document ('Package pname ss) qr mut

type ServiceQuery (p :: Package snm mnm) (s :: Service snm mnm)
  = [OneMethodQuery p s]

data OneMethodQuery (p :: Package snm mnm) (s :: Service snm mnm) where
  OneMethodQuery
    :: Maybe Text
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
  RetObject    :: ServiceQuery ('Package pname ss) (LookupService ss s)
               -> ReturnQuery ('Package pname ss) ('ObjectRef s)
