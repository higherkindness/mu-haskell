{-# language DataKinds           #-}
{-# language GADTs               #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators       #-}
module Mu.GraphQL.Query.Definition where

import           Data.SOP.NP
import           Data.SOP.NS
import           Data.Text
import           Mu.Rpc
import           Mu.Schema

data Document (p :: Package snm mnm anm)
              (qr :: Maybe snm) (mut :: Maybe snm) (sub :: Maybe snm) where
  QueryDoc
    :: LookupService ss qr ~ 'Service qr qanns qms
    => ServiceQuery ('Package pname ss) (LookupService ss qr)
    -> Document ('Package pname ss) ('Just qr) mut sub
  MutationDoc
    :: LookupService ss mut ~ 'Service mut manns mms
    => ServiceQuery ('Package pname ss) (LookupService ss mut)
    -> Document ('Package pname ss) qr ('Just mut) sub
  SubscriptionDoc
    :: LookupService ss sub ~ 'Service sub manns mms
    => OneMethodQuery ('Package pname ss) (LookupService ss sub)
    -> Document ('Package pname ss) qr mut ('Just sub)

type ServiceQuery (p :: Package snm mnm anm) (s :: Service snm mnm anm)
  = [OneMethodQuery p s]

data OneMethodQuery (p :: Package snm mnm anm) (s :: Service snm mnm anm) where
  OneMethodQuery
    :: Maybe Text
    -> NS (ChosenMethodQuery p) ms
    -> OneMethodQuery p ('Service nm anns ms)

data ChosenMethodQuery (p :: Package snm mnm anm) (m :: Method snm mnm anm) where
  ChosenMethodQuery
    :: NP (ArgumentValue p) args
    -> ReturnQuery p r
    -> ChosenMethodQuery p ('Method mname anns args r)

data ArgumentValue (p :: Package snm mnm anm) (a :: Argument snm anm) where
  ArgumentValue  :: ArgumentValue' p r
                 -> ArgumentValue p ('ArgSingle aname anns r)
  ArgumentStream :: ArgumentValue' p ('ListRef r)
                 -> ArgumentValue p ('ArgStream aname anns r)

data ArgumentValue' (p :: Package snm mnm anm) (r :: TypeRef snm) where
  ArgPrimitive :: t -> ArgumentValue' p ('PrimitiveRef t)
  ArgSchema    :: Term sch (sch :/: sty)
               -> ArgumentValue' p ('SchemaRef sch sty)
  ArgList      :: [ArgumentValue' p r]
               -> ArgumentValue' p ('ListRef r)
  ArgOptional  :: Maybe (ArgumentValue' p r)
               -> ArgumentValue' p ('OptionalRef r)

data ReturnQuery (p :: Package snm mnm anm) (r :: Return snm) where
  RNothing :: ReturnQuery p 'RetNothing
  RSingle  :: ReturnQuery' p r -> ReturnQuery p ('RetSingle r)
  RStream  :: ReturnQuery' p r -> ReturnQuery p ('RetStream r)

data ReturnQuery' (p :: Package snm mnm anm) (r :: TypeRef snm) where
  RetPrimitive :: ReturnQuery' p ('PrimitiveRef t)
  RetSchema    :: SchemaQuery sch (sch :/: sty)
               -> ReturnQuery' p ('SchemaRef sch sty)
  RetList      :: ReturnQuery' p r -> ReturnQuery' p ('ListRef r)
  RetOptional  :: ReturnQuery' p r -> ReturnQuery' p ('OptionalRef r)
  RetObject    :: ServiceQuery ('Package pname ss) (LookupService ss s)
               -> ReturnQuery' ('Package pname ss) ('ObjectRef s)

data SchemaQuery (sch :: Schema tn fn) (t :: TypeDef tn fn) where
  QueryEnum   :: SchemaQuery sch ('DEnum nm choices)
  QueryRecord :: [OneFieldQuery sch fs]
              -> SchemaQuery sch ('DRecord ty fs)

data OneFieldQuery (sch :: Schema tn fn) (fs :: [FieldDef tn fn]) where
  OneFieldQuery
    :: Maybe Text
    -> NS (ChosenFieldQuery sch) fs
    -> OneFieldQuery sch fs

data ChosenFieldQuery (sch :: Schema tn fn) (f :: FieldDef tn fn) where
  ChosenFieldQuery
    :: ReturnSchemaQuery sch r
    -> ChosenFieldQuery sch ('FieldDef name r)

data ReturnSchemaQuery (sch :: Schema tn fn) (r :: FieldType tn) where
  RetSchPrimitive :: ReturnSchemaQuery sch ('TPrimitive t)
  RetSchSchema    :: SchemaQuery sch (sch :/: sty)
                  -> ReturnSchemaQuery sch ('TSchematic sty)
  RetSchList      :: ReturnSchemaQuery sch r
                  -> ReturnSchemaQuery sch ('TList r)
  RetSchOptional  :: ReturnSchemaQuery sch r
                  -> ReturnSchemaQuery sch ('TOption r)
