{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module Schema where

import           Data.Int      (Int32)
import           Data.Text     as T
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

#if __GHCIDE__
grpc "SeedSchema" id "examples/seed/seed.proto"
#else
grpc "SeedSchema" id "seed.proto"
#endif

data Person = Person
  { name :: Maybe T.Text
  , age  :: Maybe Int32
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "Person"
             , FromSchema Maybe SeedSchema "Person" )

newtype PeopleRequest = PeopleRequest
  { name :: Maybe T.Text
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "PeopleRequest"
             , FromSchema Maybe SeedSchema "PeopleRequest" )

newtype PeopleResponse = PeopleResponse
  { person :: Maybe Person
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   Maybe SeedSchema "PeopleResponse"
             , FromSchema Maybe SeedSchema "PeopleResponse" )
