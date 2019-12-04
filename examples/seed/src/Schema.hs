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

grpc "SeedSchema" id "seed.proto"

data Person = Person
  { name :: T.Text
  , age  :: Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema SeedSchema "Person")

newtype PeopleRequest = PeopleRequest
  { name :: T.Text
  } deriving (Eq, Show, Ord, Generic, HasSchema SeedSchema "PeopleRequest")

newtype PeopleResponse = PeopleResponse
  { person :: Person
  } deriving (Eq, Show, Ord, Generic, HasSchema SeedSchema "PeopleResponse")
