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

grpc "PersistentSchema" id "with-persistent.proto"

newtype PersonId = PersonId
  { id :: Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema PersistentSchema "PersonId")

data Person = Person
  { personId :: PersonId
  , name     :: T.Text
  , age      :: Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema PersistentSchema "Person")
