{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DuplicateRecordFields      #-}
{-# language EmptyDataDecls             #-}
{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language OverloadedStrings          #-}
{-# language PolyKinds                  #-}
{-# language QuasiQuotes                #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language UndecidableInstances       #-}
module Schema where

import           Data.Int                (Int32, Int64)
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

grpc "PersistentSchema" id "with-persistent.proto"

newtype MPersonRequest = MPersonRequest
  { identifier :: Maybe Int64
  } deriving (Eq, Show, Ord, Generic)

instance ToSchema   Maybe PersistentSchema "PersonRequest" MPersonRequest
instance FromSchema Maybe PersistentSchema "PersonRequest" MPersonRequest

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name T.Text
  age  Int32
  deriving Show
|]

data MPerson = MPerson
  { pid  :: Maybe MPersonRequest
  , name :: Maybe T.Text
  , age  :: Maybe Int32 }
  deriving (Generic)

instance ToSchema   Maybe PersistentSchema "Person" MPerson
instance FromSchema Maybe PersistentSchema "Person" MPerson

-- Unfortunately we need to write this instance by hand 😔 (for now!)
instance ToSchema Maybe PersistentSchema "Person" (Entity Person) where
  toSchema (Entity (PersonKey (SqlBackendKey pid)) (Person name age))
    = TRecord $
        Field (Just $ FSchematic (TRecord (Field (Just $ FPrimitive pid) :* Nil)))
        :* Field (Just $ FPrimitive name)
        :* Field (Just $ FPrimitive age) :* Nil
