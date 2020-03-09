{-# language CPP                        #-}
{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DerivingVia                #-}
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
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}

module Schema where

import           Data.Int                (Int32, Int64)
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

import           Mu.Adapter.Persistent   (WithEntityNestedId (..))
import           Mu.Quasi.GRpc
import           Mu.Schema

#if __GHCIDE__
grpc "PersistentSchema" id "examples/with-persistent/with-persistent.proto"
#else
grpc "PersistentSchema" id "with-persistent.proto"
#endif

newtype MPersonRequest = MPersonRequest
  { identifier :: Int64
  } deriving (Eq, Show, Ord, Generic)

instance ToSchema   PersistentSchema "PersonRequest" MPersonRequest
instance FromSchema PersistentSchema "PersonRequest" MPersonRequest

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name T.Text
  age  Int32
  deriving Show Generic
|]

data MPerson = MPerson
  { pid  :: Maybe MPersonRequest
  , name :: T.Text
  , age  :: Int32
  } deriving (Eq, Ord, Show, Generic)

instance ToSchema   PersistentSchema "Person" MPerson
instance FromSchema PersistentSchema "Person" MPerson

type PersonFieldMapping = '[ "personAge" ':-> "age", "personName" ':-> "name" ]

deriving via (WithEntityNestedId "Person" PersonFieldMapping (Entity Person))
  instance ToSchema PersistentSchema "Person" (Entity Person)
