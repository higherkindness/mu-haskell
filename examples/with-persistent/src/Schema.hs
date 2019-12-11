{-# language DataKinds                  #-}
{-# language DeriveAnyClass             #-}
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
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language UndecidableInstances       #-}

module Schema where

import           Data.Int                (Int32)
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

grpc "PersistentSchema" id "with-persistent.proto"

newtype PersonRequest = PersonRequest
  { name :: T.Text
  } deriving (Eq, Show, Ord, Generic, HasSchema PersistentSchema "PersonRequest")

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name         String
  age          Int
  UniquePerson name
  deriving Show
|]

deriving instance Generic Person
deriving instance HasSchema PersistentSchema "Person" Person
