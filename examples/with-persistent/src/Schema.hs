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

newtype PersonRequest = PersonRequest
  { identifier :: Int64
  } deriving (Eq, Show, Ord, Generic)

instance HasSchema PersistentSchema "PersonRequest" PersonRequest

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name T.Text
  age  Int32
  deriving Show
|]

deriving instance Generic Person

-- Unfortunately we need to write this instance by hand 😔 (for now!)
instance HasSchema PersistentSchema "Person" (Entity Person) where
  fromSchema (TRecord (Field (FSchematic (TRecord (Field (FPrimitive pid) :* Nil))) :* Field (FPrimitive name) :* Field (FPrimitive age) :* Nil)) = Entity (PersonKey (SqlBackendKey pid)) (Person name age)
  toSchema (Entity (PersonKey (SqlBackendKey pid)) (Person name age)) = TRecord $ Field (FSchematic (TRecord (Field (FPrimitive pid) :* Nil))) :* Field (FPrimitive name) :* Field (FPrimitive age) :* Nil
