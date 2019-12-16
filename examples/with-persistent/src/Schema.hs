{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DerivingVia                #-}
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
{-# language TypeApplications           #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}

module Schema where

import           Data.Functor.Identity
import           Data.Int                 (Int32, Int64)
import qualified Data.Text                as T
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           GHC.Generics
import           GHC.TypeLits

import           Mu.Quasi.GRpc
import           Mu.Schema
import           Mu.Schema.Class
import           Mu.Schema.Interpretation

grpc "PersistentSchema" id "examples/with-persistent/with-persistent.proto"

newtype MPersonRequest = MPersonRequest
  { identifier :: Maybe Int64
  } deriving (Eq, Show, Ord, Generic)

instance ToSchema   Maybe PersistentSchema "PersonRequest" MPersonRequest
instance FromSchema Maybe PersistentSchema "PersonRequest" MPersonRequest

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name T.Text
  age  Int32
  deriving Show Generic
|]

data MPerson = MPerson
  { pid  :: Maybe MPersonRequest
  , name :: Maybe T.Text
  , age  :: Maybe Int32 }
  deriving (Generic)

instance ToSchema   Maybe PersistentSchema "Person" MPerson
instance FromSchema Maybe PersistentSchema "Person" MPerson

newtype WithEntityPlainId (ty :: Symbol) (fmap :: Mappings Symbol Symbol) a
  = WithEntityPlainId { unWithEntityPlainId :: a }
newtype WithEntityNestedId (ty :: Symbol) (fmap :: Mappings Symbol Symbol) a
  = WithEntityNestedId { unWithEntityNestedId :: a }

instance ( Generic t, Applicative w
         , (sch :/: sty) ~ 'DRecord name (idArg ': args)
         , idArg ~ 'Mu.Schema.FieldDef idArgName ('TPrimitive Int64)
         , Rep t ~ D1 dInfo (C1 cInfo f)
         , GToSchemaRecord Identity sch fmap args f
         , ToBackendKey (PersistEntityBackend t) t
         , PersistEntityBackend t ~ SqlBackend )
         => ToSchema w sch sty (WithEntityPlainId sty fmap (Entity t)) where
  toSchema (WithEntityPlainId (Entity key x))
    = TRecord $ Field (pure $ FPrimitive (unSqlBackendKey $ toBackendKey key))
                :* transFieldsNoMaps up (toSchemaRecord (Proxy @fmap) (unM1 $ unM1 $ from x))
    where up :: Identity a -> w a
          up (Identity x) = pure x

instance ( Generic t, Applicative w
         , (sch :/: sty) ~ 'DRecord name (nestedIdArg ': args)
         , nestedIdArg ~ 'Mu.Schema.FieldDef fname ('TSchematic idTy)
         , (sch :/: idTy) ~ 'DRecord idName '[idArg]
         , idArg ~ 'Mu.Schema.FieldDef idArgName ('TPrimitive Int64)
         , Rep t ~ D1 dInfo (C1 cInfo f)
         , GToSchemaRecord Identity sch fmap args f
         , ToBackendKey (PersistEntityBackend t) t
         , PersistEntityBackend t ~ SqlBackend )
         => ToSchema w sch sty (WithEntityNestedId sty fmap (Entity t)) where
  toSchema (WithEntityNestedId (Entity key x))
    = TRecord $ Field (pure $ FSchematic $ TRecord (Field (pure $ FPrimitive key') :* Nil))
                :* transFieldsNoMaps up (toSchemaRecord (Proxy @fmap) (unM1 $ unM1 $ from x))
    where key' = unSqlBackendKey $ toBackendKey key
          up :: Identity a -> w a
          up (Identity x) = pure x

type PersonFieldMapping
  = '[ "personAge" ':-> "age", "personName" ':-> "name" ]
deriving via (WithEntityNestedId "Person" PersonFieldMapping (Entity Person))
         instance ToSchema Maybe PersistentSchema "Person" (Entity Person)
