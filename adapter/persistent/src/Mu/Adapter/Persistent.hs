{-# language DataKinds              #-}
{-# language FlexibleContexts       #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language KindSignatures         #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-|
Description : Utilities for interoperation between Mu and Persistent

The @persistent@ library, and in particular its quasi-quoters
for entities, generate data types which do not look exactly as
plain records. This module defines some wrappers which modify
the 'ToSchema' and 'FromSchema' derivation to work with them.
-}
module Mu.Adapter.Persistent (
  -- * Wrappers for use with @DerivingVia@
  WithEntityNestedId(..)
, WithEntityPlainId(..)
  -- * Generic utilities
, runDb
) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource.Internal
import           Data.Int
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.TypeLits

import           Mu.Schema
import           Mu.Schema.Class

-- | Wrapper for 'Entity' to be used with @DerivingVia@.
--   This wrappers indicates that the identifier is to be found
--   as the sole field of another object, like in:
--
--   > { id: { key: 3 }, name: "Somebody" }
newtype WithEntityNestedId (ty :: Symbol) (fmap :: Mappings Symbol Symbol) a
  = WithEntityNestedId { unWithEntityNestedId :: a }

-- | Wrapper for 'Entity' to be used with @DerivingVia@.
--   This wrappers indicates that the identifier is to be found
--   in the schema at the same level as other fields, like in:
--
--   > { id: 3, name: "Somebody" }
newtype WithEntityPlainId (ty :: Symbol) (fmap :: Mappings Symbol Symbol) a
  = WithEntityPlainId { unWithEntityPlainId :: a }

instance ( Generic t
         , (sch :/: sty) ~ 'DRecord name (idArg ': args)
         , idArg ~ 'Mu.Schema.FieldDef idArgName ('TPrimitive Int64)
         , Rep t ~ D1 dInfo (C1 cInfo f)
         , GToSchemaRecord sch fmap args f
         , ToBackendKey (PersistEntityBackend t) t
         , PersistEntityBackend t ~ SqlBackend )
         => ToSchema sch sty (WithEntityPlainId sty fmap (Entity t)) where
  toSchema (WithEntityPlainId (Entity key x))
    = TRecord $ Field (FPrimitive (unSqlBackendKey $ toBackendKey key))
      :* toSchemaRecord (Proxy @fmap) (unM1 $ unM1 $ from x)

instance ( Generic t
         , (sch :/: sty) ~ 'DRecord name (nestedIdArg ': args)
         , nestedIdArg ~ 'Mu.Schema.FieldDef fname k
         , ToSchemaKey sch idTy k
         , (sch :/: idTy) ~ 'DRecord idName '[idArg]
         , idArg ~ 'Mu.Schema.FieldDef idArgName ('TPrimitive Int64)
         , Rep t ~ D1 dInfo (C1 cInfo f)
         , GToSchemaRecord sch fmap args f
         , ToBackendKey (PersistEntityBackend t) t
         , PersistEntityBackend t ~ SqlBackend )
         => ToSchema sch sty (WithEntityNestedId sty fmap (Entity t)) where
  toSchema (WithEntityNestedId (Entity key x))
    = TRecord $ Field (toSchemaKey $ FSchematic $ TRecord (Field (FPrimitive key') :* Nil))
      :* toSchemaRecord (Proxy @fmap) (unM1 $ unM1 $ from x)
    where key' = unSqlBackendKey $ toBackendKey key

class ToSchemaKey (sch :: Schema') (idTy :: Symbol) t | sch t -> idTy where
  toSchemaKey :: FieldValue sch ('TSchematic idTy) -> FieldValue sch t
instance ToSchemaKey sch idTy ('TSchematic idTy) where
  toSchemaKey = id
instance ToSchemaKey sch idTy t => ToSchemaKey sch idTy ('TOption t) where
  toSchemaKey = FOption . Just . toSchemaKey

-- | Simple utility to execute a database operation
--   in any monad which supports 'IO' operations.
--   Note that all logging messages are discarded.
runDb :: MonadIO m
      => SqlBackend
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
      -> m a
runDb = (liftIO .) . flip runSqlPersistM
