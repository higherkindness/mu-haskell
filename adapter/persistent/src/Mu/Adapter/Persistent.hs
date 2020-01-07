{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language KindSignatures        #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
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
import           Data.Functor.Identity
import           Data.Int
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.TypeLits
import           Mu.Schema
import           Mu.Schema.Class
import           Mu.Schema.Interpretation

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
          up (Identity i) = pure i

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
          up (Identity i) = pure i

-- | Simple utility to execute a database operation
--   in any monad which supports 'IO' operations.
--   Note that all logging messages are discarded.
runDb :: MonadIO m
      => SqlBackend
      -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
      -> m a
runDb = (liftIO .) . flip runSqlPersistM
