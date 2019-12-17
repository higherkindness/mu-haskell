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

module Mu.Adapter.Persistent where

import           Data.Functor.Identity
import           Data.Int                 (Int64)
import           GHC.Generics
import           GHC.TypeLits

import           Database.Persist.Sqlite
import           Mu.Schema
import           Mu.Schema.Class
import           Mu.Schema.Interpretation

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
