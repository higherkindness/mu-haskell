{-# language DataKinds, PolyKinds,
             GADTs, TypeFamilies,
             ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators #-}

module Mu.Server where

import Data.Conduit
import Data.Kind
import Data.SOP

import Mu.Rpc

type ServerT m = NP (HandlerT m)
type ServerIO  = ServerT IO

data HandlerT m method where
  Handler :: forall m h args ret name. Handles args ret m h
          => h -> HandlerT m ('Method name args ret)
type HandlerIO = HandlerT IO

-- Define a relation for handling
class Handles (args :: [Argument]) (ret :: Return)
              (m :: Type -> Type) (h :: Type)

-- Arguments
instance (HasSchema' sty t, Handles args ret m h)
         => Handles ('ArgSingle sty ': args) ret m
                    (t -> h)
instance (HasSchema' sty t, Handles args ret m h)
         => Handles ('ArgStream sty ': args) ret m
                    (ConduitT () t m () -> h)
-- Result with exception
instance (HasSchema' ety e, HasSchema' vty v)
         => Handles '[] ('RetThrows ety vty) m
                    (m (Either e v))
instance (HasSchema' vty v)
         => Handles '[] ('RetSingle vty) m (m v)
instance (HasSchema' vty v)
         => Handles '[] ('RetStream vty) m (ConduitT v Void m ())