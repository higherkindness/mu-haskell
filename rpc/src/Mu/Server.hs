{-# language DataKinds, PolyKinds,
             GADTs, TypeFamilies,
             MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators #-}

module Mu.Server where

import Data.Conduit
import Data.Kind
import Data.SOP

import Mu.Rpc
import Mu.Schema

type ServerT sch m = NP (HandlerT sch m)

data HandlerT sch m method where
  Handler :: Handles sch m args ret
          => Handler' sch m args ret
          -> HandlerT sch m ('Method name args ret)

class Handles (sch :: Schema tn fn) (m :: Type -> Type)
              (args :: [Object]) (ret :: Object) where
  type Handler' sch m args ret :: Type

-- Arguments
instance (HasSchema sch sty t, Handles sch m args r)
         => Handles sch m ('Single t ': args) r where
  type Handler' sch m ('Single t ': args) r
    = t -> Handler' sch m args r
instance (HasSchema sch sty t, Handles sch m args r)
         => Handles sch m ('Stream t ': args) r where
  type Handler' sch m ('Stream t ': args) r
    = ConduitT () t m () -> Handler' sch m args r

-- Results
instance HasSchema sch sty t => Handles sch m '[] ('Single t) where
  type Handler' sch m '[] ('Single t) = m t
instance HasSchema sch sty t => Handles sch m '[] ('Stream t) where
  type Handler' sch m '[] ('Stream t) = ConduitT t Void m ()
