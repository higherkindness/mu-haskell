{-# language DataKinds, PolyKinds,
             GADTs, ExistentialQuantification #-}
module Mu.Rpc where

import Data.Kind
import GHC.TypeLits

type Service' = Service Symbol

type Service serviceName
  = [Method serviceName]

data Method serviceName
  = Method serviceName [Object] (Object)

data Object = Single Type | Stream Type

