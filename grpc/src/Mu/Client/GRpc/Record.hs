{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances, TypeApplications,
             ScopedTypeVariables, AllowAmbiguousTypes #-}
module Mu.Client.GRpc.Record where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import Mu.Client.GRpc
import Mu.Rpc

buildService :: forall (s :: Service') t
                (nm :: Symbol) (anns :: [Annotation]) (ms :: [Method Symbol]).
                (s ~ 'Service nm anns ms, Generic t, BuildService s ms (Rep t))
             => GrpcClient -> t
buildService client = to (buildService' (Proxy @s) (Proxy @ms) client)

class BuildService (s :: Service') (ms :: [Method Symbol]) (f :: * -> *) where
  buildService' :: Proxy s -> Proxy ms -> GrpcClient -> f a

instance BuildService s ms U1 where
  buildService' _ _ _ = U1
instance BuildService s ms f => BuildService s ms (D1 meta f) where
  buildService' ps pms client
    = M1 (buildService' ps pms client)
instance BuildService s ms f => BuildService s ms (C1 meta f) where
  buildService' ps pms client
    = M1 (buildService' ps pms client)
instance TypeError ('Text "building a service from sums is not supported")
         => BuildService s ms (f :+: g) where
  buildService' = error "this should never happen"
instance (BuildService s ms f, BuildService s ms g)
         => BuildService s ms (f :*: g) where
  buildService' ps pms client
    = buildService' ps pms client :*: buildService' ps pms client
instance (GRpcServiceMethodCall s (s :-->: m) h)
         => BuildService s ms (S1 ('MetaSel ('Just m) u ss ds) (K1 i h)) where
  buildService' ps _ client
    = M1 $ K1 $ gRpcServiceMethodCall ps (Proxy @(s :-->: m)) client