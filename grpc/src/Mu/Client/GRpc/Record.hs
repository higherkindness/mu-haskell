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

buildService :: forall (s :: Service') (p :: Symbol) t
                (nm :: Symbol) (anns :: [Annotation]) (ms :: [Method Symbol]).
                (s ~ 'Service nm anns ms, Generic t, BuildService s p ms (Rep t))
             => GrpcClient -> t
buildService client = to (buildService' (Proxy @s) (Proxy @p) (Proxy @ms) client)

class BuildService (s :: Service') (p :: Symbol) (ms :: [Method Symbol]) (f :: * -> *) where
  buildService' :: Proxy s -> Proxy p -> Proxy ms -> GrpcClient -> f a

instance BuildService s p ms U1 where
  buildService' _ _ _ _ = U1
instance BuildService s p ms f => BuildService s p ms (D1 meta f) where
  buildService' ps ppr pms client
    = M1 (buildService' ps ppr pms client)
instance BuildService s p ms f => BuildService s p ms (C1 meta f) where
  buildService' ps ppr pms client
    = M1 (buildService' ps ppr pms client)
instance TypeError ('Text "building a service from sums is not supported")
         => BuildService s p ms (f :+: g) where
  buildService' = error "this should never happen"
instance (BuildService s p ms f, BuildService s p ms g)
         => BuildService s p ms (f :*: g) where
  buildService' ps ppr pms client
    = buildService' ps ppr pms client :*: buildService' ps ppr pms client
instance (m ~ AppendSymbol p x, GRpcServiceMethodCall s (s :-->: x) h)
         => BuildService s p ms (S1 ('MetaSel ('Just m) u ss ds) (K1 i h)) where
  buildService' ps _ _ client
    = M1 $ K1 $ gRpcServiceMethodCall ps (Proxy @(s :-->: x)) client