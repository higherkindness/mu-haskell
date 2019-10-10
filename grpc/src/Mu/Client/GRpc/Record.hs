{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances, TypeApplications,
             ScopedTypeVariables, AllowAmbiguousTypes #-}
module Mu.Client.GRpc.Record where

import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

import Mu.Client.GRpc
import Mu.Rpc

buildService :: forall (pkg :: Symbol) (s :: Service')
                       t (nm :: Symbol) (ms :: [Method Symbol]).
                (s ~ 'Service nm ms, KnownSymbol pkg, Generic t, BuildService s ms (Rep t))
             => GrpcClient -> t
buildService client = to (buildService' pkgName (Proxy @s) (Proxy @ms) client)
  where pkgName = BS.pack (symbolVal (Proxy @pkg))

class BuildService (s :: Service') (ms :: [Method Symbol]) (f :: * -> *) where
  buildService' :: ByteString -> Proxy s -> Proxy ms -> GrpcClient -> f a

instance BuildService s ms U1 where
  buildService' _ _ _ _ = U1
instance BuildService s ms f => BuildService s ms (D1 meta f) where
  buildService' pkg ps pms client
    = M1 (buildService' pkg ps pms client)
instance BuildService s ms f => BuildService s ms (C1 meta f) where
  buildService' pkg ps pms client
    = M1 (buildService' pkg ps pms client)
instance TypeError ('Text "building a service from sums is not supported")
         => BuildService s ms (f :+: g) where
  buildService' = error "this should never happen"
instance (BuildService s ms f, BuildService s ms g)
         => BuildService s ms (f :*: g) where
  buildService' pkg ps pms client
    = buildService' pkg ps pms client :*: buildService' pkg ps pms client
instance (GRpcServiceMethodCall s (s :-->: m) h)
         => BuildService s ms (S1 ('MetaSel ('Just m) u ss ds) (K1 i h)) where
  buildService' pkg ps _ client
    = M1 $ K1 $ gRpcServiceMethodCall pkg ps (Proxy @(s :-->: m)) client