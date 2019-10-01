{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             FlexibleInstances, FlexibleContexts,
             UndecidableInstances,
             TypeApplications, TypeOperators,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fprint-explicit-foralls -fprint-explicit-kinds #-}
module Mu.Server.GRpc where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Kind
import Data.Proxy
import Network.GRPC.Server.Wai (ServiceHandler)
import Network.GRPC.Server.Handlers.NoLens

import Mu.Rpc
import Mu.Server
import Mu.Schema
import Mu.Schema.Adapter.ProtoBuf

gRpcServer :: forall name methods handlers.
              (KnownName name, GRpcServiceHandlers methods handlers)
           => ByteString -> ServerIO ('Service name methods) handlers
           -> [ServiceHandler]
gRpcServer p (Server svr) = gRpcServiceHandlers p serviceName svr
  where serviceName = BS.pack (nameVal (Proxy @name))
        
class GRpcServiceHandlers (ms :: [Method mnm]) (hs :: [Type]) where
  gRpcServiceHandlers :: ByteString -> ByteString
                      -> HandlersIO ms hs -> [ServiceHandler]

instance GRpcServiceHandlers '[] '[] where
  gRpcServiceHandlers _ _ H0 = []
instance (KnownName name, GRpcServiceHandler args r h, GRpcServiceHandlers rest hs)
         => GRpcServiceHandlers ('Method name args r ': rest) (h ': hs) where
  gRpcServiceHandlers p s (h :<|>: rest)
    = gRpcServiceHandler (Proxy @args) (Proxy @r) (RPC p s methodName) h
      : gRpcServiceHandlers p s rest
    where methodName = BS.pack (nameVal (Proxy @name))

class GRpcServiceHandler args r h where
  gRpcServiceHandler :: Proxy args -> Proxy r -> RPC -> h -> ServiceHandler

instance (HasProtoSchema vsch vty v, HasProtoSchema rsch rty r)
         => GRpcServiceHandler '[ 'ArgSingle '(vsch, vty) ] ('RetSingle '(rsch, rty)) (v -> IO r) where
  gRpcServiceHandler _ _ rpc h
    = unary (fromProtoViaSchema @vsch, toProtoViaSchema @rsch) rpc (\_req -> h)
