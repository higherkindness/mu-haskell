{-# language AllowAmbiguousTypes    #-}
{-# language DataKinds              #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language KindSignatures         #-}
{-# language RankNTypes             #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
module Mu.GRpc.Client.Optics (
  GRpcConnection
, initGRpc
, G.GrpcClientConfig
, G.grpcClientConfigSimple
, CompressMode
, GRpcReply(..)
, module Optics.Core
, module Mu.Schema.Optics
) where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import           Data.Conduit
import           Data.Proxy
import           GHC.TypeLits
import           Network.GRPC.Client         (CompressMode)
import qualified Network.GRPC.Client.Helpers as G
import           Network.HTTP2.Client        (ClientError)
import           Optics.Core

import           Mu.GRpc.Client.Internal
import           Mu.Rpc
import           Mu.Schema
import           Mu.Schema.Optics

newtype GRpcConnection (s :: Service Symbol Symbol)
  = GRpcConnection { gcClient  :: G.GrpcClient }

initGRpc :: G.GrpcClientConfig -> forall s. IO (Either ClientError (GRpcConnection s))
initGRpc config = do
  setup <- setupGrpcClient' config
  case setup of
    Left e  -> return $ Left e
    Right c -> return $ Right $ GRpcConnection c

instance forall (serviceName :: Symbol) anns (methods :: [Method Symbol]) (m :: Symbol) (t :: *).
         ( SearchMethodOptic methods m t
         , KnownName serviceName
         , KnownName (FindPackageName anns))
         => LabelOptic m A_Getter
                       (GRpcConnection ('Service serviceName anns methods))
                       (GRpcConnection ('Service serviceName anns methods))
                       t t where
  labelOptic = to (searchMethodOptic (Proxy @methods) (Proxy @m) pkgName svrName . gcClient)
    where pkgName = BS.pack (nameVal (Proxy @(FindPackageName anns)))
          svrName = BS.pack (nameVal (Proxy @serviceName))

class SearchMethodOptic (methods :: [Method Symbol]) (m :: Symbol) t
      | methods m -> t where
  searchMethodOptic ::  Proxy methods -> Proxy m -> ByteString -> ByteString -> G.GrpcClient -> t

{- Not possible due to functional dependency
instance TypeError ('Text "could not find method " ':<>: ShowType m)
         => SearchMethodOptic '[] m t where
-}
instance {-# OVERLAPS #-} MethodOptic ('Method name anns ins outs) t
         => SearchMethodOptic ('Method name anns ins outs ': rest) name t where
  searchMethodOptic _ _ pkg srv = methodOptic pkg srv (Proxy @('Method name anns ins outs))
instance {-# OVERLAPPABLE #-} SearchMethodOptic rest name t
         => SearchMethodOptic ('Method other anns ins outs ': rest) name t where
  searchMethodOptic _ = searchMethodOptic (Proxy @rest)

class GRpcMethodCall method t => MethodOptic (method :: Method Symbol) t
      | method -> t where
  methodOptic :: ByteString -> ByteString -> Proxy method -> G.GrpcClient -> t
  methodOptic = gRpcMethodCall

-- No arguments
instance forall (name :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ ] 'RetNothing) t
         , t ~ IO (GRpcReply ()) )
         => MethodOptic ('Method name anns '[ ] 'RetNothing) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ ] ('RetSingle ('ViaSchema sch r))) t
         , t ~ IO (GRpcReply (Term Maybe sch (sch :/: r))) )
         => MethodOptic ('Method name anns '[ ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ ] ('RetStream ('ViaSchema sch r))) t
         , t ~ IO (ConduitT () (GRpcReply (Term Maybe sch (sch :/: r))) IO ()) )
         => MethodOptic ('Method name anns '[ ] ('RetStream ('ViaSchema sch r))) t
-- Simple arguments
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] 'RetNothing) t
         , t ~ (Term Maybe sch (sch :/: v) -> IO (GRpcReply ())) )
         => MethodOptic ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] 'RetNothing) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] ('RetSingle ('ViaSchema sch r))) t
         , t ~ (Term Maybe sch (sch :/: v) -> IO (GRpcReply (Term Maybe sch (sch :/: r))) ) )
         => MethodOptic ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
         , t ~ (Term Maybe sch (sch :/: v) ->  IO (ConduitT () (GRpcReply (Term Maybe sch (sch :/: r))) IO ()) ) )
         => MethodOptic ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
-- Stream arguments
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ 'ArgStream ('ViaSchema sch v) ] ('RetSingle ('ViaSchema sch r))) t
         , t ~ (CompressMode -> IO (ConduitT (Term Maybe sch (sch :/: v)) Void IO (GRpcReply (Term Maybe sch (sch :/: r))))) )
         => MethodOptic ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t.
         ( GRpcMethodCall ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
         , t ~ (CompressMode -> IO (ConduitT (Term Maybe sch (sch :/: v)) (GRpcReply (Term Maybe sch (sch :/: r))) IO ())) )
         => MethodOptic ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
