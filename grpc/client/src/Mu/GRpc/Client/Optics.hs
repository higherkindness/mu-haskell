{-# language AllowAmbiguousTypes    #-}
{-# language DataKinds              #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language RankNTypes             #-}
{-# language ScopedTypeVariables    #-}
{-# language TypeApplications       #-}
{-# language TypeFamilies           #-}
{-# language TypeOperators          #-}
{-# language UndecidableInstances   #-}
{-|
Description : Client for gRPC services using optics and labels

For further information over initialization of the connection,
consult the <http://hackage.haskell.org/package/http2-client-grpc http2-client-grpc docs>.
-}
module Mu.GRpc.Client.Optics (
  -- * Initialization of the gRPC client
  GRpcConnection
, initGRpc
, GRpcMessageProtocol(..)
, msgProtoBuf
, msgAvro
, G.GrpcClientConfig
, G.grpcClientConfigSimple
  -- * Request arguments and responses
, CompressMode
, GRpcReply(..)
  -- * Re-exported for convenience
, module Optics.Core
, module Mu.Schema.Optics
) where

import qualified Data.ByteString.Char8       as BS
import           Data.Conduit
import           Data.Functor.Identity
import           Data.Proxy
import           GHC.TypeLits
import           Network.GRPC.Client         (CompressMode)
import qualified Network.GRPC.Client.Helpers as G
import           Network.HTTP2.Client        (ClientError)
import           Optics.Core

import           Mu.GRpc.Bridge
import           Mu.GRpc.Client.Internal
import           Mu.Rpc
import           Mu.Schema
import           Mu.Schema.Optics

-- | Represents a connection to the service @s@.
newtype GRpcConnection (s :: Service Symbol Symbol) (p :: GRpcMessageProtocol)
  = GRpcConnection { gcClient  :: G.GrpcClient }

-- | Initializes a connection to a gRPC server.
--   Usually the service you are connecting to is
--   inferred from the usage later on.
--   However, it can also be made explicit by using
--
--   > initGRpc config @Service
--
initGRpc :: G.GrpcClientConfig  -- ^ gRPC configuration
         -> Proxy p
         -> forall s. IO (Either ClientError (GRpcConnection s p))
initGRpc config _ = do
  setup <- setupGrpcClient' config
  case setup of
    Left e  -> return $ Left e
    Right c -> return $ Right $ GRpcConnection c

instance forall (serviceName :: Symbol) anns (methods :: [Method Symbol]) (m :: Symbol)
                (t :: *) (p :: GRpcMessageProtocol).
         ( SearchMethodOptic p methods m t
         , KnownName serviceName
         , KnownName (FindPackageName anns)
         , KnownName m
         , MkRPC p )
         => LabelOptic m A_Getter
                       (GRpcConnection ('Service serviceName anns methods) p)
                       (GRpcConnection ('Service serviceName anns methods) p)
                       t t where
  labelOptic = to (searchMethodOptic @p (Proxy @methods) (Proxy @m) rpc . gcClient)
    where pkgName = BS.pack (nameVal (Proxy @(FindPackageName anns)))
          svrName = BS.pack (nameVal (Proxy @serviceName))
          metName = BS.pack (nameVal (Proxy @m))
          rpc = mkRPC (Proxy @p) pkgName svrName metName

class SearchMethodOptic (p :: GRpcMessageProtocol) (methods :: [Method Symbol]) (m :: Symbol) t
      | p methods m -> t where
  searchMethodOptic :: Proxy methods -> Proxy m -> RPCTy p -> G.GrpcClient -> t

{- Not possible due to functional dependency
instance TypeError ('Text "could not find method " ':<>: ShowType m)
         => SearchMethodOptic '[] m t where
-}
instance {-# OVERLAPS #-} MethodOptic p ('Method name anns ins outs) t
         => SearchMethodOptic p ('Method name anns ins outs ': rest) name t where
  searchMethodOptic _ _ rpc = methodOptic @p rpc (Proxy @('Method name anns ins outs))
instance {-# OVERLAPPABLE #-} SearchMethodOptic p rest name t
         => SearchMethodOptic p ('Method other anns ins outs ': rest) name t where
  searchMethodOptic _ = searchMethodOptic @p (Proxy @rest)

class GRpcMethodCall p method t
      => MethodOptic (p :: GRpcMessageProtocol) (method :: Method Symbol) t
      | p method -> t where
  methodOptic :: RPCTy p -> Proxy method -> G.GrpcClient -> t
  methodOptic = gRpcMethodCall @p

class ProtocolWrapper (p :: GRpcMessageProtocol) (w :: * -> *) | p -> w where
instance ProtocolWrapper 'MsgAvro Identity where
instance ProtocolWrapper 'MsgProtoBuf Maybe where

-- No arguments
instance forall (name :: Symbol) anns t p.
         ( GRpcMethodCall p ('Method name anns '[ ] 'RetNothing) t
         , t ~ IO (GRpcReply ()) )
         => MethodOptic p ('Method name anns '[ ] 'RetNothing) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ ] ('RetSingle ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ IO (GRpcReply (Term w sch (sch :/: r))) )
         => MethodOptic p ('Method name anns '[ ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ ] ('RetStream ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ IO (ConduitT () (GRpcReply (Term w sch (sch :/: r))) IO ()) )
         => MethodOptic p ('Method name anns '[ ] ('RetStream ('ViaSchema sch r))) t
-- Simple arguments
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] 'RetNothing) t
         , ProtocolWrapper p w
         , t ~ (Term w sch (sch :/: v) -> IO (GRpcReply ())) )
         => MethodOptic p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] 'RetNothing) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v) ] ('RetSingle ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ (Term w sch (sch :/: v)
               -> IO (GRpcReply (Term w sch (sch :/: r))) ) )
         => MethodOptic p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ (Term w sch (sch :/: v)
                ->  IO (ConduitT () (GRpcReply (Term Maybe sch (sch :/: r))) IO ()) ) )
         => MethodOptic p ('Method name anns '[ 'ArgSingle ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
-- Stream arguments
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ 'ArgStream ('ViaSchema sch v) ] ('RetSingle ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ (CompressMode
                -> IO (ConduitT (Term w sch (sch :/: v))
                                Void IO
                                (GRpcReply (Term w sch (sch :/: r))))) )
         => MethodOptic p ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetSingle ('ViaSchema sch r))) t
instance forall (name :: Symbol) (sch :: Schema Symbol Symbol) (v :: Symbol) (r :: Symbol) anns t p w.
         ( GRpcMethodCall p ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
         , ProtocolWrapper p w
         , t ~ (CompressMode
               -> IO (ConduitT (Term w sch (sch :/: v))
                               (GRpcReply (Term w sch (sch :/: r))) IO ())) )
         => MethodOptic p ('Method name anns '[ 'ArgStream ('ViaSchema sch v)  ] ('RetStream ('ViaSchema sch r))) t
