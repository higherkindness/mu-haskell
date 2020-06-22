{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleInstances         #-}
{-# language GADTs                     #-}
{-# language MultiParamTypeClasses     #-}
{-# language OverloadedStrings         #-}
{-# language PolyKinds                 #-}
{-# language ScopedTypeVariables       #-}
{-# language TypeApplications          #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
{-|
Description : Protocol-independent declaration of services

This module defines a type-level language to describe
RPC-like microservices independently of the transport
and protocol.
-}
module Mu.Rpc (
  Package', Package(..)
, Service', Service(..), Object
, Method', Method(..), ObjectField
, LookupService, LookupMethod
, TypeRef(..), Argument', Argument(..), Return(..)
, TyInfo(..), RpcInfo(..), ReflectRpcInfo(..)
) where

import           Data.Kind
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.TypeLits
import qualified Language.Haskell.TH       as TH
import           Network.HTTP.Types.Header
import           Type.Reflection

import           Mu.Schema
import           Mu.Schema.Registry

-- | Packages whose names are given by type-level strings.
type Package' = Package Symbol Symbol Symbol (TypeRef Symbol)
-- | Services whose names are given by type-level strings.
type Service' = Service Symbol Symbol Symbol (TypeRef Symbol)
-- | Methods whose names are given by type-level strings.
type Method' = Method Symbol Symbol Symbol (TypeRef Symbol)
-- | Arguments whose names are given by type-level strings.
type Argument' = Argument Symbol Symbol (TypeRef Symbol)

-- | A package is a set of services.
data Package serviceName methodName argName tyRef
  = Package (Maybe serviceName)
            [Service serviceName methodName argName tyRef]

-- | A service is a set of methods.
data Service serviceName methodName argName tyRef
  = Service serviceName
            [Method serviceName methodName argName tyRef]

-- | A method is defined by its name, arguments, and return type.
data Method serviceName methodName argName tyRef
  = Method methodName
           [Argument serviceName argName tyRef]
           (Return serviceName tyRef)

-- Synonyms for GraphQL
-- | An object is a set of fields, in GraphQL lingo.
type Object = 'Service
-- | A field in an object takes some input objects,
--   and returns a value or some other object,
--   in GraphQL lingo.
type ObjectField = 'Method

-- | Look up a service in a package definition using its name.
type family LookupService (ss :: [Service snm mnm anm tr]) (s :: snm)
              :: Service snm mnm anm tr where
  LookupService '[] s = TypeError ('Text "could not find method " ':<>: 'ShowType s)
  LookupService ('Service s ms ': ss) s = 'Service s ms
  LookupService (other         ': ss) s = LookupService ss s

-- | Look up a method in a service definition using its name.
type family LookupMethod (s :: [Method snm mnm anm tr]) (m :: mnm)
              :: Method snm mnm anm tr where
  LookupMethod '[] m = TypeError ('Text "could not find method " ':<>: 'ShowType m)
  LookupMethod ('Method m args r ': ms) m = 'Method m args r
  LookupMethod (other            ': ms) m = LookupMethod ms m

-- | Defines a reference to a type, either primitive or coming from the schema.
--   'TypeRef's are used to define arguments and result types.
data TypeRef serviceName where
  -- | A primitive type.
  PrimitiveRef :: Type -> TypeRef serviceName
  -- | Chain with another service.
  ObjectRef    :: serviceName -> TypeRef serviceName
  -- | Point to schema.
  SchemaRef    :: Schema typeName fieldName -> typeName -> TypeRef serviceName
  -- | Registry subject, type to convert to, and preferred serialization version
  RegistryRef  :: Registry -> Type -> Nat -> TypeRef serviceName
  -- | To be used only during TH generation!
  THRef        :: TH.Type -> TypeRef serviceName
  -- Combinators found in the gRPC and GraphQL languages.
  -- | Represents a list of values.
  ListRef      :: TypeRef serviceName -> TypeRef serviceName
  -- | Represents a possibly-missing value.
  OptionalRef  :: TypeRef serviceName -> TypeRef serviceName

instance Show (TypeRef s) where
  show _ = "ty"

-- | Defines the way in which arguments are handled.
data Argument serviceName argName tyRef where
  -- | Use a single value.
  ArgSingle :: Maybe argName
            -> tyRef
            -> Argument serviceName argName tyRef
  -- | Consume a stream of values.
  ArgStream :: Maybe argName
            -> tyRef
            -> Argument serviceName argName tyRef

-- | Defines the different possibilities for returning
--   information from a method.
data Return serviceName tyRef where
  -- | Fire and forget.
  RetNothing :: Return serviceName tyRef
  -- | Return a single value.
  RetSingle  :: tyRef -> Return serviceName tyRef
  -- | Return a stream of values.
  RetStream  :: tyRef -> Return serviceName tyRef
  -- | Return a value or an error.
  RetThrows  :: tyRef -> tyRef -> Return serviceName tyRef

-- | Reflection

data RpcInfo i
  = NoRpcInfo
  | RpcInfo { packageInfo :: Package Text Text Text TyInfo
            , serviceInfo :: Service Text Text Text TyInfo
            , methodInfo  :: Method  Text Text Text TyInfo
            , headers     :: RequestHeaders
            , extraInfo   :: i
            }

data TyInfo
  = TyList   TyInfo
  | TyOption TyInfo
  | TyTy     Text
  deriving (Show, Eq)

instance Show (RpcInfo i) where
  show NoRpcInfo
    = "<no info>"
  show (RpcInfo (Package Nothing _) (Service s _) (Method m _ _) _ _)
    = T.unpack (s <> ":" <> m)
  show (RpcInfo (Package (Just p) _) (Service s _) (Method m _ _) _ _)
    = T.unpack (p <> ":" <> s <> ":" <> m)

class ReflectRpcInfo (p :: Package') (s :: Service') (m :: Method') where
  reflectRpcInfo :: Proxy p -> Proxy s -> Proxy m -> RequestHeaders -> i -> RpcInfo i
class ReflectService (s :: Service') where
  reflectService :: Proxy s -> Service Text Text Text TyInfo
class ReflectMethod (m :: Method') where
  reflectMethod  :: Proxy m -> Method Text Text Text TyInfo
class ReflectArg (arg :: Argument') where
  reflectArg     :: Proxy arg -> Argument Text Text TyInfo
class ReflectReturn (r :: Return Symbol (TypeRef Symbol)) where
  reflectReturn  :: Proxy r -> Return Text TyInfo
class ReflectTyRef (r :: TypeRef Symbol) where
  reflectTyRef   :: Proxy r -> TyInfo

class KnownMaySymbol (m :: Maybe Symbol) where
  maySymbolVal :: Proxy m -> Maybe Text
instance KnownMaySymbol 'Nothing where
  maySymbolVal _ = Nothing
instance (KnownSymbol s) => KnownMaySymbol ('Just s) where
  maySymbolVal _ = Just $ T.pack $ symbolVal (Proxy @s)

class ReflectServices (ss :: [Service']) where
  reflectServices :: Proxy ss -> [Service Text Text Text TyInfo]
instance ReflectServices '[] where
  reflectServices _ = []
instance (ReflectService s, ReflectServices ss)
         => ReflectServices (s ': ss) where
  reflectServices _ = reflectService (Proxy @s) : reflectServices (Proxy @ss)

class ReflectMethods (ms :: [Method']) where
  reflectMethods :: Proxy ms -> [Method Text Text Text TyInfo]
instance ReflectMethods '[] where
  reflectMethods _ = []
instance (ReflectMethod m, ReflectMethods ms)
         => ReflectMethods (m ': ms) where
  reflectMethods _ = reflectMethod (Proxy @m) : reflectMethods (Proxy @ms)

class ReflectArgs (ms :: [Argument']) where
  reflectArgs :: Proxy ms -> [Argument Text Text TyInfo]
instance ReflectArgs '[] where
  reflectArgs _ = []
instance (ReflectArg m, ReflectArgs ms)
         => ReflectArgs (m ': ms) where
  reflectArgs _ = reflectArg (Proxy @m) : reflectArgs (Proxy @ms)

instance (KnownMaySymbol pname, ReflectServices ss, ReflectService s, ReflectMethod m)
         => ReflectRpcInfo ('Package pname ss) s m where
  reflectRpcInfo _ ps pm req extra
    = RpcInfo (Package (maySymbolVal (Proxy @pname))
                       (reflectServices (Proxy @ss)))
              (reflectService ps) (reflectMethod pm) req extra

instance (KnownSymbol sname, ReflectMethods ms)
         => ReflectService ('Service sname ms) where
  reflectService _
    = Service (T.pack $ symbolVal (Proxy @sname))
              (reflectMethods (Proxy @ms))

instance (KnownSymbol mname, ReflectArgs args, ReflectReturn r)
         => ReflectMethod ('Method mname args r) where
  reflectMethod _
    = Method (T.pack $ symbolVal (Proxy @mname))
             (reflectArgs (Proxy @args)) (reflectReturn (Proxy @r))

instance (KnownMaySymbol aname, ReflectTyRef t)
         => ReflectArg ('ArgSingle aname t) where
  reflectArg _
    = ArgSingle (maySymbolVal (Proxy @aname)) (reflectTyRef (Proxy @t))
instance (KnownMaySymbol aname, ReflectTyRef t)
         => ReflectArg ('ArgStream aname t) where
  reflectArg _
    = ArgStream (maySymbolVal (Proxy @aname)) (reflectTyRef (Proxy @t))

instance ReflectReturn 'RetNothing where
  reflectReturn _ = RetNothing
instance (ReflectTyRef t)
         => ReflectReturn ('RetSingle t) where
  reflectReturn _ = RetSingle (reflectTyRef (Proxy @t))
instance (ReflectTyRef t)
         => ReflectReturn ('RetStream t) where
  reflectReturn _ = RetStream (reflectTyRef (Proxy @t))
instance (ReflectTyRef e, ReflectTyRef t)
         => ReflectReturn ('RetThrows e t) where
  reflectReturn _ = RetThrows (reflectTyRef (Proxy @e))
                              (reflectTyRef (Proxy @t))

instance ReflectTyRef t => ReflectTyRef ('ListRef t) where
  reflectTyRef _ = TyList (reflectTyRef (Proxy @t))
instance ReflectTyRef t => ReflectTyRef ('OptionalRef t) where
  reflectTyRef _ = TyOption (reflectTyRef (Proxy @t))
instance Typeable t => ReflectTyRef ('PrimitiveRef t) where
  reflectTyRef _ = TyTy (T.pack $ show $ typeRep @t)
instance KnownSymbol s => ReflectTyRef ('ObjectRef s) where
  reflectTyRef _ = TyTy (T.pack $ symbolVal $ Proxy @s)
instance KnownSymbol s => ReflectTyRef ('SchemaRef sch s) where
  reflectTyRef _ = TyTy (T.pack $ symbolVal $ Proxy @s)
instance Typeable t => ReflectTyRef ('RegistryRef r t n) where
  reflectTyRef _ = TyTy (T.pack $ show $ typeRep @t)
