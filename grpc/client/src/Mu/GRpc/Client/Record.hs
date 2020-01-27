{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Client for gRPC services using plain Haskell records

For further information over initialization of the connection,
consult the <http://hackage.haskell.org/package/http2-client-grpc http2-client-grpc docs>.
-}
module Mu.GRpc.Client.Record (
  -- * Initialization of the gRPC client
  GrpcClient
, GrpcClientConfig
, grpcClientConfigSimple
, setupGrpcClient'
  -- * Fill and generate the Haskell record of functions
, buildService
, GRpcMessageProtocol(..)
, CompressMode(..)
, GRpcReply(..)
, generateRecordFromService
) where

import           Control.Applicative
import           Data.Char
import           Data.Conduit                 (ConduitT)
import           Data.Proxy
import           Data.Void
import           GHC.Generics                 hiding (NoSourceStrictness, NoSourceUnpackedness)
import           GHC.TypeLits
import           Language.Haskell.TH          hiding (ppr)
import           Language.Haskell.TH.Datatype

import           Network.GRPC.Client          (CompressMode (..))
import           Network.GRPC.Client.Helpers

import Mu.GRpc.Bridge
import           Mu.GRpc.Client.Internal
import           Mu.Rpc

-- | Fills in a Haskell record of functions with the corresponding
--   calls to gRPC services from a Mu 'Service' declaration.
buildService :: forall (pro :: GRpcMessageProtocol) (s :: Service') (p :: Symbol) t
                (nm :: Symbol) (anns :: [ServiceAnnotation]) (ms :: [Method Symbol]).
                (s ~ 'Service nm anns ms, Generic t, BuildService pro s p ms (Rep t))
             => GrpcClient -> t
buildService client = to (buildService' (Proxy @pro) (Proxy @s) (Proxy @p) (Proxy @ms) client)

class BuildService (pro :: GRpcMessageProtocol) (s :: Service')
                   (p :: Symbol) (ms :: [Method Symbol]) (f :: * -> *) where
  buildService' :: Proxy pro -> Proxy s -> Proxy p -> Proxy ms -> GrpcClient -> f a

instance BuildService pro s p ms U1 where
  buildService' _ _ _ _ _ = U1
instance BuildService pro s p ms f => BuildService pro s p ms (D1 meta f) where
  buildService' ppro ps ppr pms client
    = M1 (buildService' ppro ps ppr pms client)
instance BuildService pro s p ms f => BuildService pro s p ms (C1 meta f) where
  buildService' ppro ps ppr pms client
    = M1 (buildService' ppro ps ppr pms client)
instance TypeError ('Text "building a service from sums is not supported")
         => BuildService pro s p ms (f :+: g) where
  buildService' = error "this should never happen"
instance (BuildService pro s p ms f, BuildService pro s p ms g)
         => BuildService pro s p ms (f :*: g) where
  buildService' ppro ps ppr pms client
    = buildService' ppro ps ppr pms client :*: buildService' ppro ps ppr pms client
instance (m ~ AppendSymbol p x, GRpcServiceMethodCall pro s (s :-->: x) h)
         => BuildService pro s p ms (S1 ('MetaSel ('Just m) u ss ds) (K1 i h)) where
  buildService' ppro ps _ _ client
    = M1 $ K1 $ gRpcServiceMethodCall ppro ps (Proxy @(s :-->: x)) client

-- TEMPLATE HASKELL
-- ================

-- | Generate the plain Haskell record corresponding to
--   a Mu 'Service' definition, and a concrete implementation
--   of 'buildService' for that record.
generateRecordFromService :: String -> String -> Namer -> Name -> Q [Dec]
generateRecordFromService newRecordName fieldsPrefix tNamer serviceTyName
  = do let serviceTy = ConT serviceTyName
       srvDef <- typeToServiceDef serviceTy
       case srvDef of
         Nothing -> fail "service definition cannot be parsed"
         Just sd -> serviceDefToDecl serviceTyName newRecordName fieldsPrefix tNamer sd

type Namer = String -> String

serviceDefToDecl :: Name -> String -> String -> Namer -> Service String String -> Q [Dec]
serviceDefToDecl serviceTyName complete fieldsPrefix tNamer (Service _ _ methods)
  = do d <- dataD (pure [])
                  (mkName complete)
                  []
                  Nothing
                  [RecC (mkName complete) <$> mapM (methodToDecl fieldsPrefix tNamer) methods]
                  [pure (DerivClause Nothing [ConT ''Generic])]
       let buildName = mkName ("build" ++ complete)
       s <- SigD buildName <$> [t|GrpcClient -> $(return (ConT (mkName complete)))|]
       c <- Clause <$> pure []
                   <*> (NormalB <$> [e|buildService @($(return $ ConT serviceTyName))
                                                    @($(return $ LitT (StrTyLit fieldsPrefix)))|])
                   <*> pure []
       return [d, s, FunD buildName [c]]

methodToDecl :: String -> Namer -> Method String -> Q (Name, Bang, Type)
methodToDecl fieldsPrefix tNamer (Method mName _ args ret)
  = do let nm = firstLower (fieldsPrefix ++ mName)
       ty <- computeMethodType tNamer args ret
       return ( mkName nm, Bang NoSourceUnpackedness NoSourceStrictness, ty )

computeMethodType :: Namer -> [Argument] -> Return -> Q Type
computeMethodType _ [] RetNothing
  = [t|IO (GRpcReply ())|]
computeMethodType n [] (RetSingle r)
  = [t|IO (GRpcReply $(typeRefToType n r))|]
computeMethodType n [ArgSingle v] RetNothing
  = [t|$(typeRefToType n v) -> IO (GRpcReply ())|]
computeMethodType n [ArgSingle v] (RetSingle r)
  = [t|$(typeRefToType n v) -> IO (GRpcReply $(typeRefToType n r))|]
computeMethodType n [ArgStream v] (RetSingle r)
  = [t|CompressMode -> IO (ConduitT $(typeRefToType n v) Void IO (GRpcReply $(typeRefToType n r)))|]
computeMethodType n [ArgSingle v] (RetStream r)
  = [t|$(typeRefToType n v) -> IO (ConduitT () (GRpcReply $(typeRefToType n r)) IO ())|]
computeMethodType n [ArgStream v] (RetStream r)
  = [t|CompressMode -> IO (ConduitT $(typeRefToType n v) (GRpcReply $(typeRefToType n r)) IO ())|]
computeMethodType _ _ _ = fail "method signature not supported"

typeRefToType :: Namer -> TypeRef -> Q Type
typeRefToType tNamer (ViaTH (LitT (StrTyLit s)))
  = return $ ConT (mkName $ completeName tNamer s)
typeRefToType _tNamer (ViaTH ty)
  = return ty
typeRefToType _ _ = error "this should never happen"

completeName :: Namer -> String -> String
completeName namer name = firstUpper (namer (firstUpper name))

firstUpper :: String -> String
firstUpper []       = error "Empty names are not allowed"
firstUpper (x:rest) = toUpper x : rest

firstLower :: String -> String
firstLower []       = error "Empty names are not allowed"
firstLower (x:rest) = toLower x : rest

-- Parsing
-- =======

typeToServiceDef :: Type -> Q (Maybe (Service String String))
typeToServiceDef toplevelty
  = typeToServiceDef' <$> resolveTypeSynonyms toplevelty
  where
    typeToServiceDef' :: Type -> Maybe (Service String String)
    typeToServiceDef' expanded
      = do (sn, _, methods) <- tyD3 'Service expanded
           methods' <- tyList methods
           Service <$> tyString sn
                   <*> pure []
                   <*> mapM typeToMethodDef methods'

    typeToMethodDef :: Type -> Maybe (Method String)
    typeToMethodDef ty
      = do (mn, _, args, ret) <- tyD4 'Method ty
           args' <- tyList args
           Method <$> tyString mn
                  <*> pure []
                  <*> mapM typeToArgDef args'
                  <*> typeToRetDef ret

    typeToArgDef :: Type -> Maybe Argument
    typeToArgDef ty
      =   ArgSingle <$> (tyD1 'ArgSingle ty >>= typeToTypeRef)
      <|> ArgStream <$> (tyD1 'ArgStream ty >>= typeToTypeRef)

    typeToRetDef :: Type -> Maybe Return
    typeToRetDef ty
      =   RetNothing <$ tyD0 'RetNothing ty
      <|> RetSingle <$> (tyD1 'RetSingle ty >>= typeToTypeRef)
      <|> (do (e, v) <- tyD2 'RetThrows ty
              RetThrows <$> typeToTypeRef e <*> typeToTypeRef v)
      <|> RetStream <$> (tyD1 'RetStream ty >>= typeToTypeRef)

    typeToTypeRef :: Type -> Maybe TypeRef
    typeToTypeRef ty
      =   (do (_,innerTy) <- tyD2 'ViaSchema ty
              return (ViaTH innerTy))
      <|> (do (_,innerTy,_) <- tyD3 'ViaRegistry ty
              return (ViaTH innerTy))

tyString :: Type -> Maybe String
tyString (SigT t _)
  = tyString t
tyString (LitT (StrTyLit s))
  = Just s
tyString _
  = Nothing

tyList :: Type -> Maybe [Type]
tyList (SigT t _)
  = tyList t
tyList PromotedNilT
  = Just []
tyList (AppT (AppT PromotedConsT ty) rest)
  = (ty :) <$> tyList rest
tyList _ = Nothing

tyD0 :: Name -> Type -> Maybe ()
tyD0 name (SigT t _) = tyD0 name t
tyD0 name (PromotedT c)
  | c == name = Just ()
  | otherwise = Nothing
tyD0 _ _ = Nothing

tyD1 :: Name -> Type -> Maybe Type
tyD1 name (SigT t _) = tyD1 name t
tyD1 name (AppT (PromotedT c) x)
  | c == name = Just x
  | otherwise = Nothing
tyD1 _ _ = Nothing

tyD2 :: Name -> Type -> Maybe (Type, Type)
tyD2 name (SigT t _) = tyD2 name t
tyD2 name (AppT (AppT (PromotedT c) x) y)
  | c == name = Just (x, y)
  | otherwise = Nothing
tyD2 _ _ = Nothing

tyD3 :: Name -> Type -> Maybe (Type, Type, Type)
tyD3 name (SigT t _) = tyD3 name t
tyD3 name (AppT (AppT (AppT (PromotedT c) x) y) z)
  | c == name = Just (x, y, z)
  | otherwise = Nothing
tyD3 _ _ = Nothing

tyD4 :: Name -> Type -> Maybe (Type, Type, Type, Type)
tyD4 name (SigT t _) = tyD4 name t
tyD4 name (AppT (AppT (AppT (AppT (PromotedT c) x) y) z) u)
  | c == name = Just (x, y, z, u)
  | otherwise = Nothing
tyD4 _ _ = Nothing
