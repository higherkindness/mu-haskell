{-# language DataKinds         #-}
{-# language KindSignatures    #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-|
Description : Quasi-quoters for gRPC files

Read @.proto@ files as a 'Mu.Schema.Definition.Schema'
and a set of 'Service's.
-}
module Mu.Quasi.GRpc (
  grpc
) where

import           Control.Monad.IO.Class
import qualified Data.Text                       as T
import           GHC.TypeLits
import           Language.Haskell.TH
import           Language.ProtocolBuffers.Parser
import qualified Language.ProtocolBuffers.Types  as P
import           Network.HTTP.Client
import           Servant.Client.Core.BaseUrl

import           Mu.Quasi.ProtoBuf
import           Mu.Rpc

-- | Reads a @.proto@ file and generates:
--   * A 'Mu.Schema.Definition.Schema' with all the message
--     types, using the name given as first argument.
--   * A 'Service' declaration for each service in the file,
--     where the name is obtained by applying the function
--     given as second argument to the name in the file.
grpc :: String -> (String -> String) -> FilePath -> Q [Dec]
grpc schemaName servicePrefix fp
  = do r <- liftIO $ parseProtoBufFile fp
       case r of
         Left e
           -> fail ("could not parse protocol buffers spec: " ++ show e)
         Right p
           -> grpcToDecls schemaName servicePrefix p

grpcToDecls :: String -> (String -> String) -> P.ProtoBuf -> Q [Dec]
grpcToDecls schemaName servicePrefix p@P.ProtoBuf { P.package = pkg, P.services = srvs }
  = do let schemaName' = mkName schemaName
       schemaDec <- protobufToDecls schemaName p
       serviceTy <- mapM (pbServiceDeclToDec servicePrefix pkg schemaName') srvs
       pure (schemaDec ++ serviceTy)

pbServiceDeclToDec :: (String -> String) -> Maybe [T.Text] -> Name -> P.ServiceDeclaration -> Q Dec
pbServiceDeclToDec servicePrefix pkg schema srv@(P.Service nm _ _)
  = tySynD (mkName $ servicePrefix $ T.unpack nm) []
           (pbServiceDeclToType pkg schema srv)

pbServiceDeclToType :: Maybe [T.Text] -> Name -> P.ServiceDeclaration -> Q Type
pbServiceDeclToType pkg schema (P.Service nm _ methods)
  = [t| 'Package $(pkgType pkg)
          '[ 'Service $(textToStrLit nm)
                      $(typesToList <$> mapM (pbMethodToType schema) methods) ] |]
  where
    pkgType Nothing  = [t| ('Nothing :: Maybe Symbol) |]
    pkgType (Just p) = [t| 'Just $(textToStrLit (T.intercalate "." p)) |]

pbMethodToType :: Name -> P.Method -> Q Type
pbMethodToType s (P.Method nm vr v rr r _)
  = [t| 'Method $(textToStrLit nm)
                $(argToType vr v) $(retToType rr r) |]
  where
    argToType P.Single (P.TOther ["google","protobuf","Empty"])
      = [t| '[ ] |]
    argToType P.Single (P.TOther a)
      = [t| '[ 'ArgSingle ('Nothing :: Maybe Symbol) ('SchemaRef $(schemaTy s) $(textToStrLit (T.intercalate "." a))) ] |]
    argToType P.Stream (P.TOther a)
      = [t| '[ 'ArgStream ('Nothing :: Maybe Symbol) ('SchemaRef $(schemaTy s) $(textToStrLit (T.intercalate "." a))) ] |]
    argToType _ _
      = fail "only message types may be used as arguments"

    retToType P.Single (P.TOther ["google","protobuf","Empty"])
      = [t| 'RetNothing |]
    retToType P.Single (P.TOther a)
      = [t| 'RetSingle ('SchemaRef $(schemaTy s) $(textToStrLit (T.intercalate "." a))) |]
    retToType P.Stream (P.TOther a)
      = [t| 'RetStream ('SchemaRef $(schemaTy s) $(textToStrLit (T.intercalate "." a))) |]
    retToType _ _
      = fail "only message types may be used as results"

schemaTy :: Name -> Q Type
schemaTy schema = pure $ ConT schema

typesToList :: [Type] -> Type
typesToList
  = foldr (AppT . AppT PromotedConsT) PromotedNilT
textToStrLit :: T.Text -> Q Type
textToStrLit s
  = pure $ LitT $ StrTyLit $ T.unpack s
