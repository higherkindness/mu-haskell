{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-|
Description : Quasi-quoters for gRPC files

Read @.proto@ files as a 'Mu.Schema.Definition.Schema'
and a set of 'Service's. The origin of those @.proto@
files can be local (if using 'grpc') or come
from a Compendium Registry (if using 'compendium').
-}
module Mu.Quasi.GRpc (
  grpc
, compendium
) where

import           Control.Monad.IO.Class
import qualified Data.Text                       as T
import           Language.Haskell.TH
import           Language.ProtocolBuffers.Parser
import qualified Language.ProtocolBuffers.Types  as P
import           Network.HTTP.Client
import           Servant.Client.Core.BaseUrl

import           Compendium.Client
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

-- | Obtains a schema and service definition from Compendium,
--   and generates the declarations from 'grpc'.
compendium :: String -> (String -> String)
           -> String -> String -> Q [Dec]
compendium schemaTypeName servicePrefix baseUrl identifier
  = do m <- liftIO $ newManager defaultManagerSettings
       u <- liftIO $ parseBaseUrl baseUrl
       r <- liftIO $ obtainProtoBuf m u (T.pack identifier)
       case r of
         Left e
           -> fail ("could not parse protocol buffers spec: " ++ show e)
         Right p
           -> grpcToDecls schemaTypeName servicePrefix p

grpcToDecls :: String -> (String -> String) -> P.ProtoBuf -> Q [Dec]
grpcToDecls schemaName servicePrefix p@P.ProtoBuf { P.package = pkg, P.services = srvs }
  = do let schemaName' = mkName schemaName
       schemaDec <- protobufToDecls schemaName p
       serviceTy <- mapM (pbServiceDeclToDec servicePrefix pkg schemaName') srvs
       return (schemaDec ++ serviceTy)

pbServiceDeclToDec :: (String -> String) -> Maybe [T.Text] -> Name -> P.ServiceDeclaration -> Q Dec
pbServiceDeclToDec servicePrefix pkg schema srv@(P.Service nm _ _)
  = tySynD (mkName $ servicePrefix $ T.unpack nm) []
           (pbServiceDeclToType pkg schema srv)

pbServiceDeclToType :: Maybe [T.Text] -> Name -> P.ServiceDeclaration -> Q Type
pbServiceDeclToType pkg schema (P.Service nm _ methods)
  = [t| 'Service $(textToStrLit nm) $(pkgType pkg)
                 $(typesToList <$> mapM (pbMethodToType schema) methods) |]
  where
    pkgType Nothing  = [t| '[] |]
    pkgType (Just p) = [t| '[ Package $(textToStrLit (T.intercalate "." p)) ] |]

pbMethodToType :: Name -> P.Method -> Q Type
pbMethodToType s (P.Method nm vr v rr r _)
  = [t| 'Method $(textToStrLit nm) '[]
                $(argToType vr v) $(retToType rr r) |]
  where
    argToType P.Single (P.TOther ["google","protobuf","Empty"])
      = [t| '[ ] |]
    argToType P.Single (P.TOther a)
      = [t| '[ 'ArgSingle ('ViaSchema $(schemaTy s) $(textToStrLit (last a))) ] |]
    argToType P.Stream (P.TOther a)
      = [t| '[ 'ArgStream ('ViaSchema $(schemaTy s) $(textToStrLit (last a))) ] |]
    argToType _ _
      = fail "only message types may be used as arguments"

    retToType P.Single (P.TOther ["google","protobuf","Empty"])
      = [t| 'RetNothing |]
    retToType P.Single (P.TOther a)
      = [t| 'RetSingle ('ViaSchema $(schemaTy s) $(textToStrLit (last a))) |]
    retToType P.Stream (P.TOther a)
      = [t| 'RetStream ('ViaSchema $(schemaTy s) $(textToStrLit (last a))) |]
    retToType _ _
      = fail "only message types may be used as results"

schemaTy :: Name -> Q Type
schemaTy schema = return $ ConT schema

typesToList :: [Type] -> Type
typesToList
  = foldr (\y ys -> AppT (AppT PromotedConsT y) ys) PromotedNilT
textToStrLit :: T.Text -> Q Type
textToStrLit s
  = return $ LitT $ StrTyLit $ T.unpack s
