{-# language CPP             #-}
{-# language DataKinds       #-}
{-# language LambdaCase      #-}
{-# language NamedFieldPuns  #-}
{-# language TemplateHaskell #-}
{-|
Description : Quasi-quoters for Protocol Buffers schemas

Read @.proto@ files as a 'Mu.Schema.Definition.Schema'.
If you want to get the service definitions too,
you should use 'Mu.Quasi.GRpc' instead.
-}
module Mu.Quasi.ProtoBuf (
  -- * Quasi-quoters for @.proto@ files
    protobuf
  -- * Only for internal use
  , protobufToDecls
  ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString                 as B
import           Data.Int
import qualified Data.Text                       as T
import           Language.Haskell.TH
import           Language.ProtocolBuffers.Parser
import qualified Language.ProtocolBuffers.Types  as P

import           Mu.Adapter.ProtoBuf
import           Mu.Schema.Definition
import           Mu.Schema.Annotations

-- | Reads a @.proto@ file and generates a 'Mu.Schema.Definition.Schema'
--   with all the message types, using the name given
--   as first argument.
protobuf :: String -> FilePath -> Q [Dec]
protobuf schemaName fp
  = do r <- liftIO $ parseProtoBufFile fp
       case r of
         Left e
           -> fail ("could not parse protocol buffers spec: " ++ show e)
         Right p
           -> protobufToDecls schemaName p

-- | Shared portion of Protocol Buffers and gRPC quasi-quoters.
protobufToDecls :: String -> P.ProtoBuf -> Q [Dec]
protobufToDecls schemaName p
  = do let schemaName' = mkName schemaName
       (schTy, annTy) <- schemaFromProtoBuf p
       schemaDec <- tySynD schemaName' [] (return schTy)
#if MIN_VERSION_template_haskell(2,15,0)
       annDec <- tySynInstD (tySynEqn Nothing
                               [t| AnnotatedSchema ProtoBufAnnotation $(conT schemaName') |]
                               (return annTy))
#else
       annDec <- tySynInstD ''AnnotatedSchema
                   (tySynEqn [ [t| ProtoBufAnnotation |], conT schemaName' ] (return annTy))
#endif
       return [schemaDec, annDec]

schemaFromProtoBuf :: P.ProtoBuf -> Q (Type, Type)
schemaFromProtoBuf P.ProtoBuf {P.types = tys} = do
  let decls = flattenDecls tys
  (schTys, anns) <- unzip <$> mapM pbTypeDeclToType decls
  return (typesToList schTys, typesToList (concat anns))

flattenDecls :: [P.TypeDeclaration] -> [P.TypeDeclaration]
flattenDecls = concatMap flattenDecl
  where
    flattenDecl d@P.DEnum {} = [d]
    flattenDecl (P.DMessage name o r fs decls) =
      P.DMessage name o r fs [] : flattenDecls decls

pbTypeDeclToType :: P.TypeDeclaration -> Q (Type, [Type])
pbTypeDeclToType (P.DEnum name _ fields) = do
  (tys, anns) <- unzip <$> mapM pbChoiceToType fields
  (,) <$> [t|'DEnum $(textToStrLit name) $(return $ typesToList tys)|] <*> pure anns
  where
    pbChoiceToType :: P.EnumField -> Q (Type, Type)
    pbChoiceToType (P.EnumField nm number _)
      = (,) <$> [t|'ChoiceDef $(textToStrLit nm) |]
            <*> [t|'AnnField $(textToStrLit name) $(textToStrLit nm) ('ProtoBufId $(intToLit number)) |]
pbTypeDeclToType (P.DMessage name _ _ fields _) = do
  (tys, anns) <- unzip <$> mapM pbMsgFieldToType fields
  (,) <$> [t|'DRecord $(textToStrLit name) $(pure $ typesToList tys)|] <*> pure anns
  where
    pbMsgFieldToType :: P.MessageField -> Q (Type, Type)
    pbMsgFieldToType (P.NormalField P.Single ty nm n _)
      = (,) <$> [t| 'FieldDef $(textToStrLit nm) $(pbFieldTypeToType ty) |]
            <*> [t| 'AnnField $(textToStrLit name) $(textToStrLit nm) ('ProtoBufId $(intToLit n)) |]
    pbMsgFieldToType (P.NormalField P.Repeated ty nm n _)
      = (,) <$> [t| 'FieldDef $(textToStrLit nm) ('TList $(pbFieldTypeToType ty)) |]
            <*> [t| 'AnnField $(textToStrLit name) $(textToStrLit nm) ('ProtoBufId $(intToLit n)) |]
    pbMsgFieldToType (P.MapField k v nm n _)
      = (,) <$> [t| 'FieldDef $(textToStrLit nm) ('TMap $(pbFieldTypeToType k) $(pbFieldTypeToType v)) |]
            <*> [t| 'AnnField $(textToStrLit name) $(textToStrLit nm) ('ProtoBufId $(intToLit n)) |]
    pbMsgFieldToType (P.OneOfField nm vs)
      | any (not . hasFieldNumber) vs
      = fail "nested oneof fields are not supported"
      | otherwise
      = (,) <$> [t| 'FieldDef $(textToStrLit nm) $(typesToList <$> mapM pbOneOfFieldToType vs ) |]
            <*> [t| 'AnnField $(textToStrLit name) $(textToStrLit nm)
                       ('ProtoBufOneOfIds $(typesToList <$> mapM (intToLit . getFieldNumber) vs )) |]

    pbFieldTypeToType :: P.FieldType -> Q Type
    pbFieldTypeToType P.TInt32     = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TUInt32    = fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt32    = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TInt64     = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TUInt64    = fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt64    = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TFixed32   = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TFixed64   = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed32  = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed64  = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TDouble    = [t|'TPrimitive Double|]
    pbFieldTypeToType P.TBool      = [t|'TPrimitive Bool|]
    pbFieldTypeToType P.TString    = [t|'TPrimitive T.Text|]
    pbFieldTypeToType P.TBytes     = [t|'TPrimitive B.ByteString|]
    pbFieldTypeToType (P.TOther t) = [t|'TSchematic $(textToStrLit (last t))|]

    hasFieldNumber P.NormalField {} = True
    hasFieldNumber P.MapField {}    = True
    hasFieldNumber _                = False

    getFieldNumber (P.NormalField _ _ _ n _) = n
    getFieldNumber (P.MapField    _ _ _ n _) = n
    getFieldNumber _                         = error "this should never happen"

    pbOneOfFieldToType (P.NormalField P.Single ty _ _ _)
      = pbFieldTypeToType ty
    pbOneOfFieldToType (P.NormalField P.Repeated ty _ _ _)
      = [t| 'TList $(pbFieldTypeToType ty) |]
    pbOneOfFieldToType (P.MapField k v _ _ _)
      = [t| 'TMap $(pbFieldTypeToType k) $(pbFieldTypeToType v) |]
    pbOneOfFieldToType _ = error "this should never happen"

typesToList :: [Type] -> Type
typesToList = foldr (\y ys -> AppT (AppT PromotedConsT y) ys) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit s = return $ LitT $ StrTyLit $ T.unpack s

intToLit :: Int -> Q Type
intToLit n = return $ LitT $ NumTyLit $ toInteger n
