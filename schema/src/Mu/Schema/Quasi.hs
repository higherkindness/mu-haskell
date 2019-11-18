{-# language DataKinds       #-}
{-# language LambdaCase      #-}
{-# language NamedFieldPuns  #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns    #-}

module Mu.Schema.Quasi (
  -- * Quasi-quoters for @.avsc@ files
    avro
  , avroFile
  -- * Quasi-quoters for @.proto@ files
  , protobuf
  , protobufFile
  -- * Only for internal use
  , schemaFromAvroType
  , schemaFromProtoBuf
  ) where

import           Data.Aeson                      (decode)
import qualified Data.Avro.Schema                as A
import qualified Data.ByteString                 as B
import           Data.ByteString.Lazy.Char8      (pack)
import           Data.Int
import qualified Data.Text                       as T
import           Data.Vector                     (fromList, toList)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.ProtocolBuffers.Parser
import qualified Language.ProtocolBuffers.Types  as P

import           Mu.Schema.Adapter.ProtoBuf
import           Mu.Schema.Definition

-- | Imports an avro definition written in-line as a 'Schema'.
avro :: QuasiQuoter
avro =
  QuasiQuoter
    (const $ fail "cannot use as expression")
    (const $ fail "cannot use as pattern")
    schemaFromAvroString
    (const $ fail "cannot use as declaration")

-- | Imports a protocol buffer definition written
--   in-line as a 'Schema'.
protobuf :: QuasiQuoter
protobuf =
  QuasiQuoter
    (const $ fail "cannot use as expression")
    (const $ fail "cannot use as pattern")
    schemaFromProtoBufString
    (const $ fail "cannot use as declaration")

-- | Imports an avro definition from a file as a 'Schema'.
avroFile :: QuasiQuoter
avroFile = quoteFile avro

-- | Imports a protocol buffer definition from a file
--   as a 'Schema'.
protobufFile :: QuasiQuoter
protobufFile = quoteFile protobuf

schemaFromAvroString :: String -> Q Type
schemaFromAvroString s =
  case decode (pack s) of
    Nothing           -> fail "could not parse avro spec!"
    Just (A.Union us) -> schemaFromAvro (toList us)
    Just t            -> schemaFromAvro [t]
  where schemaFromAvro = (typesToList <$>) . mapM schemaDecFromAvroType . flattenAvroDecls

schemaDecFromAvroType :: A.Type -> Q Type
schemaDecFromAvroType (A.Record name _ _ _ fields) =
  [t|'DRecord $(textToStrLit $ A.baseName name) '[] $(typesToList <$> mapM avroFieldToType fields)|]
  where 
    avroFieldToType :: A.Field -> Q Type
    avroFieldToType field =
      [t|'FieldDef $(textToStrLit $ A.fldName field) '[] $(schemaFromAvroType $ A.fldType field)|]
schemaDecFromAvroType (A.Enum name _ _ symbols) =
  [t|'DEnum $(textToStrLit $ A.baseName name) '[] $(typesToList <$> mapM avChoiceToType (toList symbols))|]
  where
    avChoiceToType :: T.Text -> Q Type
    avChoiceToType c = [t|'ChoiceDef $(textToStrLit c) '[]|]
schemaDecFromAvroType t = [t| 'DSimple $(schemaFromAvroType t) |]

schemaFromAvroType :: A.Type -> Q Type
schemaFromAvroType = \case
  A.Null -> [t|'TPrimitive 'TNull|]
  A.Boolean -> [t|'TPrimitive Bool|]
  A.Int -> [t|'TPrimitive Int32|]
  A.Long -> [t|'TPrimitive Int64|]
  A.Float -> [t|'TPrimitive Float|]
  A.Double -> [t|'TPrimitive Double|]
  A.Bytes -> [t|'TPrimitive B.ByteString|]
  A.String -> [t|'TPrimitive T.Text|]
  A.Array item -> [t|'TList $(schemaFromAvroType item)|]
  A.Map values -> [t|'TMap T.Text $(schemaFromAvroType values)|]
  A.NamedType typeName ->
    [t|'TSchematic $(textToStrLit (A.baseName typeName))|]
  A.Enum {} -> fail "should never happen, please, file an issue"
  A.Record {} -> fail "should never happen, please, file an issue"
  A.Union options ->
    case toList options of
      [A.Null, x] -> toOption x
      [x, A.Null] -> toOption x
      _ -> [t|'TUnion $(typesToList <$> mapM schemaFromAvroType (toList options))|]
    where toOption x = [t|'TOption $(schemaFromAvroType x)|]
  A.Fixed {} -> fail "fixed integers are not currently supported"

schemaFromProtoBufString :: String -> Q Type
schemaFromProtoBufString ts =
  case parseProtoBuf (T.pack ts) of
    Left e  -> fail ("could not parse protocol buffers spec: " ++ show e)
    Right p -> schemaFromProtoBuf p

flattenAvroDecls :: [A.Type] -> [A.Type]
flattenAvroDecls = concatMap (uncurry (:) . flattenDecl)
  where
    flattenDecl :: A.Type -> (A.Type, [A.Type])
    flattenDecl (A.Record name a d o fields) = 
      let (flds, tts) = unzip (flattenAvroField <$> fields)
      in (A.Record name a d o flds, concat tts)
    flattenDecl (A.Union _) = error "should never happen, please, file an issue" 
    flattenDecl t = (t, [])

    flattenAvroType :: A.Type -> (A.Type, [A.Type])
    flattenAvroType (A.Record name a d o fields) = 
      let (flds, tts) = unzip (flattenAvroField <$> fields)
      in (A.NamedType name, A.Record name a d o flds : concat tts)
    flattenAvroType (A.Union (toList -> ts)) = 
      let (us, tts) = unzip (map flattenAvroType ts)
      in (A.Union $ fromList us, concat tts)
    flattenAvroType e@A.Enum {A.name} = (A.NamedType name, [e])
    flattenAvroType t = (t, [])

    flattenAvroField :: A.Field -> (A.Field, [A.Type])
    flattenAvroField f =
      let (t, decs) = flattenAvroType (A.fldType f)
       in (f {A.fldType = t}, decs)

schemaFromProtoBuf :: P.ProtoBuf -> Q Type
schemaFromProtoBuf P.ProtoBuf {P.types = tys} =
  let decls = flattenDecls tys
   in typesToList <$> mapM pbTypeDeclToType decls

flattenDecls :: [P.TypeDeclaration] -> [P.TypeDeclaration]
flattenDecls = concatMap flattenDecl
  where
    flattenDecl d@P.DEnum {} = [d]
    flattenDecl (P.DMessage name o r fs decls) =
      P.DMessage name o r fs [] : flattenDecls decls

pbTypeDeclToType :: P.TypeDeclaration -> Q Type
pbTypeDeclToType (P.DEnum name _ fields) =
  [t|'DEnum $(textToStrLit name) '[] $(typesToList <$> mapM pbChoiceToType fields)|]
  where
    pbChoiceToType :: P.EnumField -> Q Type
    pbChoiceToType (P.EnumField nm number _) =
      [t|'ChoiceDef $(textToStrLit nm) '[ ProtoBufId $(intToLit number)]|]
pbTypeDeclToType (P.DMessage name _ _ fields _) =
  [t|'DRecord $(textToStrLit name) '[] $(typesToList <$> mapM pbMsgFieldToType fields)|]
  where
    pbMsgFieldToType :: P.MessageField -> Q Type
    pbMsgFieldToType (P.NormalField P.Single ty nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] $(pbFieldTypeToType ty)|]
    pbMsgFieldToType (P.NormalField P.Repeated ty nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] ('TList $(pbFieldTypeToType ty))|]
    pbMsgFieldToType (P.MapField k v nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] ('TMap $(pbFieldTypeToType k) $(pbFieldTypeToType v))|]
    pbMsgFieldToType P.OneOfField {} = fail "oneof fields are not currently supported"
    pbFieldTypeToType :: P.FieldType -> Q Type
    pbFieldTypeToType P.TInt32 = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TUInt32 = fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt32 = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TInt64 = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TUInt64 = fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt64 = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TFixed32 = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TFixed64 = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed32 = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed64 = fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TDouble = [t|'TPrimitive Double|]
    pbFieldTypeToType P.TBool = [t|'TPrimitive Bool|]
    pbFieldTypeToType P.TString = [t|'TPrimitive T.Text|]
    pbFieldTypeToType P.TBytes = [t|'TPrimitive B.ByteString|]
    pbFieldTypeToType (P.TOther t) = [t|'TSchematic $(textToStrLit (last t))|]

typesToList :: [Type] -> Type
typesToList = foldr (\y ys -> AppT (AppT PromotedConsT y) ys) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit s = return $ LitT $ StrTyLit $ T.unpack s

intToLit :: Int -> Q Type
intToLit n = return $ LitT $ NumTyLit $ toInteger n
