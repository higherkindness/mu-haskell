{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Mu.Schema.Quasi
  -- * Quasi-quoters for @.avsc@ files
  ( avro
  , avroFile
  -- * Quasi-quoters for @.proto@ files
  , protobuf
  , protobufFile
  -- * Only for internal use
  , schemaFromAvroType
  , schemaFromProtoBuf
  ) where

import           Data.Aeson                      (decode, encode)
import qualified Data.Avro.Schema                as A
import qualified Data.ByteString                 as B
import           Data.Int
import qualified Data.Text                       as T
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
  case decode (encode s) of
    Nothing -> fail "could not parse avro spec!"
    Just p  -> schemaFromAvroType p

schemaFromAvroType :: A.Type -> Q Type
schemaFromAvroType = undefined -- TODO: here goes the magic

schemaFromProtoBufString :: String -> Q Type
schemaFromProtoBufString ts =
  case parseProtoBuf (T.pack ts) of
    Left e  -> fail ("could not parse protocol buffers spec: " ++ show e)
    Right p -> schemaFromProtoBuf p

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
  [t|'DEnum $(textToStrLit name) '[] $(typesToList <$>
                                       mapM pbChoiceToType fields)|]
  where
    pbChoiceToType :: P.EnumField -> Q Type
    pbChoiceToType (P.EnumField nm number _) =
      [t|'ChoiceDef $(textToStrLit nm) '[ ProtoBufId $(intToLit number)]|]
pbTypeDeclToType (P.DMessage name _ _ fields _) =
  [t|'DRecord $(textToStrLit name) '[] $(typesToList <$>
                                         mapM pbMsgFieldToType fields)|]
  where
    pbMsgFieldToType :: P.MessageField -> Q Type
    pbMsgFieldToType (P.NormalField P.Single ty nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] $(pbFieldTypeToType
                                                                       ty)|]
    pbMsgFieldToType (P.NormalField P.Repeated ty nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] ('TList $(pbFieldTypeToType
                                                                               ty))|]
    pbMsgFieldToType (P.MapField k v nm n _) =
      [t|'FieldDef $(textToStrLit nm) '[ ProtoBufId $(intToLit n)] ('TMap $(pbFieldTypeToType
                                                                              k) $(pbFieldTypeToType
                                                                                     v))|]
    pbMsgFieldToType P.OneOfField {} =
      fail "oneof fields are not currently supported"
    pbFieldTypeToType :: P.FieldType -> Q Type
    pbFieldTypeToType P.TInt32 = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TUInt32 =
      fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt32 = [t|'TPrimitive Int32|]
    pbFieldTypeToType P.TInt64 = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TUInt64 =
      fail "unsigned integers are not currently supported"
    pbFieldTypeToType P.TSInt64 = [t|'TPrimitive Int64|]
    pbFieldTypeToType P.TFixed32 =
      fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TFixed64 =
      fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed32 =
      fail "fixed integers are not currently supported"
    pbFieldTypeToType P.TSFixed64 =
      fail "fixed integers are not currently supported"
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
