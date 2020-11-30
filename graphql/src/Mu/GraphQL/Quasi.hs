{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-# language TupleSections     #-}
{-# language ViewPatterns      #-}
{-|
Description : Quasi-quoters for GraphQL schemas

Read @.graphql@ files as a 'Mu.Schema.Definition.Schema'
and 'Package' with one 'Service' per object in the schema.
-}
module Mu.GraphQL.Quasi (
  graphql
, graphql'
) where

import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Aeson                  as JSON
import           Data.Foldable               (toList)
import qualified Data.HashMap.Strict         as HM
import           Data.List                   (foldl')
import           Data.Maybe                  (catMaybes)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.UUID                   (UUID)
import qualified Language.GraphQL.AST        as GQL
import           Language.Haskell.TH

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Quasi.LostParser (parseTypeSysDefinition)
import           Mu.Rpc
import           Mu.Schema.Definition

-- | Imports an GraphQL schema definition from a file.
graphql :: String   -- ^ Name for the 'Package' type, the 'Schema' is derived from it
        -> FilePath -- ^ Route to the file
        -> Q [Dec]
graphql name = graphql' (name <> "Schema") name

-- | Imports an GraphQL schema definition from a file.
graphql' :: String   -- ^ Name for the 'Schema' type
         -> String   -- ^ Name for the 'Package' type
         -> FilePath -- ^ Route to the file
         -> Q [Dec]
graphql' scName svName file = do
  schema <- liftIO $ TIO.readFile file
  case parseTypeSysDefinition schema of
    Left e  -> fail ("could not parse graphql spec: " ++ show e)
    Right p -> graphqlToDecls scName svName p

type TypeMap   = HM.HashMap T.Text GQLType
type SchemaMap = HM.HashMap T.Text GQL.OperationType

data Result =
    GQLScalar
  | GQLSchema  Type
  | GQLService Type [(T.Text, (T.Text, (T.Text, Type)))]

data GQLType =
    Enum
  | Object
  | Scalar
  | InputObject
  | Other

classifySchema :: [GQL.TypeSystemDefinition] -> SchemaMap
classifySchema = foldl' schemaToMap HM.empty
  where
    schemaToMap :: SchemaMap -> GQL.TypeSystemDefinition -> SchemaMap
    schemaToMap mp (GQL.SchemaDefinition _ (toList -> ops)) = foldl' operationToKeyValue mp ops
    schemaToMap _ _ = error "this should have been taken care by graphqlToDecls"
    operationToKeyValue :: SchemaMap -> GQL.OperationTypeDefinition -> SchemaMap
    operationToKeyValue mp (GQL.OperationTypeDefinition opType name) = HM.insert name opType mp

classify :: [GQL.TypeDefinition] -> TypeMap
classify = HM.fromList . (typeToKeyValue <$>)
  where
    typeToKeyValue :: GQL.TypeDefinition -> (T.Text, GQLType)
    typeToKeyValue (GQL.ScalarTypeDefinition _ name _)
      = (name, Scalar)
    typeToKeyValue (GQL.ObjectTypeDefinition _ name _ _ _)
      = (name, Object)
    typeToKeyValue (GQL.InterfaceTypeDefinition _ name _ _)
      = (name, Other)
    typeToKeyValue (GQL.UnionTypeDefinition _ name _ _)
      = (name, Other)
    typeToKeyValue (GQL.EnumTypeDefinition _ name _ _)
      = (name, Enum)
    typeToKeyValue (GQL.InputObjectTypeDefinition _ name _ _)
      = (name, InputObject)

-- | Constructs the GraphQL tree splitting between Schemas and Services.
graphqlToDecls :: String -> String -> [GQL.TypeSystemDefinition] -> Q [Dec]
graphqlToDecls schemaName serviceName allTypes = do
  let schemaName'  = mkName schemaName
      serviceName' = mkName serviceName
      types        = [t | GQL.TypeDefinition t <- allTypes]
      schTypes     = [t | t@GQL.SchemaDefinition {} <- allTypes]
      typeMap      = classify types
      schMap       = classifySchema schTypes
  rs <- traverse (typeToDec schemaName' typeMap schMap) types
  let schemaTypes  = [x | GQLSchema  x <- rs]
      serviceTypes = [x | GQLService x _ <- rs]
      defaultDefs  = concat [d | GQLService _ d <- rs]
  schemaDec <- tySynD schemaName' [] (pure $ typesToList schemaTypes)
  pkgTy <- [t| 'Package ('Just $(textToStrLit $ T.pack serviceName))
                        $(pure $ typesToList serviceTypes) |]
  serviceDec <- tySynD serviceName' [] (pure pkgTy)
  defaultDec <- [d| type instance AnnotatedPackage DefaultValue $(pure pkgTy) =
                      $(typesToList <$> traverse defaultDeclToTy defaultDefs) |]
  pure $ schemaDec : serviceDec : defaultDec

defaultDeclToTy :: (T.Text, (T.Text, (T.Text, Type))) -> Q Type
defaultDeclToTy (sn, (mn, (an, dv)))
  = [t| 'AnnArg $(textToStrLit sn) $(textToStrLit mn) $(textToStrLit an) $(pure dv) |]

-- | Reads a GraphQL 'TypeDefinition' and returns a 'Result'.
typeToDec :: Name -> TypeMap -> SchemaMap -> GQL.TypeDefinition -> Q Result
typeToDec _ _ _ GQL.InterfaceTypeDefinition {}
  = fail "interface types are not supported"
typeToDec _ _ _ GQL.UnionTypeDefinition {}
  = fail "union types are not supported"
typeToDec schemaName tm _ (GQL.ScalarTypeDefinition _ s _) =
  GQLScalar <$ gqlTypeToType s tm schemaName
typeToDec schemaName tm sm (GQL.ObjectTypeDefinition _ nm _ _ flds) = do
  (fieldInfos, defaults) <- unzip <$> traverse (gqlFieldToType nm) flds
  GQLService <$> [t| 'Service $(textToStrLit nm)
                              $(pure $ typesToList fieldInfos) |]
             <*> pure ((nm,) <$> concat defaults)
  where
    gqlFieldToType :: T.Text -> GQL.FieldDefinition
                   -> Q (Type, [(T.Text, (T.Text, Type))])
    gqlFieldToType sn (GQL.FieldDefinition _ fnm (GQL.ArgumentsDefinition args) ftyp _) = do
      (argInfos, defaults) <- unzip <$> traverse argToType args
      (,) <$> [t| 'Method $(textToStrLit fnm)
                          $(pure $ typesToList argInfos)
                          $(returnType sn ftyp) |]
          <*> pure ((fnm,) <$> catMaybes defaults)
    returnType :: T.Text -> GQL.Type -> Q Type
    returnType serviceName typ =
      case HM.lookup serviceName sm of
        Just GQL.Subscription -> [t|'RetStream $(retToType typ)|]
        _                     -> [t|'RetSingle $(retToType typ)|]
    argToType :: GQL.InputValueDefinition -> Q (Type, Maybe (T.Text, Type))
    argToType (GQL.InputValueDefinition _ aname atype Nothing _) =
      (, Nothing) <$> [t| 'ArgSingle ('Just $(textToStrLit aname)) $(retToType atype) |]
    argToType (GQL.InputValueDefinition _ aname atype (Just (GQL.Node defs _)) _) =
      (,) <$> [t| 'ArgSingle ('Just $(textToStrLit aname)) $(retToType atype) |]
          <*> (Just . (aname,) <$> [t| 'DefaultValue $( defToVConst defs ) |])
    defToVConst :: GQL.ConstValue -> Q Type
    defToVConst (GQL.ConstBoolean _) = [t| 'VCBoolean|]
    defToVConst GQL.ConstNull        = [t| 'VCNull |]
    defToVConst (GQL.ConstInt _)     = [t| 'VCInt |]
    defToVConst (GQL.ConstFloat _)
      = fail "floats as default arguments are not supported"
    defToVConst (GQL.ConstString s)
      = [t| 'VCString $(textToStrLit s) |]
    defToVConst (GQL.ConstEnum e)
      = [t| 'VCEnum $(textToStrLit e) |]
    defToVConst (GQL.ConstList xs)
      = [t| 'VCList $(typesToList <$> traverse defToVConst xs) |]
    defToVConst (GQL.ConstObject obj)
      = [t| 'VCObject $(typesToList <$> traverse fromGQLField obj) |]
    fromGQLField :: GQL.ObjectField GQL.ConstValue -> Q Type
    fromGQLField (GQL.ObjectField n (GQL.Node v _) _) = [t| ($(textToStrLit n), $(defToVConst v)) |]
    retToType :: GQL.Type -> Q Type
    retToType (GQL.TypeNonNull (GQL.NonNullTypeNamed a)) =
      [t| $(gqlTypeToType a tm schemaName) |]
    retToType (GQL.TypeNonNull (GQL.NonNullTypeList a)) =
      [t| 'ListRef $(retToType a) |]
    retToType (GQL.TypeNamed a) =
      [t| 'OptionalRef $(gqlTypeToType a tm schemaName) |]
    retToType (GQL.TypeList a) =
      [t| 'OptionalRef ('ListRef $(retToType a)) |]
typeToDec _ _ _ (GQL.EnumTypeDefinition _ name _ symbols) =
  GQLSchema <$> [t|'DEnum $(textToStrLit name)
                          $(typesToList <$> traverse gqlChoiceToType symbols)|]
  where
    gqlChoiceToType :: GQL.EnumValueDefinition -> Q Type
    gqlChoiceToType (GQL.EnumValueDefinition _ c _) =
      [t|'ChoiceDef $(textToStrLit c)|]
typeToDec _ _ _ (GQL.InputObjectTypeDefinition _ name _ fields) =
  GQLSchema <$> [t|'DRecord $(textToStrLit name)
                            $(typesToList <$> traverse gqlFieldToType fields)|]
  where
    gqlFieldToType :: GQL.InputValueDefinition -> Q Type
    gqlFieldToType (GQL.InputValueDefinition _ fname ftype _ _) =
      [t|'FieldDef $(textToStrLit fname) $(ginputTypeToType ftype)|]
    ginputTypeToType :: GQL.Type -> Q Type
    ginputTypeToType (GQL.TypeNonNull (GQL.NonNullTypeNamed a)) =
      [t| $(typeToPrimType a) |]
    ginputTypeToType (GQL.TypeNonNull (GQL.NonNullTypeList a)) =
      [t| 'ListRef $(ginputTypeToType a) |]
    ginputTypeToType (GQL.TypeNamed a) =
      [t| 'OptionalRef $(typeToPrimType a) |]
    ginputTypeToType (GQL.TypeList a) =
      [t| 'OptionalRef ('ListRef $(ginputTypeToType a)) |]
    typeToPrimType :: GQL.Name -> Q Type
    typeToPrimType "Int"        = [t|'TPrimitive Integer|]
    typeToPrimType "Float"      = [t|'TPrimitive Double|]
    typeToPrimType "String"     = [t|'TPrimitive T.Text|]
    typeToPrimType "Boolean"    = [t|'TPrimitive Bool|]
    typeToPrimType "ID"         = [t|'TPrimitive UUID|]
    typeToPrimType "JSON"       = [t|'TPrimitive JSON.Value|]
    typeToPrimType "JSONObject" = [t|'TPrimitive JSON.Object|]
    typeToPrimType nm           = [t|'TSchematic $(textToStrLit nm)|]

-- For the JSON scalar we follow
-- https://github.com/taion/graphql-type-json

gqlTypeToType :: GQL.Name -> TypeMap -> Name -> Q Type
gqlTypeToType "Int"        _ _ = [t|'PrimitiveRef Integer|]
gqlTypeToType "Float"      _ _ = [t|'PrimitiveRef Double|]
gqlTypeToType "String"     _ _ = [t|'PrimitiveRef T.Text|]
gqlTypeToType "Boolean"    _ _ = [t|'PrimitiveRef Bool|]
gqlTypeToType "ID"         _ _ = [t|'PrimitiveRef UUID|]
gqlTypeToType "JSON"       _ _ = [t|'PrimitiveRef JSON.Value|]
gqlTypeToType "JSONObject" _ _ = [t|'PrimitiveRef JSON.Object|]
gqlTypeToType name tm schemaName =
  let schemaRef = [t|'SchemaRef $(conT schemaName) $(textToStrLit name)|]
   in case HM.lookup name tm of
        Just Enum        -> schemaRef
        Just InputObject -> schemaRef
        _                -> [t|'ObjectRef $(textToStrLit name)|]

typesToList :: [Type] -> Type
typesToList = foldr (AppT . AppT PromotedConsT) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit = litT . strTyLit . T.unpack
