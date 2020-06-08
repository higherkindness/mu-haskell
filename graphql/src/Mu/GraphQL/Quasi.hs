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

import           Control.Monad.IO.Class        (liftIO)
import           Data.Coerce                   (coerce)
import qualified Data.HashMap.Strict           as HM
import           Data.List                     (foldl')
import           Data.Maybe                    (catMaybes)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.UUID                     (UUID)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Quasi.LostParser   (parseTypeSysDefinition)
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

classifySchema :: [GQL.SchemaDefinition] -> SchemaMap
classifySchema = foldl' schemaToMap HM.empty
  where
    schemaToMap :: SchemaMap -> GQL.SchemaDefinition -> SchemaMap
    schemaToMap mp (GQL.SchemaDefinition _ ops) = foldl' operationToKeyValue mp ops
    operationToKeyValue :: SchemaMap -> GQL.RootOperationTypeDefinition -> SchemaMap
    operationToKeyValue mp (GQL.RootOperationTypeDefinition opType (coerce -> name)) = HM.insert name opType mp

classify :: [GQL.TypeDefinition] -> TypeMap
classify = HM.fromList . (typeToKeyValue <$>)
  where
    typeToKeyValue :: GQL.TypeDefinition -> (T.Text, GQLType)
    typeToKeyValue (GQL.TypeDefinitionScalar (GQL.ScalarTypeDefinition _ name _))
      = (coerce name, Scalar)
    typeToKeyValue (GQL.TypeDefinitionObject (GQL.ObjectTypeDefinition _ name _ _ _))
      = (coerce name, Object)
    typeToKeyValue (GQL.TypeDefinitionInterface (GQL.InterfaceTypeDefinition _ name _ _))
      = (coerce name, Other)
    typeToKeyValue (GQL.TypeDefinitionUnion (GQL.UnionTypeDefinition _ name _ _))
      = (coerce name, Other)
    typeToKeyValue (GQL.TypeDefinitionEnum (GQL.EnumTypeDefinition _ name _ _))
      = (coerce name, Enum)
    typeToKeyValue (GQL.TypeDefinitionInputObject (GQL.InputObjectTypeDefinition _ name _ _))
      = (coerce name, InputObject)

-- | Constructs the GraphQL tree splitting between Schemas and Services.
graphqlToDecls :: String -> String -> [GQL.TypeSystemDefinition] -> Q [Dec]
graphqlToDecls schemaName serviceName allTypes = do
  let schemaName'  = mkName schemaName
      serviceName' = mkName serviceName
      types        = [t | GQL.TypeSystemDefinitionType t <- allTypes]
      schTypes     = [t | GQL.TypeSystemDefinitionSchema t <- allTypes]
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
typeToDec _ _ _ (GQL.TypeDefinitionInterface _)
  = fail "interface types are not supported"
typeToDec _ _ _ (GQL.TypeDefinitionUnion _)
  = fail "union types are not supported"
typeToDec schemaName tm _ (GQL.TypeDefinitionScalar (GQL.ScalarTypeDefinition _ s _)) =
  GQLScalar <$ gqlTypeToType s tm schemaName
typeToDec schemaName tm sm (GQL.TypeDefinitionObject objs) = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Result
    objToDec (GQL.ObjectTypeDefinition _ (coerce -> nm) _ _ flds) = do
      (fieldInfos, defaults) <- unzip <$> traverse (gqlFieldToType nm) flds
      GQLService <$> [t| 'Service $(textToStrLit nm)
                                  $(pure $ typesToList fieldInfos) |]
                 <*> pure ((nm,) <$> concat defaults)
    gqlFieldToType :: T.Text -> GQL.FieldDefinition
                   -> Q (Type, [(T.Text, (T.Text, Type))])
    gqlFieldToType sn (GQL.FieldDefinition _ (coerce -> fnm) args ftyp _) = do
      (argInfos, defaults) <- unzip <$> traverse argToType args
      (,) <$> [t| 'Method $(textToStrLit fnm)
                          $(pure $ typesToList argInfos)
                          $(returnType sn ftyp) |]
          <*> pure ((fnm,) <$> catMaybes defaults)
    returnType :: T.Text -> GQL.GType -> Q Type
    returnType serviceName typ =
      case HM.lookup serviceName sm of
        Just GQL.OperationTypeSubscription -> [t|'RetStream $(retToType typ)|]
        _                                  -> [t|'RetSingle $(retToType typ)|]
    argToType :: GQL.InputValueDefinition -> Q (Type, Maybe (T.Text, Type))
    argToType (GQL.InputValueDefinition _ (coerce -> aname) atype Nothing) =
      (, Nothing) <$> [t| 'ArgSingle ('Just $(textToStrLit aname)) $(retToType atype) |]
    argToType (GQL.InputValueDefinition _ (coerce -> aname) atype (Just defs)) =
      (,) <$> [t| 'ArgSingle ('Just $(textToStrLit aname)) $(retToType atype) |]
          <*> (Just . (aname,) <$> [t| 'DefaultValue $( defToVConst defs ) |])
    defToVConst :: GQL.DefaultValue -> Q Type
    defToVConst (GQL.VCBoolean _) = [t| 'VCBoolean|]
    defToVConst GQL.VCNull        = [t| 'VCNull |]
    defToVConst (GQL.VCInt _)     = [t| 'VCInt |]
    defToVConst (GQL.VCFloat _)
      = fail "floats as default arguments are not supported"
    defToVConst (GQL.VCString (coerce -> s))
      = [t| 'VCString $(textToStrLit s) |]
    defToVConst (GQL.VCEnum (coerce -> e))
      = [t| 'VCEnum $(textToStrLit e) |]
    defToVConst (GQL.VCList (GQL.ListValueG xs))
      = [t| 'VCList $(typesToList <$> traverse defToVConst xs) |]
    defToVConst (GQL.VCObject (GQL.ObjectValueG obj))
      = [t| 'VCObject $(typesToList <$> traverse fromGQLField obj) |]
    fromGQLField :: GQL.ObjectFieldG GQL.ValueConst -> Q Type
    fromGQLField (GQL.ObjectFieldG (coerce -> n) v)   = [t| ($(textToStrLit n), $(defToVConst v)) |]
    retToType :: GQL.GType -> Q Type
    retToType (GQL.TypeNamed (coerce -> False) (coerce -> a)) =
      [t| $(gqlTypeToType a tm schemaName) |]
    retToType (GQL.TypeNamed (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef $(gqlTypeToType a tm schemaName) |]
    retToType (GQL.TypeList (coerce -> False) (coerce -> a)) =
      [t| 'ListRef $(retToType a) |]
    retToType (GQL.TypeList (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef ('ListRef $(retToType a)) |]
    retToType _ = fail "this should not happen, please, file an issue"
typeToDec _ _ _ (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Result
    enumToDecl (GQL.EnumTypeDefinition _ (coerce -> name) _ symbols) =
      GQLSchema <$> [t|'DEnum $(textToStrLit name)
                              $(typesToList <$> traverse gqlChoiceToType symbols)|]
    gqlChoiceToType :: GQL.EnumValueDefinition -> Q Type
    gqlChoiceToType (GQL.EnumValueDefinition _ (coerce -> c) _) =
      [t|'ChoiceDef $(textToStrLit c)|]
typeToDec _ _ _ (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
  where
    inputObjToDec :: GQL.InputObjectTypeDefinition -> Q Result
    inputObjToDec (GQL.InputObjectTypeDefinition _ (coerce -> name) _ fields) =
        GQLSchema <$> [t|'DRecord $(textToStrLit name)
                                  $(typesToList <$> traverse gqlFieldToType fields)|]
    gqlFieldToType :: GQL.InputValueDefinition -> Q Type
    gqlFieldToType (GQL.InputValueDefinition _ (coerce -> fname) ftype _) =
      [t|'FieldDef $(textToStrLit fname) $(ginputTypeToType ftype)|]
    ginputTypeToType :: GQL.GType -> Q Type
    ginputTypeToType (GQL.TypeNamed (coerce -> False) (coerce -> a)) =
      [t| $(typeToPrimType a) |]
    ginputTypeToType (GQL.TypeNamed (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef $(typeToPrimType a) |]
    ginputTypeToType (GQL.TypeList (coerce -> False) (coerce -> a)) =
      [t| 'ListRef $(ginputTypeToType a) |]
    ginputTypeToType (GQL.TypeList (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef ('ListRef $(ginputTypeToType a)) |]
    ginputTypeToType _ = fail "this should not happen, please, file an issue"
    typeToPrimType :: GQL.Name -> Q Type
    typeToPrimType (GQL.unName -> "Int")     = [t|'TPrimitive Integer|]
    typeToPrimType (GQL.unName -> "Float")   = [t|'TPrimitive Double|]
    typeToPrimType (GQL.unName -> "String")  = [t|'TPrimitive T.Text|]
    typeToPrimType (GQL.unName -> "Boolean") = [t|'TPrimitive Bool|]
    typeToPrimType (GQL.unName -> "ID")      = [t|'TPrimitive UUID|]
    typeToPrimType (coerce -> name)          = [t|'TSchematic $(textToStrLit name)|]

gqlTypeToType :: GQL.Name -> TypeMap -> Name -> Q Type
gqlTypeToType (GQL.unName -> "Int") _  _     = [t|'PrimitiveRef Integer|]
gqlTypeToType (GQL.unName -> "Float") _ _    = [t|'PrimitiveRef Double|]
gqlTypeToType (GQL.unName -> "String") _ _   = [t|'PrimitiveRef T.Text|]
gqlTypeToType (GQL.unName -> "Boolean") _ _  = [t|'PrimitiveRef Bool|]
gqlTypeToType (GQL.unName -> "ID") _ _       = [t|'PrimitiveRef UUID|]
gqlTypeToType (coerce -> name) tm schemaName =
  let schemaRef = [t|'SchemaRef $(conT schemaName) $(textToStrLit name)|]
   in case HM.lookup name tm of
        Just Enum        -> schemaRef
        Just InputObject -> schemaRef
        _                -> [t|'ObjectRef $(textToStrLit name)|]

typesToList :: [Type] -> Type
typesToList = foldr (AppT . AppT PromotedConsT) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit = litT . strTyLit . T.unpack
