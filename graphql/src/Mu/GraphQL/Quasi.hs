{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-# language ViewPatterns      #-}

module Mu.GraphQL.Quasi (
  -- * 'Schema' generation from @.graphql@ files
  graphql
) where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Coerce                   (coerce)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.UUID                     (UUID)
import           Language.GraphQL.Draft.Parser (parseSchemaDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

import           Mu.GraphQL.Annotations
import           Mu.Rpc
import           Mu.Schema.Definition

-- | Imports an graphql definition written in-line as a 'Schema'.
graphql :: String -> String -> FilePath -> Q [Dec]
graphql scName svName file = do
  schema <- liftIO $ TIO.readFile file
  case parseSchemaDoc schema of
    Left e  -> fail ("could not parse graphql spec: " ++ show e)
    Right p -> graphqlToDecls scName svName p

type TypeMap = HM.HashMap T.Text GQLType

data Result =
    GQLScalar
  | GQLSchema Type
  | GQLService Type

data GQLType =
    Enum
  | Object
  | Scalar
  | InputObject
  | Other

classify :: [GQL.TypeDefinition] -> TypeMap
classify = HM.fromList . (typeToKeyValue <$>)
  where
    typeToKeyValue :: GQL.TypeDefinition -> (T.Text, GQLType)
    typeToKeyValue (GQL.TypeDefinitionScalar (GQL.ScalarTypeDefinition _ (coerce -> name) _))             = (name, Scalar)
    typeToKeyValue (GQL.TypeDefinitionObject (GQL.ObjectTypeDefinition _ (coerce -> name) _ _ _))         = (name, Object)
    typeToKeyValue (GQL.TypeDefinitionInterface (GQL.InterfaceTypeDefinition _ (coerce -> name) _ _))     = (name, Other)
    typeToKeyValue (GQL.TypeDefinitionUnion (GQL.UnionTypeDefinition _ (coerce -> name) _ _))             = (name, Other)
    typeToKeyValue (GQL.TypeDefinitionEnum (GQL.EnumTypeDefinition _ (coerce -> name) _ _))               = (name, Enum)
    typeToKeyValue (GQL.TypeDefinitionInputObject (GQL.InputObjectTypeDefinition _ (coerce -> name) _ _)) = (name, InputObject)

-- | Constructs the GraphQL tree splitting between Schemas and Services.
graphqlToDecls :: String -> String -> GQL.SchemaDocument -> Q [Dec]
graphqlToDecls schemaName serviceName (GQL.SchemaDocument types) = do
  let schemaName'  = mkName schemaName
      serviceName' = mkName serviceName
      typeMap      = classify types
  rs <- traverse (typeToDec schemaName' typeMap) types
  let schemaTypes  = [x | GQLSchema  x <- rs]
      serviceTypes = [x | GQLService x <- rs]
  schemaDec <- tySynD schemaName' [] (pure $ typesToList schemaTypes)
  serviceDec <- tySynD serviceName' []
    [t| 'Package ('Just $(textToStrLit $ T.pack serviceName))
                  $(pure $ typesToList serviceTypes) |]
  pure [schemaDec, serviceDec]

-- | Reads a GraphQL 'TypeDefinition' and returns a 'Result'.
typeToDec :: Name -> TypeMap -> GQL.TypeDefinition -> Q Result
typeToDec schemaName tm (GQL.TypeDefinitionScalar (GQL.ScalarTypeDefinition _ s _)) = GQLScalar <$ scalarToType s tm schemaName
typeToDec schemaName tm (GQL.TypeDefinitionObject objs) = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Result
    objToDec (GQL.ObjectTypeDefinition _ nm _ _ flds) =
      GQLService <$> [t| 'Service $(textToStrLit $ coerce nm) '[]
          $(typesToList <$> traverse gqlFieldToType flds) |]
    gqlFieldToType :: GQL.FieldDefinition -> Q Type
    gqlFieldToType (GQL.FieldDefinition _ (coerce -> fnm) args ftyp _) =
      [t| 'Method $(textToStrLit fnm) '[]
            $(typesToList <$> traverse argToType args)
            ('RetSingle $(gtypeToType ftyp)) |] -- TODO: `RetStream` if it's a subscription!
    argToType :: GQL.InputValueDefinition -> Q Type
    argToType (GQL.InputValueDefinition _ (coerce -> aname) atype Nothing) =
      [t| 'ArgSingle ('Just $(textToStrLit aname)) '[] $(gtypeToType atype) |]
    argToType (GQL.InputValueDefinition _ (coerce -> aname) atype (Just defs)) =
      [t| 'ArgSingle ('Just $(textToStrLit aname))
                      '[DefaultValue $( defToVConst defs )] $(gtypeToType atype) |]
    defToVConst :: GQL.DefaultValue -> Q Type
    defToVConst (GQL.VCInt _)                         = [t| 'VCInt |]
    defToVConst (GQL.VCFloat _)                       = fail "floats as default arguments are not supported"
    defToVConst (GQL.VCString (coerce -> s))          = [t| 'VCString $(textToStrLit s) |]
    defToVConst (GQL.VCBoolean _)                     = [t| 'VCBoolean|]
    defToVConst GQL.VCNull                            = [t| 'VCNull |]
    defToVConst (GQL.VCEnum (coerce -> e))            = [t| 'VCEnum $(textToStrLit e) |]
    defToVConst (GQL.VCList (GQL.ListValueG xs))      = [t| 'VCList $(typesToList <$> traverse defToVConst xs) |]
    defToVConst (GQL.VCObject (GQL.ObjectValueG obj)) = [t| 'VCObject $(typesToList <$> traverse fromGQLField obj) |]
    fromGQLField :: GQL.ObjectFieldG GQL.ValueConst -> Q Type
    fromGQLField (GQL.ObjectFieldG (coerce -> n) v)   = [t| ($(textToStrLit n), $(defToVConst v)) |]
    gtypeToType :: GQL.GType -> Q Type
    gtypeToType (GQL.TypeNamed (coerce -> False) (coerce -> a)) =
      [t| $(scalarToType a tm schemaName) |]
    gtypeToType (GQL.TypeNamed (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef $(scalarToType a tm schemaName) |]
    gtypeToType (GQL.TypeList (coerce -> False) (coerce -> a)) =
      [t| 'ListRef $(gtypeToType a) |]
    gtypeToType (GQL.TypeList (coerce -> True) (coerce -> a)) =
      [t| 'OptionalRef ('ListRef $(gtypeToType a)) |]
    gtypeToType _ = fail "this should not happen, please, file an issue"
typeToDec _ _ (GQL.TypeDefinitionInterface _)       = fail "interface types are not supported"
typeToDec _ _ (GQL.TypeDefinitionUnion _)           = fail "union types are not supported"
typeToDec _ _ (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Result
    enumToDecl (GQL.EnumTypeDefinition _ (coerce -> name) _ symbols) =
      GQLSchema <$> [t|'DEnum $(textToStrLit name)
                              $(typesToList <$> traverse gqlChoiceToType symbols)|]
    gqlChoiceToType :: GQL.EnumValueDefinition -> Q Type
    gqlChoiceToType (GQL.EnumValueDefinition _ (coerce -> c) _) =
      [t|'ChoiceDef $(textToStrLit c)|]
typeToDec _ _ (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
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

scalarToType :: GQL.Name -> TypeMap -> Name -> Q Type
scalarToType (GQL.unName -> "Int") _  _     = [t|'PrimitiveRef Integer|]
scalarToType (GQL.unName -> "Float") _ _    = [t|'PrimitiveRef Double|]
scalarToType (GQL.unName -> "String") _ _   = [t|'PrimitiveRef T.Text|]
scalarToType (GQL.unName -> "Boolean") _ _  = [t|'PrimitiveRef Bool|]
scalarToType (GQL.unName -> "ID") _ _       = [t|'PrimitiveRef UUID|]
scalarToType (coerce -> name) tm schemaName =
  let schemaRef = [t|'SchemaRef $(conT schemaName) $(textToStrLit name)|]
   in case HM.lookup name tm of
        Just Enum        -> schemaRef
        Just InputObject -> schemaRef
        _                -> [t|'ObjectRef $(textToStrLit name)|]

typesToList :: [Type] -> Type
typesToList = foldr (AppT . AppT PromotedConsT) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit = litT . strTyLit . T.unpack
