{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-# language ViewPatterns      #-}

module Mu.GraphQL.Quasi where

import           Control.Monad.IO.Class        (liftIO)
import           Data.Int                      (Int32)
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

data Result =
    GQLSchema Type
  | GQLService Type
  deriving (Show, Eq)

-- | Constructs the GraphQL tree splitting between Schemas and Services.
graphqlToDecls :: String -> String -> GQL.SchemaDocument -> Q [Dec]
graphqlToDecls schemaName serviceName (GQL.SchemaDocument types) = do
  let schemaName'  = mkName schemaName
      serviceName' = mkName serviceName
  rs <- traverse (typeToDec schemaName') types
  let schemaTypes  = [x | GQLSchema  x <- rs]
      serviceTypes = [x | GQLService x <- rs]
  schemaDec <- tySynD schemaName' [] (pure $ typesToList schemaTypes)
  serviceDec <- tySynD serviceName' [] (pure $ typesToList serviceTypes)
  pure [schemaDec, serviceDec]

-- | Reads a GraphQL 'TypeDefinition' and returns a 'Result'.
typeToDec :: Name -> GQL.TypeDefinition -> Q Result
typeToDec _ (GQL.TypeDefinitionScalar (GQL.ScalarTypeDefinition _ s _)) = GQLSchema <$> scalarToType s
  where
    scalarToType :: GQL.Name -> Q Type
    scalarToType (GQL.unName -> "Int")     = [t|'TPrimitive Int32|]
    scalarToType (GQL.unName -> "Float")   = [t|'TPrimitive Double|]
    scalarToType (GQL.unName -> "String")  = [t|'TPrimitive T.Text|]
    scalarToType (GQL.unName -> "Boolean") = [t|'TPrimitive Bool|]
    scalarToType (GQL.unName -> "ID")      = [t|'TPrimitive UUID|]
    scalarToType _                         = fail "not well-known scalar types are not supported"
typeToDec _ (GQL.TypeDefinitionObject objs) = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Result
    objToDec (GQL.ObjectTypeDefinition _ nm _ _ flds) =
      GQLService <$> [t| 'Service $(textToStrLit $ GQL.unName nm) '[]
          $(typesToList <$> traverse gqlFieldToType flds) |]
    gqlFieldToType :: GQL.FieldDefinition -> Q Type
    gqlFieldToType (GQL.FieldDefinition _ (GQL.unName -> fnm) args ftyp _) =
      [t| 'Method $(textToStrLit fnm) '[]
            $(typesToList <$> traverse argToType args)
            'RetSingle $(gtypeToType ftyp) |]
    argToType :: GQL.InputValueDefinition -> Q Type
    argToType (GQL.InputValueDefinition _ (GQL.unName -> aname) atype Nothing) =
      [t| 'ArgSingle ('Just $(textToStrLit aname)) '[] $(gtypeToType atype) |]
    argToType (GQL.InputValueDefinition _ (GQL.unName -> aname) atype (Just defs)) =
      [t| 'ArgSingle ('Just $(textToStrLit aname)) '[] {-DefaultValue $(fromGQLValueConst defs)]-} $(gtypeToType atype) |]
    gtypeToType :: GQL.GType -> Q Type
    gtypeToType (GQL.TypeNamed (GQL.unNullability -> False) (GQL.unName . GQL.unNamedType -> a)) =
      [t| 'ObjectRef $(textToStrLit a) |]
    gtypeToType (GQL.TypeNamed (GQL.unNullability -> True) (GQL.unName . GQL.unNamedType -> a)) =
      [t| 'OptionalRef ('ObjectRef $(textToStrLit a)) |]
    gtypeToType (GQL.TypeList (GQL.unNullability -> False) (GQL.unListType -> a)) =
      [t| 'ListRef $(gtypeToType a) |]
    gtypeToType (GQL.TypeList (GQL.unNullability -> True) (GQL.unListType -> a)) =
      [t| 'OptionalRef ('ListRef $(gtypeToType a)) |]
    gtypeToType _ = fail "this should not happen, please, file an issue"
typeToDec _ (GQL.TypeDefinitionInterface _)       = fail "interface types are not supported"
typeToDec _ (GQL.TypeDefinitionUnion _)           = fail "union types are not supported"
typeToDec _ (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Result
    enumToDecl (GQL.EnumTypeDefinition _ (GQL.unName -> name) _ symbols) =
      GQLSchema <$> [t|'DEnum $(textToStrLit name)
                              $(typesToList <$> traverse gqlChoiceToType symbols)|]
    gqlChoiceToType :: GQL.EnumValueDefinition -> Q Type
    gqlChoiceToType (GQL.EnumValueDefinition _ (GQL.unName . GQL.unEnumValue -> c) _) =
      [t|'ChoiceDef $(textToStrLit c)|]
typeToDec _ (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
  where
    inputObjToDec :: GQL.InputObjectTypeDefinition -> Q Result
    inputObjToDec (GQL.InputObjectTypeDefinition _ (GQL.unName -> name) _ fields) =
        GQLSchema <$> [t|'DRecord $(textToStrLit name)
                                  $(typesToList <$> traverse gqlFieldToType fields)|]
    gqlFieldToType :: GQL.InputValueDefinition -> Q Type
    gqlFieldToType (GQL.InputValueDefinition _ (GQL.unName -> fname) ftype _) =
      [t|'FieldDef $(textToStrLit fname) $(ginputTypeToType ftype)|]
    ginputTypeToType :: GQL.GType -> Q Type
    ginputTypeToType (GQL.TypeNamed (GQL.unNullability -> False) (GQL.unName . GQL.unNamedType -> a)) =
      [t| 'ObjectRef $(textToStrLit a) |]
    ginputTypeToType (GQL.TypeNamed (GQL.unNullability -> True) (GQL.unName . GQL.unNamedType -> a)) =
      [t| 'OptionalRef ('ObjectRef $(textToStrLit a)) |]
    ginputTypeToType (GQL.TypeList (GQL.unNullability -> False) (GQL.unListType -> a)) =
      [t| 'ListRef $(ginputTypeToType a) |]
    ginputTypeToType (GQL.TypeList (GQL.unNullability -> True) (GQL.unListType -> a)) =
      [t| 'OptionalRef ('ListRef $(ginputTypeToType a)) |]
    ginputTypeToType _ = fail "this should not happen, please, file an issue"

typesToList :: [Type] -> Type
typesToList = foldr (AppT . AppT PromotedConsT) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit = litT . strTyLit . T.unpack
