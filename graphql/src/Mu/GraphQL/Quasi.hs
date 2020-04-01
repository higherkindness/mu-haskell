{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}

module Mu.GraphQL.Quasi where

import           Data.Int
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (parseSchemaDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

import           Mu.Rpc
import           Mu.Schema.Definition

-- | Imports an graphql definition written in-line as a 'Schema'.
graphql :: String -> String -> T.Text -> Q [Dec]
graphql scName svName schema =
  case parseSchemaDoc schema of
    Left e  -> fail ("could not parse graphql spec: " ++ show e)
    Right p -> graphqlToDecls scName svName p

data Result =
    GQLSchema Type
  | GQLService Type
  deriving (Show, Eq)

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

typeToDec :: Name -> GQL.TypeDefinition -> Q Result
typeToDec _ (GQL.TypeDefinitionScalar _)          = fail "scalar types are not supported"
typeToDec schemaName (GQL.TypeDefinitionObject objs)       = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Result
    objToDec (GQL.ObjectTypeDefinition _ nm _ _ flds) =
      GQLService <$> [t| 'Service $(textToStrLit $ GQL.unName nm) '[]
          $(typesToList <$> traverse gqlFieldToType flds) |]
    gqlFieldToType :: GQL.FieldDefinition -> Q Type
    gqlFieldToType (GQL.FieldDefinition _ fnm args ftyp _) =
      [t| 'Method $(textToStrLit $ GQL.unName fnm) '[]
        $(typesToList <$> traverse argToType args)
        $(retToType ftyp) |]
    argToType :: GQL.InputValueDefinition -> Q Type
    argToType (GQL.InputValueDefinition _ aname _ _) =
      [t| 'ArgSingle 'Nothing '[] ('SchemaRef $(conT schemaName) $(textToStrLit (GQL.unName aname))) |]
    retToType :: GQL.GType -> Q Type
    retToType (GQL.TypeNamed _ a) =
      [t| 'RetSingle ('SchemaRef $(conT schemaName) $(textToStrLit . GQL.unName . GQL.unNamedType $ a)) |]
    retToType _ = fail "only named types may be used as results"
typeToDec _ (GQL.TypeDefinitionInterface _)       = fail "interface types are not supported"
typeToDec _ (GQL.TypeDefinitionUnion _)           = fail "union types are not supported"
typeToDec _ (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Result
    enumToDecl = error "not implemented" -- TODO: this will return a `GQLSchema`
typeToDec _ (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
  where
    inputObjToDec :: GQL.InputObjectTypeDefinition -> Q Result
    inputObjToDec = error "not implemented" -- TODO: this will return a `GQLSchema`

-- data ObjectTypeDefinition
--   = ObjectTypeDefinition
--   { _otdDescription          :: !(Maybe Description)
--   , _otdName                 :: !Name
--   , _otdImplementsInterfaces :: ![NamedType]
--   , _otdDirectives           :: ![Directive]
--   , _otdFieldsDefinition     :: ![FieldDefinition]
--   }

-- data FieldDefinition
--   = FieldDefinition
--   { _fldDescription         :: !(Maybe Description)
--   , _fldName                :: !Name
--   , _fldArgumentsDefinition :: !ArgumentsDefinition
--   , _fldType                :: !GType
--   , _fldDirectives          :: ![Directive]
--   }

-- type ArgumentsDefinition = [InputValueDefinition]

-- data InputValueDefinition
--   = InputValueDefinition
--   { _ivdDescription  :: !(Maybe Description)
--   , _ivdName         :: !Name
--   , _ivdType         :: !GType
--   , _ivdDefaultValue :: !(Maybe DefaultValue)
--   }

-- data GType
--   = TypeNamed !Nullability !NamedType
--   | TypeList !Nullability !ListType

-- newtype Nullability
--   = Nullability { unNullability :: Bool }

-- newtype NamedType
--   = NamedType { unNamedType :: Name }

-- newtype ListType
--   = ListType {unListType :: GType }

-- data EnumTypeDefinition
--   = EnumTypeDefinition
--   { _etdDescription      :: !(Maybe Description)
--   , _etdName             :: !Name
--   , _etdDirectives       :: ![Directive]
--   , _etdValueDefinitions :: ![EnumValueDefinition]
--   }

-- data InputObjectTypeDefinition
--   = InputObjectTypeDefinition
--   { _iotdDescription      :: !(Maybe Description)
--   , _iotdName             :: !Name
--   , _iotdDirectives       :: ![Directive]
--   , _iotdValueDefinitions :: ![InputValueDefinition]
--   }

schemaFromGQL :: [GQL.ValueConst] -> Q Type
schemaFromGQL = (typesToList <$>) . traverse schemaFromGQLValue

schemaFromGQLValue :: GQL.ValueConst -> Q Type
schemaFromGQLValue (GQL.VCInt _)     = [t|'TPrimitive Int32|]
schemaFromGQLValue (GQL.VCFloat _)   = [t|'TPrimitive Double|]
schemaFromGQLValue (GQL.VCString _)  = [t|'TPrimitive T.Text|]
schemaFromGQLValue (GQL.VCBoolean _) = [t|'TPrimitive Bool|]
schemaFromGQLValue GQL.VCNull        = [t|'TPrimitive 'TNull|]
schemaFromGQLValue (GQL.VCEnum _ {- !EnumValue -}) = undefined -- TODO:
schemaFromGQLValue (GQL.VCList vals) = [t|'TList $(schemaFromGQL $ GQL.unListValue vals)|]
schemaFromGQLValue (GQL.VCObject _ {- !ObjectValueC -}) = undefined -- TODO:

-- newtype ObjectValueG a
--   = ObjectValueG {unObjectValue :: [ObjectFieldG a]}

-- type ObjectValueC = ObjectValueG ValueConst

-- data ObjectFieldG a
--   = ObjectFieldG
--   { _ofName  :: Name
--   , _ofValue :: a

typesToList :: [Type] -> Type
typesToList = foldr (\y ys -> AppT (AppT PromotedConsT y) ys) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit s = litT $ strTyLit $ T.unpack s
