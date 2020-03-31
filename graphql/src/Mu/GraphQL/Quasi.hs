{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}

module Mu.GraphQL.Quasi where

import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Data.Int
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (parseSchemaDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

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
  rs <- traverse typeToDec types
  let schemaTypes  = [x | GQLSchema  x <- rs]
      serviceTypes = [x | GQLService x <- rs]
      schemaName'  = mkName schemaName
      serviceName' = mkName serviceName
  schemaDec <- tySynD schemaName' [] (pure $ typesToList schemaTypes)
  serviceDec <- tySynD serviceName' [] (pure $ typesToList serviceTypes)
  pure [schemaDec, serviceDec]

typeToDec :: GQL.TypeDefinition -> Q Result
typeToDec (GQL.TypeDefinitionScalar _)          = fail "scalar types are not supported"
typeToDec (GQL.TypeDefinitionObject objs)       = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Result
    objToDec = error "not implemented" -- TODO: this will return a `GQLService`
typeToDec (GQL.TypeDefinitionInterface _)       = fail "interface types are not supported"
typeToDec (GQL.TypeDefinitionUnion _)           = fail "union types are not supported"
typeToDec (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Result
    enumToDecl = error "not implemented" -- TODO: this will return a `GQLSchema`
typeToDec (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
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
-- schemaFromGQLValue (GQL.VCEnum !EnumValue
schemaFromGQLValue (GQL.VCList vals) = [t|'TList $(schemaFromGQL $ GQL.unListValue vals)|]
-- schemaFromGQLValue (GQL.VCObject !ObjectValueC

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
