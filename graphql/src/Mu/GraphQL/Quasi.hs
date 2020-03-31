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
graphql :: T.Text -> Q [Dec]
graphql schema =
  case parseSchemaDoc schema of
    Left e  -> fail ("could not parse graphql spec: " ++ show e)
    Right p -> graphqlToDecls p

graphqlToDecls :: GQL.SchemaDocument -> Q [Dec]
graphqlToDecls (GQL.SchemaDocument types) = traverse typeToDec types

typeToDec :: GQL.TypeDefinition -> Q Dec
typeToDec (GQL.TypeDefinitionScalar _)          = fail "scalar types are not supported"
typeToDec (GQL.TypeDefinitionObject objs)       = objToDec objs
  where
    objToDec :: GQL.ObjectTypeDefinition -> Q Dec
    objToDec = error "not implemented" -- TODO:
typeToDec (GQL.TypeDefinitionInterface _)       = fail "interface types are not supported"
typeToDec (GQL.TypeDefinitionUnion _)           = fail "union types are not supported"
typeToDec (GQL.TypeDefinitionEnum enums)        = enumToDecl enums
  where
    enumToDecl :: GQL.EnumTypeDefinition -> Q Dec
    enumToDecl = error "not implemented" -- TODO:
typeToDec (GQL.TypeDefinitionInputObject inpts) = inputObjToDec inpts
  where
    inputObjToDec :: GQL.InputObjectTypeDefinition -> Q Dec
    inputObjToDec = error "not implemented" -- TODO:

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
--   , _fldType                :: !GType TODO:
--   , _fldDirectives          :: ![Directive]
--   }

-- type ArgumentsDefinition = [InputValueDefinition] FIXME:

-- data InputValueDefinition
--   = InputValueDefinition
--   { _ivdDescription  :: !(Maybe Description)
--   , _ivdName         :: !Name
--   , _ivdType         :: !GType TODO:
--   , _ivdDefaultValue :: !(Maybe DefaultValue)
--   }

-- data GType TODO:
--   = TypeNamed !Nullability !NamedType
--   | TypeList !Nullability !ListType

-- newtype Nullability
--   = Nullability { unNullability :: Bool }

-- newtype NamedType
--   = NamedType { unNamedType :: Name }

-- newtype ListType
--   = ListType {unListType :: GType }

---

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
--   , _iotdValueDefinitions :: ![InputValueDefinition] FIXME:
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
