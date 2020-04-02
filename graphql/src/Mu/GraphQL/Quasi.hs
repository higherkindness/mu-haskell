{-# language DataKinds         #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}
{-# language ViewPatterns      #-}

module Mu.GraphQL.Quasi where

import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (parseSchemaDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

import           Mu.Rpc
import           Mu.Schema.Definition          ()

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
typeToDec _ (GQL.TypeDefinitionScalar _)             = error "not implemented" -- TODO: handle "well-known scalars"
typeToDec schemaName (GQL.TypeDefinitionObject objs) = objToDec objs
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
    argToType (GQL.InputValueDefinition _ (GQL.unName -> aname) atype _) =
      [t| 'ArgSingle ('Just $(textToStrLit aname)) '[] $(gtypeToType atype) |]
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
