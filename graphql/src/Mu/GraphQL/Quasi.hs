{-# language OverloadedStrings #-}
{-# language TemplateHaskell   #-}

module Mu.GraphQL.Quasi where

import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Data.Int
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Language.Haskell.TH

import           Mu.Schema.Definition

-- | Imports an graphql definition written in-line as a 'Schema'.
graphql :: T.Text -> Q [Dec]
graphql schema =
  case parseExecutableDoc schema of
    Left e  -> fail ("could not parse graphql spec: " ++ show e)
    Right p -> graphqlToDecls p

graphqlToDecls :: GQL.ExecutableDocument -> Q [Dec]
graphqlToDecls = error "not implemented"

schemaFromGQL :: [GQL.Value] -> Q Type
schemaFromGQL = (typesToList <$>) . traverse schemaFromGQLType

schemaFromGQLType :: GQL.Value -> Q Type
  -- | VVariable !Variable
schemaFromGQLType (GQL.VInt _) = [t|'TPrimitive Int32|]
  -- | VFloat !Double
  -- | VString !StringValue
  -- | VBoolean !Bool
  -- | VNull
  -- | VEnum !EnumValue
  -- | VList !ListValue
  -- | VObject !ObjectValue

typesToList :: [Type] -> Type
typesToList = foldr (\y ys -> AppT (AppT PromotedConsT y) ys) PromotedNilT

textToStrLit :: T.Text -> Q Type
textToStrLit s = litT $ strTyLit $ T.unpack s
