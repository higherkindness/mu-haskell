{-# language OverloadedStrings #-}
{-# language ViewPatterns      #-}
module Mu.GraphQL.Quasi.LostParser (
  parseTypeSysDefinition, parseDoc
) where

import           Data.Foldable        (toList)
import qualified Data.Text            as T
import           Language.GraphQL.AST (document)
import qualified Language.GraphQL.AST as GQL
import           Text.Megaparsec      (runParser)

parseDoc :: T.Text -> Either T.Text [GQL.Definition]
parseDoc s =
  case runParser document "<doc>" s of
    Right d -> Right (toList d)
    Left  e -> Left (T.pack $ show e)

parseTypeSysDefinition :: T.Text -> Either T.Text [GQL.TypeSystemDefinition]
parseTypeSysDefinition s =
  case runParser document "<doc>" s of
    Right (toList -> d)
      -> let tds = [td | GQL.TypeSystemDefinition td _ <- d]
         in if length d == length tds
               then Right tds
               else Left "unexpected query or type system extension"
    Left e
      -> Left (T.pack $ show e)
