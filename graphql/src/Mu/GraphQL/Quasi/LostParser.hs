module Mu.GraphQl.Quasi.LostParser (
  parseTypeSysDefinition
) where

import           Control.Applicative           ((<|>))
import           Data.Attoparsec.Text
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (runParser, schemaDocument)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Text.Parser.Token             (whiteSpace)

schemaDefinition :: Parser GQL.SchemaDefinition
schemaDefinition = error "not implemented" -- TODO:

-- data SchemaDefinition
--   = SchemaDefinition
--   { _sdDirectives                   :: !(Maybe [Directive]) -- ignore -> Nothing
--   , _sdRootOperationTypeDefinitions :: ![RootOperationTypeDefinition]
--   }

-- data RootOperationTypeDefinition
--   = RootOperationTypeDefinition
--   { _rotdOperationType     :: !OperationType
--   , _rotdOperationTypeType :: !NamedType -- FIXME: coerces to `Name`!
--   }

-- data OperationType
--   = OperationTypeQuery
--   | OperationTypeMutation
--   | OperationTypeSubscription

typeSystemDefinition :: Parser [GQL.TypeSystemDefinition]
typeSystemDefinition =
   whiteSpace *> fmap concat (many1 (
       (\(GQL.SchemaDocument d) -> map GQL.TypeSystemDefinitionType d) <$> schemaDocument
   <|> (: []) . GQL.TypeSystemDefinitionSchema <$> schemaDefinition ) )

-- data TypeSystemDefinition
--   = TypeSystemDefinitionSchema !SchemaDefinition
--   | TypeSystemDefinitionType !TypeDefinition

parseTypeSysDefinition :: T.Text -> Either T.Text [GQL.TypeSystemDefinition]
parseTypeSysDefinition = runParser typeSystemDefinition
