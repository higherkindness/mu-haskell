module Mu.GraphQL.Quasi.LostParser (
  -- * The Lost 'Parser'™️
  parseTypeSysDefinition
) where

import           Control.Applicative           ((<|>))
import           Data.Attoparsec.Text          (Parser, many1)
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (nameParser, runParser, schemaDocument)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Text.Parser.Token             (braces, symbol, whiteSpace)

schemaDefinition :: Parser GQL.SchemaDefinition
schemaDefinition = symbol "schema" *> braces (
    GQL.SchemaDefinition
          Nothing -- ignore [Directive]
      <$> many1 rootOperationParser
  )

rootOperationParser :: Parser GQL.RootOperationTypeDefinition
rootOperationParser =
  GQL.RootOperationTypeDefinition
    <$> (operationTypeParser <* symbol ":")
    <*> (GQL.NamedType <$> nameParser)

operationTypeParser :: Parser GQL.OperationType
operationTypeParser =
      GQL.OperationTypeQuery <$ symbol "query"
  <|> GQL.OperationTypeMutation <$ symbol "mutation"
  <|> GQL.OperationTypeSubscription <$ symbol "subscription"

typeSystemDefinition :: Parser [GQL.TypeSystemDefinition]
typeSystemDefinition = whiteSpace *> (concat <$> many1 (
      (\(GQL.SchemaDocument d) -> GQL.TypeSystemDefinitionType <$> d) <$> schemaDocument
  <|> (: []) . GQL.TypeSystemDefinitionSchema <$> schemaDefinition ))

parseTypeSysDefinition :: T.Text -> Either T.Text [GQL.TypeSystemDefinition]
parseTypeSysDefinition = runParser typeSystemDefinition
