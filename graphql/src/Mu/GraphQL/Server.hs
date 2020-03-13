{-# language DataKinds         #-}
{-# language FlexibleContexts  #-}
{-# language GADTs             #-}
{-# language OverloadedLists   #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mu.GraphQL.Server (
    GraphQLApp
  , runGraphQLApp
  , runGraphQLAppSettings
  , graphQLApp
  , graphQLAppTrans
) where

import           Control.Applicative           ((<|>))
import           Control.Monad                 (join)
import qualified Data.Aeson                    as A
import           Data.Aeson.Text               (encodeToLazyText)
import           Data.ByteString.Lazy          (fromStrict, toStrict)
import           Data.Coerce                   (coerce)
import qualified Data.Foldable                 as F
import           Data.HashMap.Strict           (empty, toList)
import           Data.Proxy
import           Data.Scientific               (floatingOrInteger)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import qualified Data.Text.Lazy.Encoding       as T
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax
import           Mu.GraphQL.Query.Parse
import           Mu.GraphQL.Query.Run
import           Mu.Server
import           Network.HTTP.Types.Header     (hContentType)
import           Network.HTTP.Types.Method     (StdMethod (..), parseMethod)
import           Network.HTTP.Types.Status     (ok200)
import           Network.Wai
import           Network.Wai.Handler.Warp      (Port, Settings, run, runSettings)

data GraphQLInput = GraphQLInput T.Text VariableMapC (Maybe T.Text)

instance A.FromJSON GraphQLInput where
  parseJSON = A.withObject "GraphQLInput" $
     \v -> GraphQLInput
      <$> v A..: "query"
      <*> (v A..: "variables" <|> pure empty)
      <*> v A..:? "operationName"

instance A.FromJSON ValueConst where
  parseJSON A.Null       = pure VCNull
  parseJSON (A.Bool b)   = pure $ VCBoolean b
  parseJSON (A.String s) = pure $ VCString $ coerce s
  parseJSON (A.Number n) = pure $ either VCFloat VCInt $ floatingOrInteger n
  parseJSON (A.Array xs) = VCList . ListValueG . F.toList <$> traverse A.parseJSON xs
  parseJSON (A.Object o) = VCObject . ObjectValueG . fmap toObjFld . toList <$> traverse A.parseJSON o
    where
      toObjFld :: (T.Text, ValueConst) -> ObjectFieldG ValueConst
      toObjFld (k, v) = ObjectFieldG (coerce k) v

-- | Turn a Mu GraphQL 'Server' into a WAI 'Application'.
--
--   These 'Application's can be later combined using,
--   for example, @wai-routes@, or you can add middleware
--   from @wai-extra@, among others.
graphQLApp ::
    ( GraphQLApp ServerErrorIO p pname ss qmethods mmethods hs chn qr mut qanns manns )
    => ServerT chn p ServerErrorIO hs
    -> Proxy qr
    -> Proxy mut
    -> Application
graphQLApp = graphQLAppTrans id

graphQLAppTrans ::
    ( GraphQLApp m p pname ss qmethods mmethods hs chn qr mut qanns manns )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Proxy qr
    -> Proxy mut
    -> Application
graphQLAppTrans f server q m req res =
  case parseMethod (requestMethod req) of
    Left err   -> toError $ decodeUtf8 err
    Right GET  -> do
      let qst = queryString req
          opN = decodeUtf8 <$> join (lookup "operationName" qst)
      case (fmap decodeUtf8 <$> lookup "query" qst, lookup "variables" qst) of
        (Just (Just qry), Just (Just vars)) ->
          case A.eitherDecode $ fromStrict vars of
            Left err  -> toError $ T.pack err
            Right vrs -> execQuery opN vrs qry
        (Just (Just qry), _)                -> execQuery opN empty qry
        _                                   -> toError "Error parsing query"
    Right POST -> do
      body <- strictRequestBody req
      case lookup hContentType $ requestHeaders req of
        Just "application/json"    ->
          case A.eitherDecode body of
            Left err                             -> toError $ T.pack err
            Right (GraphQLInput qry vars opName) -> execQuery opName vars qry
        Just "application/graphql" ->
          execQuery Nothing empty (decodeUtf8 $ toStrict body)
        _                          -> toError "No `Content-Type` header found!"
    _          -> toError "Unsupported method"
  where
    execQuery :: Maybe T.Text -> VariableMapC -> T.Text -> IO ResponseReceived
    execQuery opn vals qry =
      case parseExecutableDoc qry of
        Left err  -> toError err
        Right doc -> runPipeline f server q m opn vals doc >>= toResponse
    toError :: T.Text -> IO ResponseReceived
    toError err = toResponse $ A.object [ ("errors", A.Array [ A.object [ ("message", A.String err) ] ])]
    toResponse :: A.Value -> IO ResponseReceived
    toResponse = res . responseBuilder ok200 [] . T.encodeUtf8Builder . encodeToLazyText

-- | Run a Mu 'graphQLApp' using the given 'Settings'.
--
--   Go to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGraphQLAppSettings ::
  ( GraphQLApp ServerErrorIO p pname ss qmethods mmethods hs chn qr mut qanns manns )
  => Settings
  -> ServerT chn p ServerErrorIO hs
  -> Proxy qr
  -> Proxy mut
  -> IO ()
runGraphQLAppSettings st svr q m = runSettings st (graphQLApp svr q m)

-- | Run a Mu 'graphQLApp' on the given port.
runGraphQLApp ::
  ( GraphQLApp ServerErrorIO p pname ss qmethods mmethods hs chn qr mut qanns manns )
  => Port
  -> ServerT chn p ServerErrorIO hs
  -> Proxy qr
  -> Proxy mut
  -> IO ()
runGraphQLApp port svr q m = run port (graphQLApp svr q m)
