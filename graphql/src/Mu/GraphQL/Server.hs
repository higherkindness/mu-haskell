{-# language DataKinds         #-}
{-# language FlexibleContexts  #-}
{-# language GADTs             #-}
{-# language OverloadedLists   #-}
{-# language OverloadedStrings #-}

module Mu.GraphQL.Server where

import qualified Data.Aeson                    as A
import           Data.Aeson.Text               (encodeToLazyText)
import qualified Data.ByteString.Internal      as B
import           Data.ByteString.Lazy          (toStrict)
import           Data.Functor.Identity
import           Data.HashMap.Strict           (empty)
import           Data.Proxy
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import qualified Data.Text.Lazy.Encoding       as T
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax (ExecutableDocument)
import           Mu.GraphQL.Query.Parse
import           Mu.GraphQL.Query.Run
import           Mu.Rpc
import           Mu.Schema.Definition          (MappingRight)
import           Mu.Server
import           Network.HTTP.Types.Header     (hContentType)
import           Network.HTTP.Types.Method     (StdMethod (..), parseMethod)
import           Network.HTTP.Types.Status     (ok200)
import           Network.Wai
import           Network.Wai.Handler.Warp      (Settings, runSettings)

graphQLApp ::
    ( p ~ 'Package pname ss
     , LookupService ss qr ~ 'Service qr qanns qmethods
     , ParseMethod p qmethods
     , LookupService ss mut ~ 'Service mut manns mmethods
     , ParseMethod p mmethods
     , RunQueryFindHandler p hs chn ss (LookupService ss qr) hs
     , MappingRight chn qr ~ ()
     , RunQueryFindHandler p hs chn ss (LookupService ss mut) hs
     , MappingRight chn mut ~ ()
     )
    => ServerT Identity chn p ServerErrorIO hs
    -> Proxy qr
    -> Proxy mut
    -> Application
graphQLApp server q m req res =
  case parseMethod (requestMethod req) of
    Left err   -> toError $ decodeUtf8 err
    Right GET  ->
      case lookup "query" (queryString req) of
        Just (Just query) -> execQuery query
        _                 -> toError "Error parsing query"
    Right POST ->
      case lookup hContentType $ requestHeaders req of
        Just "application/json"    -> undefined -- TODO: do as with GET reading from json params
        Just "application/graphql" -> strictRequestBody req >>= execQuery . toStrict
        _                          -> toError "No `Content-Type` header found!"
    _          -> toError "Unsupported method"
  where
    execQuery :: B.ByteString -> IO ResponseReceived
    execQuery query =
      case parseExecutableDoc $ decodeUtf8 query of
        Left err  -> toError err
        Right doc -> toJSONValue doc >>= toResponse
    toJSONValue :: ExecutableDocument -> IO A.Value
    toJSONValue = runPipeline server q m Nothing empty
    toError :: T.Text -> IO ResponseReceived
    toError err = toResponse $ A.object [ ("errors", A.Array [ A.object [ ("message", A.String err) ] ])]
    toResponse :: A.Value -> IO ResponseReceived
    toResponse = res . responseBuilder ok200 [] . T.encodeUtf8Builder . encodeToLazyText

runGraphQLApp ::
  ( p ~ 'Package pname ss
    , ParseMethod p qmethods
    , ParseMethod p mmethods
    , RunQueryFindHandler p hs chn ss (LookupService ss qr) hs
    , RunQueryFindHandler p hs chn ss (LookupService ss mut) hs
    , MappingRight chn qr ~ ()
    , LookupService ss qr ~ 'Service qr qanns qmethods
    , LookupService ss mut ~ 'Service mut manns mmethods
    , MappingRight chn mut ~ ()
  )
  => Settings
  -> ServerT Identity chn p ServerErrorIO hs
  -> Proxy qr
  -> Proxy mut
  -> IO ()
runGraphQLApp st svr q m = runSettings st (graphQLApp svr q m)
