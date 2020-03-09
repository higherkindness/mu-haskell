{-# language DataKinds           #-}
{-# language FlexibleContexts    #-}
{-# language GADTs               #-}
{-# language OverloadedStrings   #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}

module Mu.GraphQL.Server where

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Text               as AT
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy          as BL
import           Data.Functor.Identity
import qualified Data.HashMap.Strict           as HM
import           Data.Proxy
import           Data.Text.Encoding            (decodeUtf8)
import qualified Data.Text.Lazy.Encoding       as T
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Mu.GraphQL.Query.Parse
import           Mu.GraphQL.Query.Run
import           Mu.Rpc
import           Mu.Schema.Definition          (MappingRight)
import           Mu.Server
import           Network.HTTP.Types.Method     (StdMethod (..), parseMethod)
import           Network.HTTP.Types.Status     (ok200)
import           Network.Wai
import           Network.Wai.Handler.Warp      (Port, Settings, run, runSettings)

graphQLApp :: forall pname ss chn (p :: Package') hs qr mut qanns qmethods manns mmethods.
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
    Left err   -> undefined -- FIXME: pure $ A.object [ ("errors", ("message", "Unsupported method")) ]
    Right GET  ->
      case lookup "query" (queryString req) of
        Just (Just query) -> execQuery query
        _                 -> undefined -- FIXME: throw error
    Right POST -> do
      query <- strictRequestBody req
      execQuery $ BL.toStrict query
    _    -> undefined -- FIXME: throw error
  where
    execQuery :: B.ByteString -> IO ResponseReceived
    execQuery query =
      case parseExecutableDoc $ decodeUtf8 query of
        Left err  -> undefined -- FIXME: throw error
        Right doc -> do
          value <- toJSONValue doc
          res $ responseBuilder ok200 [] $ T.encodeUtf8Builder $ AT.encodeToLazyText value
    toJSONValue :: GQL.ExecutableDocument -> IO A.Value
    toJSONValue = runPipeline server q m Nothing HM.empty
