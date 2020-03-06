{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}

module Mu.GraphQL.Server where

import qualified Data.Aeson                    as A
import           Data.Functor.Identity
import           Data.Proxy
import qualified Data.Text.Lazy                as T
import           Data.Text.Lazy.Encoding       (decodeUtf8)
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import           Mu.GraphQL.Query.Run          (runPipeline)
import           Mu.Server
import           Network.HTTP.Types.Method     (StdMethod (..), parseMethod)
import           Network.Wai
import           Network.Wai.Handler.Warp      (Port, Settings, run, runSettings)

graphQLApp :: forall chn p hs qr mut.
       ServerT Identity chn p ServerErrorIO hs
    -> Proxy qr
    -> Proxy mut
    -> Application
graphQLApp server _ _ req res =
  case parseMethod (requestMethod req) of
    Left err   -> undefined -- FIXME: pure $ A.object [ ("errors", ("message", "Unsupported method")) ]
    Right GET  ->
      case lookup "query" (queryString req) of
        Just (Just query) -> undefined
        Nothing           -> undefined -- FIXME: throw error
    Right POST -> do
      query <- strictRequestBody req
      case parseExecutableDoc $ T.toStrict $ decodeUtf8 query of
        Left err  -> undefined -- FIXME: throw error
        Right doc -> undefined -- runPipeline server @qr @mut _ (Just doc)
    _    -> undefined

