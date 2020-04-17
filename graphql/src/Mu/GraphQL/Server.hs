{-# language DataKinds           #-}
{-# language FlexibleContexts    #-}
{-# language GADTs               #-}
{-# language OverloadedLists     #-}
{-# language OverloadedStrings   #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-|
Description : Execute a Mu 'Server' using GraphQL

This module allows you to server a Mu 'Server'
as a WAI 'Application' using GraphQL.

The simples way is to use 'runGraphQLAppQuery'
(if you only provide GraphQL queries) or
'runGraphQLApp' (if you also have mutations
or subscriptions). All other variants provide
more control over the settings.
-}
module Mu.GraphQL.Server (
    GraphQLApp
  -- * Run an GraphQL resolver directly
  , runGraphQLApp
  , runGraphQLAppSettings
  , runGraphQLAppQuery
  , runGraphQLAppTrans
  -- * Build a WAI 'Application'
  , graphQLApp
  , graphQLAppQuery
  , graphQLAppTrans
  , graphQLAppTransQuery
) where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (join)
import qualified Data.Aeson                       as A
import           Data.Aeson.Text                  (encodeToLazyText)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import qualified Data.HashMap.Strict              as HM
import           Data.Proxy
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import qualified Data.Text.Lazy.Encoding          as T
import           Language.GraphQL.Draft.Parser    (parseExecutableDoc)
import           Network.HTTP.Types.Header        (hContentType)
import           Network.HTTP.Types.Method        (StdMethod (..), parseMethod)
import           Network.HTTP.Types.Status        (ok200)
import           Network.Wai
import           Network.Wai.Handler.Warp         (Port, Settings, run, runSettings)
import qualified Network.Wai.Handler.WebSockets   as WS
import qualified Network.WebSockets               as WS

import           Mu.GraphQL.Query.Parse
import           Mu.GraphQL.Query.Run
import           Mu.GraphQL.Subscription.Protocol
import           Mu.Server

data GraphQLInput = GraphQLInput T.Text VariableMapC (Maybe T.Text)

instance A.FromJSON GraphQLInput where
  parseJSON = A.withObject "GraphQLInput" $
     \v -> GraphQLInput
      <$> v A..: "query"
      <*> (v A..: "variables" <|> pure HM.empty)
      <*> v A..:? "operationName"

-- | Turn a Mu GraphQL 'Server' into a WAI 'Application'.
--   Use this version when your server has not only
--   queries, but also mutations or subscriptions.
graphQLApp ::
    ( GraphQLApp p qr mut sub ServerErrorIO chn hs )
    => ServerT chn p ServerErrorIO hs
    -> Proxy qr
    -> Proxy mut
    -> Proxy sub
    -> Application
graphQLApp = graphQLAppTrans id

-- | Turn a Mu GraphQL 'Server' into a WAI 'Application'.
--   Use this version when your server has only queries.
graphQLAppQuery ::
    forall qr p chn hs.
    ( GraphQLApp p ('Just qr) 'Nothing 'Nothing ServerErrorIO chn hs )
    => ServerT chn p ServerErrorIO hs
    -> Proxy qr
    -> Application
graphQLAppQuery svr _
  = graphQLApp svr (Proxy @('Just qr)) (Proxy @'Nothing) (Proxy @'Nothing)

-- | Turn a Mu GraphQL 'Server' into a WAI 'Application'
--   using a combined transformer stack.
--   See also documentation for 'graphQLAppQuery'.
graphQLAppTransQuery ::
    forall qr m p chn hs.
    ( GraphQLApp p ('Just qr) 'Nothing 'Nothing m chn hs )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Proxy qr
    -> Application
graphQLAppTransQuery f svr _
  = graphQLAppTrans f svr (Proxy @('Just qr)) (Proxy @'Nothing) (Proxy @'Nothing)

-- | Turn a Mu GraphQL 'Server' into a WAI 'Application'
--   using a combined transformer stack.
--   See also documentation for 'graphQLApp'.
graphQLAppTrans ::
    ( GraphQLApp p qr mut sub m chn hs )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Proxy qr
    -> Proxy mut
    -> Proxy sub
    -> Application
graphQLAppTrans f server q m s
  = WS.websocketsOr WS.defaultConnectionOptions
                    (wsGraphQLAppTrans f server q m s)
                    (httpGraphQLAppTrans f server q m s)

httpGraphQLAppTrans ::
    ( GraphQLApp p qr mut sub m chn hs )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Proxy qr
    -> Proxy mut
    -> Proxy sub
    -> Application
httpGraphQLAppTrans f server q m s req res =
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
        (Just (Just qry), _)                -> execQuery opN HM.empty qry
        _                                   -> toError "Error parsing query"
    Right POST -> do
      body <- strictRequestBody req
      case lookup hContentType $ requestHeaders req of
        Just "application/json"    ->
          case A.eitherDecode body of
            Left err                             -> toError $ T.pack err
            Right (GraphQLInput qry vars opName) -> execQuery opName vars qry
        Just "application/graphql" ->
          execQuery Nothing HM.empty (decodeUtf8 $ toStrict body)
        _                          -> toError "No `Content-Type` header found!"
    _          -> toError "Unsupported method"
  where
    execQuery :: Maybe T.Text -> VariableMapC -> T.Text -> IO ResponseReceived
    execQuery opn vals qry =
      case parseExecutableDoc qry of
        Left err  -> toError err
        Right doc -> runPipeline f server q m s opn vals doc >>= toResponse
    toError :: T.Text -> IO ResponseReceived
    toError err = toResponse $ A.object [ ("errors", A.Array [ A.object [ ("message", A.String err) ] ])]
    toResponse :: A.Value -> IO ResponseReceived
    toResponse = res . responseBuilder ok200 [] . T.encodeUtf8Builder . encodeToLazyText

wsGraphQLAppTrans
    :: ( GraphQLApp p qr mut sub m chn hs )
    => (forall a. m a -> ServerErrorIO a)
    -> ServerT chn p m hs
    -> Proxy qr
    -> Proxy mut
    -> Proxy sub
    -> WS.ServerApp
wsGraphQLAppTrans f server q m s conn
  = do conn' <- WS.acceptRequest conn
       flip protocol conn' $ runSubscriptionPipeline f server q m s

-- | Run a Mu 'graphQLApp' using the given 'Settings'.
--
--   Go to 'Network.Wai.Handler.Warp' to declare 'Settings'.
runGraphQLAppSettings ::
  ( GraphQLApp p qr mut sub ServerErrorIO chn hs )
  => Settings
  -> ServerT chn p ServerErrorIO hs
  -> Proxy qr
  -> Proxy mut
  -> Proxy sub
  -> IO ()
runGraphQLAppSettings st svr q m s = runSettings st (graphQLApp svr q m s)

-- | Run a Mu 'graphQLApp' on the given port.
runGraphQLApp ::
  ( GraphQLApp p qr mut sub ServerErrorIO chn hs )
  => Port
  -> ServerT chn p ServerErrorIO hs
  -> Proxy qr
  -> Proxy mut
  -> Proxy sub
  -> IO ()
runGraphQLApp port svr q m s = run port (graphQLApp svr q m s)

-- | Run a Mu 'graphQLApp' on a transformer stack on the given port.
runGraphQLAppTrans ::
  ( GraphQLApp p qr mut sub m chn hs )
  => Port
  -> (forall a. m a -> ServerErrorIO a)
  -> ServerT chn p m hs
  -> Proxy qr
  -> Proxy mut
  -> Proxy sub
  -> IO ()
runGraphQLAppTrans port f svr q m s = run port (graphQLAppTrans f svr q m s)

-- | Run a query-only Mu 'graphQLApp' on the given port.
runGraphQLAppQuery ::
  ( GraphQLApp p ('Just qr) 'Nothing 'Nothing ServerErrorIO chn hs )
  => Port
  -> ServerT chn p ServerErrorIO hs
  -> Proxy qr
  -> IO ()
runGraphQLAppQuery port svr q = run port (graphQLAppQuery svr q)
