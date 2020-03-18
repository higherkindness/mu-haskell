{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-
This module implements the protocol as specified in
https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
-}
module Mu.GraphQL.Subscription.Protocol where

import           Control.Applicative
import           Data.Aeson             ((.:), (.:?), (.=))
import qualified Data.Aeson             as A
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Network.WebSockets

import           Mu.GraphQL.Query.Parse

data ClientMessage
  = GQLConnectionInit { initPayload :: Maybe A.Value }
  | GQLStart { clientMsgId   :: T.Text
             , query         :: T.Text
             , variables     :: VariableMapC
             , operationName :: Maybe T.Text}
  | GQLStop { clientMsgId :: T.Text }
  | GQLTerminate
  deriving Show

data ServerMessage
  = GQLConnectionError { errorPayload :: Maybe A.Value }
  | GQLConnectionAck
  | GQLData     { serverMsgId :: T.Text
                , payload     :: A.Value }
  | GQLError    { serverMsgId :: T.Text
                , payload     :: A.Value}
  | GQLComplete { serverMsgId :: T.Text}
  | GQLKeepAlive
  deriving Show

instance A.FromJSON ClientMessage where
  parseJSON = A.withObject "ClientMessage" $ \v -> do
     ty :: String <- v .: "type"
     case ty of
       "GQL_CONNECTION_INIT"
         -> GQLConnectionInit <$> v .:? "payload"
       "GQL_START"
         -> do i <- v .: "id"
               (q,vrs,opN) <- v .: "payload" >>= parsePayload
               pure $ GQLStart i q vrs opN
       "GQL_STOP"
         -> GQLStop <$> v .: "id"
       "GQL_TERMINATE"
         -> pure GQLTerminate
    where
      parsePayload = A.withObject "ClientMessage/GQL_START" $
        \v -> (,,) <$> v .: "query"
                   <*> (v .: "variables" <|> pure HM.empty)
                   <*> v .:? "operationName"

theType :: (A.KeyValue kv) => T.Text -> kv
theType t = "type" .= t

instance A.ToJSON ServerMessage where
  toJSON (GQLConnectionError e)
    = A.object [theType "GQL_CONNECTION_ERROR", "payload" .= e]
  toJSON GQLConnectionAck
    = A.object [theType "GQL_CONNECTION_ACK"]
  toJSON (GQLData i p)
    = A.object [theType "GQL_DATA", "id" .= i, "payload" .= p]
  toJSON (GQLError i p)
    = A.object [theType "GQL_ERROR", "id" .= i, "payload" .= p]
  toJSON (GQLComplete i)
    = A.object [theType "GQL_COMPLETE", "id" .= i]
  toJSON GQLKeepAlive
    = A.object [theType "GQL_CONNECTION_KEEP_ALIVE"]
