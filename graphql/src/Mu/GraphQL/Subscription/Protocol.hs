{-# language FlexibleContexts    #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-
This module implements the protocol as specified in
https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
-}
module Mu.GraphQL.Subscription.Protocol where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Aeson                    ((.:), (.:?), (.=))
import qualified Data.Aeson                    as A
import           Data.Conduit
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax (ExecutableDocument)
import qualified ListT                         as L
import           Network.WebSockets
import qualified StmContainers.Map             as M

import           Mu.GraphQL.Query.Parse

protocol :: ( Maybe T.Text -> VariableMapC -> ExecutableDocument
              -> ConduitT A.Value Void IO ()
              -> IO () )
         -> Connection -> IO ()
protocol f conn = start
  where
    -- listen for GQL_CONNECTION_INIT
    start = do
      msg <- receiveJSON conn
      case msg of
        Just (GQLConnectionInit _)
          -> do -- send GQL_CONNECTION_ACK
                sendJSON conn GQLConnectionAck
                vars <- M.newIO
                -- send GQL_KEEP_ALIVE each 1s.
                ka <- async $ do
                  sendJSON conn GQLKeepAlive
                  threadDelay 1000000
                -- start listening for incoming messages
                listen ka vars
        _ -> start  -- Keep waiting
    -- listen for messages from client
    listen ka vars = do
      msg <- receiveJSON conn
      case msg of
        Just (GQLStart i q v o)  -- start handling
          -> do t <- async $ handle i q v o >> atomically (M.delete i vars)
                atomically $ M.insert t i vars
                listen ka vars
        Just (GQLStop i)  -- stop with handling that query
          -> do r <- atomically $ M.lookup i vars
                case r of
                  Nothing -> return ()
                  Just a  -> do cancel a
                                atomically $ M.delete i vars
                listen ka vars
        Just GQLTerminate  -- terminate all queries
          -> do cancelAll ka vars
                sendClose conn ("GraphQL session terminated" :: T.Text)
        _ -> listen ka vars  -- Keep going
    -- Handle a single query
    handle i q v o
      = case parseExecutableDoc q of
          Left err -> sendJSON conn (GQLError i (A.toJSON err))
          Right d  -> do
            f o v d (cndt i)
            sendJSON conn (GQLComplete i)
    -- Conduit which sends the results via the wire
    cndt i = do
      msg <- await
      case msg of
        Nothing -> return ()
        Just v  -> do liftIO $ sendJSON conn (GQLData i v)
                      cndt i
    -- Cancel all pending subscriptions
    cancelAll ka vars
      = do cancel ka
           vs <- atomically $ L.toList $ M.listT vars
           forM_ (map snd vs) cancel

receiveJSON :: A.FromJSON a => Connection -> IO (Maybe a)
receiveJSON conn = do
  d <- receiveData conn
  return $ A.decode d

sendJSON :: A.ToJSON a => Connection -> a -> IO ()
sendJSON conn v
  = sendTextData conn (A.encode v)

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
                , payload     :: A.Value }
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
       _ -> empty
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
