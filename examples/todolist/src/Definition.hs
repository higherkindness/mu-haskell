{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module Definition where

import           Data.Int
import           Data.Text     (Text)
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

grpc "TodoListSchema" id "todolist.proto"

newtype MessageId = MessageId
  { value :: Maybe Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema Maybe TodoListSchema "MessageId")

data TodoListMessage = TodoListMessage
  { id, tagId :: Maybe Int32
  , title     :: Maybe Text
  , completed :: Maybe Bool
  } deriving (Eq, Show, Ord, Generic, HasSchema Maybe TodoListSchema "TodoListMessage")

data TodoListRequest = TodoListRequest
  { title :: Maybe Text
  , tagId :: Maybe Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema Maybe TodoListSchema "TodoListRequest")

newtype TodoListList = TodoListList
  { list :: Maybe [TodoListMessage]
  } deriving (Eq, Show, Ord, Generic, HasSchema Maybe TodoListSchema "TodoListList")

newtype TodoListResponse = TodoListResponse
  { msg :: Maybe TodoListMessage
  } deriving (Eq, Show, Ord, Generic, HasSchema Maybe TodoListSchema "TodoListResponse")
