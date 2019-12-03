{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeOperators         #-}

module Definition where

import           Data.Int
import           Data.Text     (Text)
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

grpc "TodoListSchema" id "todolist.proto"

newtype MessageId = MessageId
  { value :: Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "MessageId")

data TodoListMessage = TodoListMessage
  { id, tagId :: Int32
  , title     :: Text
  , completed :: Bool
  } deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListMessage")

data TodoListRequest = TodoListRequest
  { title :: Text
  , tagId :: Int32
  } deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListRequest")

newtype TodoListList = TodoListList
  { list :: [TodoListMessage]
  } deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListList")

newtype TodoListResponse = TodoListResponse
  { msg :: TodoListMessage
  } deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListResponse")
