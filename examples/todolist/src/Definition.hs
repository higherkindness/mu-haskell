{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Definition where

import           Data.Int
import           Data.Text     as T
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

$(grpc "TodoListSchema" id "todolist.proto")

newtype MessageId =
  MessageId
    { value :: Int32
    }
  deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "MessageId")

data TodoListMessage =
  TodoListMessage
    { id, tagId :: Int32
    , title     :: T.Text
    }
  deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListMessage")

data TodoListRequest =
  TodoListRequest
    { title :: T.Text
    , tagId :: Int32
    }
  deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListRequest")

newtype TodoListList =
  TodoListList
    { list :: [TodoListMessage]
    }
  deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListList")

newtype TodoListResponse =
  TodoListResponse
    { msg :: TodoListMessage
    }
  deriving (Eq, Show, Ord, Generic, HasSchema TodoListSchema "TodoListResponse")
