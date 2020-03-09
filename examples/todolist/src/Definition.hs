{-# language CPP                   #-}
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

#if __GHCIDE__
grpc "TodoListSchema" id "examples/todolist/todolist.proto"
#else
grpc "TodoListSchema" id "todolist.proto"
#endif

newtype MessageId = MessageId
  { value :: Int32
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   TodoListSchema "MessageId"
             , FromSchema TodoListSchema "MessageId" )

data TodoListMessage = TodoListMessage
  { id, tagId :: Int32
  , title     :: Text
  , completed :: Bool
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   TodoListSchema "TodoListMessage"
             , FromSchema TodoListSchema "TodoListMessage" )

data TodoListRequest = TodoListRequest
  { title :: Text
  , tagId :: Int32
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   TodoListSchema "TodoListRequest"
             , FromSchema TodoListSchema "TodoListRequest" )

newtype TodoListList = TodoListList
  { list :: [TodoListMessage]
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   TodoListSchema "TodoListList"
             , FromSchema TodoListSchema "TodoListList" )

newtype TodoListResponse = TodoListResponse
  { msg :: Maybe TodoListMessage
  } deriving ( Eq, Show, Ord, Generic
             , ToSchema   TodoListSchema "TodoListResponse"
             , FromSchema TodoListSchema "TodoListResponse" )
