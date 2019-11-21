{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import           Control.Concurrent.STM
import           Data.Int
import           Data.Maybe
import qualified Data.Text              as T
import qualified StmContainers.Map      as M

import           Mu.GRpc.Server
import           Mu.Server

import           Definition

main :: IO ()
main = do
  putStrLn "running todolist application"
  m <- M.newIO
  todolist <- newEmptyTMVarIO
  runGRpcApp 8080 (server m todolist)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/todolist/server/src/main/scala/handlers/TodoListRpcServiceHandler.scala
type TodoListMap = M.Map Int32 T.Text

server :: TodoListMap -> TVar TodoListResponse -> ServerIO TodoListService _
server m t =
  Server
    (reset m :<|>: insert m _ :<|>: retrieve m _ :<|>: list_ m :<|>: update m _ :<|>:
     destroy _ :<|>:
     H0)

reset :: TodoListMap -> IO MessageId
reset m = do
  putStrLn "reset"
  atomically $ M.reset m
  return $ MessageId 0

insert :: TodoListMap -> TodoListRequest -> IO TodoListResponse
insert m upd r@(TodoListRequest title tagId) = do
  putStr "insert: " >> print (title, tagId)
  atomically $ do
    M.insert tagId title m
    putTMVar $ TodoListResponse "Todo inserted correctly."

retrieve :: TodoListMap -> MessageId -> IO TodoListResponse
retrieve = undefined -- TODO: no idea

list_ :: TodoListMap -> IO TodoListList
list_ = undefined -- TODO: no idea

update :: TodoListMap -> TodoListMessage -> IO TodoListResponse
update = undefined -- TODO: no idea

destroy :: TodoListMap -> MessageId -> IO MessageId
destroy m (MessageId msg) = do
  putStr "destroy: " >> print msg
  atomically $ M.delete msg m
