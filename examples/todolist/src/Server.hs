{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}

module Main where

import           Control.Concurrent.STM
import           Data.Int
import           Data.List              (find)
import           Data.Maybe             (fromMaybe)

import           Mu.GRpc.Server
import           Mu.Server

import           Definition
import           Prelude                hiding (id)

main :: IO ()
main = do
  putStrLn "running todolist application"
  todoId <- newTVarIO 0
  todolist <- newTVarIO []
  runGRpcApp 8080 (server todoId todolist)

-- Server implementation
-- https://github.com/frees-io/freestyle/blob/master/modules/examples/todolist-lib/src/main/scala/todo/service/TodoListService.scala

type Id = TVar Int32
type TodoList = TVar [TodoListMessage]

server :: Id -> TodoList -> ServerIO TodoListService _
server i t = Server
  (reset i t :<|>: insert i t :<|>: retrieve t :<|>: list_ t :<|>: update t :<|>: destroy t :<|>: H0)

reset :: Id -> TodoList -> IO MessageId
reset i t = do
  putStrLn "reset"
  atomically $ do
    writeTVar i 0
    writeTVar t []
  pure $ MessageId 0 -- returns nothing

insert :: Id -> TodoList -> TodoListRequest -> IO TodoListResponse
insert oldId t (TodoListRequest titl tgId) = do
  putStr "insert: " >> print (titl, tgId)
  atomically $ do
    modifyTVar oldId (+1)
    newId <- readTVar oldId
    let newTodo = TodoListMessage newId tgId titl False
    modifyTVar t (newTodo:)
    pure $ TodoListResponse newTodo

getMsg :: Int32 -> TodoListMessage -> Bool
getMsg x TodoListMessage {id} = id == x

retrieve :: TodoList -> MessageId -> IO TodoListResponse
retrieve t (MessageId idMsg) = do
  putStr "retrieve: " >> print idMsg
  todos <- readTVarIO t
  let todo = fromMaybe (TodoListMessage 0 0 "I don't know" False) (find (getMsg idMsg) todos)
  pure $ TodoListResponse todo

list_ :: TodoList -> IO TodoListList
list_ t = do
  putStrLn "list"
  atomically $ do
    todos <- readTVar t
    pure $ TodoListList todos

update :: TodoList -> TodoListMessage -> IO TodoListResponse
update t mg@(TodoListMessage idM titM tgM compl) = do
  putStr "update: " >> print (idM, titM, tgM, compl)
  atomically $ modifyTVar t (fmap (\m -> if getMsg idM m then mg else m))
  pure $ TodoListResponse mg

destroy :: TodoList -> MessageId -> IO MessageId
destroy t (MessageId idMsg) = do
  putStr "destroy: " >> print idMsg
  atomically $ do
    todos <- readTVar t
    case find (getMsg idMsg) todos of
      Just todo -> do
        modifyTVar t (filter (/=todo))
        pure $ MessageId idMsg -- OK ✅
      Nothing   -> pure $ MessageId 0 -- did nothing
