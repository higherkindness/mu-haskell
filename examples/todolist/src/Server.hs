{-# language DataKinds             #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}

module Main where

import           Control.Concurrent.STM
import           Data.Int
import           Data.List              (find)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T

import           Mu.GRpc.Server
import           Mu.Server

import           Definition

main :: IO ()
main = do
  putStrLn "running todolist application"
  todoId <- newTVarIO 0
  todolist <- newTVarIO []
  pure ()
  -- runGRpcApp 8080 (server m todolist)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/todolist/server/src/main/scala/handlers/TodoListRpcServiceHandler.scala
type Ids = TVar Int32
type TodoList = TVar [TodoListMessage]

-- server :: TodoList -> ServerIO TodoListService _
-- server i t = Server
--   (reset t :<|>: insert i t :<|>: retrieve m _ :<|>: list_ m :<|>: update m _ :<|>: destroy _ :<|>: H0)

reset :: TodoList -> IO MessageId
reset t = do
  putStrLn "reset"
  atomically $ writeTVar t []
  pure $ MessageId 0 -- this means nothing

insert :: Ids -> TodoList -> TodoListRequest -> IO TodoListResponse
insert oldId t (TodoListRequest titl tgId) = do
  putStr "insert: " >> print (titl, tgId)
  atomically $ do
    modifyTVar oldId (+1)
    newId <- readTVar oldId
    let newTodo = TodoListMessage newId tgId titl
    modifyTVar t (newTodo:)
    pure $ TodoListResponse newTodo

retrieve :: TodoList -> MessageId -> IO (Maybe TodoListResponse)
retrieve t (MessageId idMsg) = do
  putStr "retrieve: " >> print idMsg
  atomically $ do
    todos <- readTVar t
    pure $ TodoListResponse <$> find (\(TodoListMessage idM _ _) -> idM == idMsg) todos

list_ :: TodoList -> IO TodoListList
list_ t = do
  putStrLn "list"
  atomically $ do
    todos <- readTVar t
    pure $ TodoListList todos

update :: TodoList -> TodoListMessage -> IO TodoListResponse
update t msg@(TodoListMessage idM titM tgM) = do
  putStr "update: " >> print (idM, titM, tgM)
  atomically $ do
    todos <- readTVar t
    modifyTVar t (fmap (\ms@(TodoListMessage idMsg _ _) -> if idM == idMsg then msg else ms))
    pure $ TodoListResponse msg

destroy :: TodoList -> MessageId -> IO MessageId
destroy t (MessageId idMsg) = do
  putStr "destroy: " >> print idMsg
  atomically $ do
    todos <- readTVar t
    case find (\(TodoListMessage idM _ _) -> idM == idMsg) todos of
      Just todo -> do
        modifyTVar t (filter (/=todo))
        pure $ MessageId idMsg -- OK ✅
      Nothing   -> pure $ MessageId 0 -- did nothing
