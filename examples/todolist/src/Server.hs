{-# language DataKinds             #-}
{-# language NamedFieldPuns        #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import           Data.Int
import           Data.List              (find)

import           Mu.GRpc.Server
import           Mu.Server

import           Definition
import           Prelude                hiding (id)

main :: IO ()
main = do
  putStrLn "running todolist application"
  todoId <- newTVarIO 0
  todolist <- newTVarIO []
  runGRpcApp msgProtoBuf 8080 (server todoId todolist)

-- Server implementation
-- https://github.com/frees-io/freestyle/blob/master/modules/examples/todolist-lib/src/main/scala/todo/service/TodoListService.scala

type Id = TVar Int32
type TodoList = TVar [TodoListMessage]

server :: Id -> TodoList -> ServerIO info TodoListService _
server i t
  = singleService ( method @"reset"    $ reset i t
                  , method @"insert"   $ insert i t
                  , method @"retrieve" $ retrieve t
                  , method @"list"     $ list_ t
                  , method @"update"   $ update t
                  , method @"destroy"  $ destroy t )

reset :: Id -> TodoList -> ServerErrorIO MessageId
reset i t = alwaysOk $ do
  putStrLn "reset"
  atomically $ do
    writeTVar i 0
    writeTVar t []
  pure $ MessageId (-1)

insert :: Id -> TodoList -> TodoListRequest -> ServerErrorIO TodoListResponse
insert oldId t (TodoListRequest titl tgId) = alwaysOk $ do
  putStr "insert: " >> print (titl, tgId)
  atomically $ do
    modifyTVar oldId (+1)
    newId <- readTVar oldId
    let newTodo = TodoListMessage newId tgId titl False
    modifyTVar t (newTodo:)
    pure $ TodoListResponse (Just newTodo)

getMsg :: Int32 -> TodoListMessage -> Bool
getMsg x TodoListMessage {id} = id == x

retrieve :: TodoList -> MessageId -> ServerErrorIO TodoListResponse
retrieve t (MessageId idMsg) = do
  liftIO (putStr "retrieve: " >> print idMsg)
  todos <- liftIO $ readTVarIO t
  case find (getMsg idMsg) todos of
    Just todo -> pure $ TodoListResponse (Just todo)
    Nothing   -> serverError $ ServerError NotFound "unknown todolist id"

list_ :: TodoList -> ServerErrorIO TodoListList
list_ t = alwaysOk $ do
  putStrLn "list"
  atomically $ do
    todos <- readTVar t
    pure $ TodoListList todos

update :: TodoList -> TodoListMessage -> ServerErrorIO TodoListResponse
update t mg@(TodoListMessage idM titM tgM compl) = alwaysOk $ do
  putStr "update: " >> print (idM, titM, tgM, compl)
  atomically $ modifyTVar t $ fmap (\m -> if getMsg idM m then mg else m)
  pure $ TodoListResponse (Just mg)

destroy :: TodoList -> MessageId -> ServerErrorIO MessageId
destroy t (MessageId idMsg) =  do
  liftIO (putStr "destroy: ") >> liftIO (print idMsg)
  r <- liftIO $ atomically $ do
         todos <- readTVar t
         case find (getMsg idMsg) todos of
           Just todo -> do
             modifyTVar t $ filter (/=todo)
             pure $ Just (MessageId idMsg) -- OK ✅
           Nothing   -> pure Nothing -- did nothing
  maybe (serverError $ ServerError NotFound "unknown message id") pure r
