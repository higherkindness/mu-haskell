{-# language TemplateHaskell #-}

module Definition where

import GHC.Generics
import Data.Hashable
import Data.Int
import Data.Text as T

import Mu.Schema
import Mu.Quasi.GRpc

$(grpc "TodoListSchema" id "todolist.proto")
