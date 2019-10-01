{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             OverloadedStrings,
             DeriveGeneric, DeriveAnyClass,
             TypeApplications, TypeOperators,
             PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Rpc.Examples where

import qualified Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Rpc
import Mu.Server

-- Defines the service from gRPC Quickstart
-- https://grpc.io/docs/quickstart/python/

type QuickstartSchema
  = '[ 'DRecord "HelloRequest"
               '[ 'FieldDef "name" ('TPrimitive T.Text) ]
     , 'DRecord "HelloResponse"
                '[ 'FieldDef "message" ('TPrimitive T.Text) ]
     ]

type QuickStartService
  = 'Service "Example"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle '(QuickstartSchema, "HelloRequest")]
                 ('RetSingle '(QuickstartSchema, "HelloResponse")) ]

newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloRequest")
newtype HelloResponse = HelloResponse { message :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloResponse")

quickstartServer :: ServerIO QuickStartService _
quickstartServer
  = Server (sayHello :<|>: H0)
  where sayHello :: HelloRequest -> IO HelloResponse
        sayHello (HelloRequest nm) = return (HelloResponse ("hi, " <> nm))
            