{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             FlexibleInstances,
             OverloadedStrings,
             DeriveGeneric, DeriveAnyClass,
             TypeApplications, TypeOperators,
             PartialTypeSignatures,
             TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Rpc.Examples where

import qualified Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Rpc
import Mu.Server
import Mu.Schema.Adapter.ProtoBuf

-- Defines the service from gRPC Quickstart
-- https://grpc.io/docs/quickstart/python/

type QuickstartSchema
  = '[ 'DRecord "HelloRequest"
               '[ 'FieldDef "name" ('TPrimitive T.Text) ]
     , 'DRecord "HelloResponse"
                '[ 'FieldDef "message" ('TPrimitive T.Text) ]
     ]

type instance ProtoBufFieldIds QuickstartSchema "HelloRequest"
  = '[ "name" ':<->: 1 ]
type instance ProtoBufFieldIds QuickstartSchema "HelloResponse"
  = '[ "message" ':<->: 1 ]

type QuickStartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle QuickstartSchema "HelloRequest"]
                 ('RetSingle QuickstartSchema "HelloResponse") ]

newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloRequest")
newtype HelloResponse = HelloResponse { message :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloResponse")

quickstartServer :: ServerIO QuickStartService _
quickstartServer
  = Server (sayHello :<|>: H0)
  where sayHello :: HelloRequest -> IO HelloResponse
        sayHello (HelloRequest nm) = return (HelloResponse ("hi, " <> nm))
            