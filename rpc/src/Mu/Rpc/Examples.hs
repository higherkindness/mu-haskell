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

import Data.Conduit
import Data.Conduit.Combinators as C
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
     , 'DRecord "HiRequest"
               '[ 'FieldDef "number" ('TPrimitive Int) ]
     ]

type instance ProtoBufFieldIds QuickstartSchema "HelloRequest"
  = '[ "name" ':<->: 1 ]
type instance ProtoBufFieldIds QuickstartSchema "HelloResponse"
  = '[ "message" ':<->: 1 ]
type instance ProtoBufFieldIds QuickstartSchema "HiRequest"
  = '[ "number" ':<->: 1 ]

type QuickStartService
  = 'Service "Greeter"
      '[ 'Method "SayHello"
                 '[ 'ArgSingle QuickstartSchema "HelloRequest" ]
                 ('RetSingle QuickstartSchema "HelloResponse")
       , 'Method "SayHi"
                 '[ 'ArgSingle QuickstartSchema "HiRequest"]
                 ('RetStream QuickstartSchema "HelloResponse")
       , 'Method "SayManyHellos"
                 '[ 'ArgStream QuickstartSchema "HelloRequest"]
                 ('RetStream QuickstartSchema "HelloResponse") ]

newtype HelloRequest = HelloRequest { name :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloRequest")
newtype HelloResponse = HelloResponse { message :: T.Text }
  deriving (Generic, HasSchema QuickstartSchema "HelloResponse")
newtype HiRequest = HiRequest { number :: Int }
  deriving (Generic, HasSchema QuickstartSchema "HiRequest")

quickstartServer :: ServerIO QuickStartService _
quickstartServer
  = Server (sayHello :<|>: sayHi :<|>: sayManyHellos :<|>: H0)
  where sayHello :: HelloRequest -> IO HelloResponse
        sayHello (HelloRequest nm)
          = return (HelloResponse ("hi, " <> nm))
        sayHi :: HiRequest -> ConduitT HelloResponse Void IO () -> IO ()
        sayHi (HiRequest n) sink
          = runConduit $ C.replicate n (HelloResponse "hi!") .| sink
        sayManyHellos :: ConduitT () HelloRequest IO ()
                      -> ConduitT HelloResponse Void IO ()
                      -> IO ()
        sayManyHellos source sink
          = runConduit $ source .| C.mapM sayHello .| sink