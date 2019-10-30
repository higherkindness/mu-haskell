{-# language PolyKinds, DataKinds, GADTs,
             MultiParamTypeClasses,
             FlexibleInstances, OverloadedStrings,
             DeriveGeneric, DeriveAnyClass, TypeOperators,
             PartialTypeSignatures, TypeFamilies #-}
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
  = '[ 'DRecord "HelloRequest" '[]
               '[ 'FieldDef "name" '[ ProtoBufId 1 ] ('TPrimitive T.Text) ]
     , 'DRecord "HelloResponse" '[]
                '[ 'FieldDef "message" '[ ProtoBufId 1 ] ('TPrimitive T.Text) ]
     , 'DRecord "HiRequest" '[]
               '[ 'FieldDef "number" '[ ProtoBufId 1 ] ('TPrimitive Int) ]
     ]

type QuickStartService
  = 'Service "Greeter" '[Package "helloworld"]
      '[ 'Method "SayHello" '[]
                 '[ 'ArgSingle ('FromSchema QuickstartSchema "HelloRequest") ]
                 ('RetSingle ('FromSchema QuickstartSchema "HelloResponse"))
       , 'Method "SayHi" '[]
                 '[ 'ArgSingle ('FromSchema QuickstartSchema "HiRequest")]
                 ('RetStream ('FromSchema QuickstartSchema "HelloResponse"))
       , 'Method "SayManyHellos" '[]
                 '[ 'ArgStream ('FromSchema QuickstartSchema "HelloRequest")]
                 ('RetStream ('FromSchema QuickstartSchema "HelloResponse")) ]

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