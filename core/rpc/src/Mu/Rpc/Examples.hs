{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mu.Rpc.Examples where

import           Data.Conduit
import           Data.Conduit.Combinators as C
import qualified Data.Text                as T
import           GHC.Generics

import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

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
  where sayHello :: HelloRequest -> ServerErrorIO HelloResponse
        sayHello (HelloRequest nm)
          = return (HelloResponse ("hi, " <> nm))
        sayHi :: HiRequest
              -> ConduitT HelloResponse Void ServerErrorIO ()
              -> ServerErrorIO ()
        sayHi (HiRequest n) sink
          = runConduit $ C.replicate n (HelloResponse "hi!") .| sink
        sayManyHellos :: ConduitT () HelloRequest ServerErrorIO ()
                      -> ConduitT HelloResponse Void ServerErrorIO ()
                      -> ServerErrorIO ()
        sayManyHellos source sink
          = runConduit $ source .| C.mapM sayHello .| sink
