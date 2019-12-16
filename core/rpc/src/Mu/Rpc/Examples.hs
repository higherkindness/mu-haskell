{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Mu.Rpc.Examples where

import           Data.Conduit
import           Data.Conduit.Combinators as C
import           Data.Functor.MaybeLike
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
                 '[ 'ArgSingle ('ViaSchema QuickstartSchema "HelloRequest") ]
                 ('RetSingle ('ViaSchema QuickstartSchema "HelloResponse"))
       , 'Method "SayHi" '[]
                 '[ 'ArgSingle ('ViaSchema QuickstartSchema "HiRequest")]
                 ('RetStream ('ViaSchema QuickstartSchema "HelloResponse"))
       , 'Method "SayManyHellos" '[]
                 '[ 'ArgStream ('ViaSchema QuickstartSchema "HelloRequest")]
                 ('RetStream ('ViaSchema QuickstartSchema "HelloResponse")) ]

newtype HelloRequest f = HelloRequest { name :: f T.Text } deriving (Generic)
deriving instance Functor f => ToSchema f QuickstartSchema "HelloRequest" (HelloRequest f)
deriving instance Functor f => FromSchema f QuickstartSchema "HelloRequest" (HelloRequest f)

newtype HelloResponse f = HelloResponse { message :: f T.Text } deriving (Generic)
deriving instance Functor f => ToSchema f QuickstartSchema "HelloResponse" (HelloResponse f)
deriving instance Functor f => FromSchema f QuickstartSchema "HelloResponse" (HelloResponse f)

newtype HiRequest f = HiRequest { number :: f Int } deriving (Generic)
deriving instance Functor f => ToSchema f QuickstartSchema "HiRequest" (HiRequest f)
deriving instance Functor f => FromSchema f QuickstartSchema "HiRequest" (HiRequest f)

quickstartServer :: forall m f.
                    (MonadServer m, Applicative f, MaybeLike f)
                 => ServerT f QuickStartService m _
quickstartServer
  = Server (sayHello :<|>: sayHi :<|>: sayManyHellos :<|>: H0)
  where sayHello :: HelloRequest f -> m (HelloResponse f)
        sayHello (HelloRequest nm)
          = return (HelloResponse (("hi, " <>) <$> nm))
        sayHi :: HiRequest f
              -> ConduitT (HelloResponse f) Void m ()
              -> m ()
        sayHi (HiRequest (likeMaybe -> Just n)) sink
          = runConduit $ C.replicate n (HelloResponse $ pure "hi!") .| sink
        sayHi (HiRequest _) sink
          = runConduit $ return () .| sink
        sayManyHellos :: ConduitT () (HelloRequest f) m ()
                      -> ConduitT (HelloResponse f) Void m ()
                      -> m ()
        sayManyHellos source sink
          = runConduit $ source .| C.mapM sayHello .| sink
