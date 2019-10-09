{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             DeriveGeneric, DeriveAnyClass,
             FlexibleInstances, FlexibleContexts #-}
module Common where

import Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Schema.Adapter.ProtoBuf
import Mu.Rpc

-- Schema for data serialization
type HealthCheckSchema
  = '[ 'DRecord "HealthCheck"
                '[ 'FieldDef "nameService" ('TPrimitive T.Text) ]
     , 'DRecord "ServerStatus"
                '[ 'FieldDef "status" ('TPrimitive T.Text) ]
     , 'DRecord "HealthStatus"
                '[ 'FieldDef "hc" ('TSchematic "HealthCheck")
                 , 'FieldDef "status" ('TSchematic "ServerStatus") ]
     , 'DRecord "AllStatus"
                '[ 'FieldDef "all" ('TList ('TSchematic "HealthStatus")) ]
     ]

-- Haskell types for serialization
newtype HealthCheck = HealthCheck { nameService :: T.Text }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "HealthCheck")

newtype ServerStatus = ServerStatus { status :: T.Text }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "ServerStatus")

data HealthStatus = HealthStatus { healthCheck :: HealthCheck, serverStatus :: ServerStatus }
  deriving (Show, Eq, Ord, Generic)
instance HasSchema HealthCheckSchema "HealthStatus" HealthStatus where
  type FieldMapping HealthCheckSchema "HealthStatus" HealthStatus
         = [ "healthCheck" ':<->: "hc", "serverStatus" ':<->: "status" ]

newtype AllStatus = AllStatus { all :: [HealthStatus] }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "AllStatus")

-- Protocol buffer field ids
type instance ProtoBufFieldIds HealthCheckSchema "HealthCheck"
  = '[ "nameService" ':<->: 1 ]
type instance ProtoBufFieldIds HealthCheckSchema "ServerStatus"
  = '[ "status" ':<->: 1 ]
type instance ProtoBufFieldIds HealthCheckSchema "HealthStatus"
  = '[ "hc" ':<->: 1, "status" ':<->: 2 ]
type instance ProtoBufFieldIds HealthCheckSchema "AllStatus"
  = '[ "all" ':<->: 1 ]

-- Service definition
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/service.scala
type HS = 'FromSchema HealthCheckSchema
type HealthCheckService
  = 'Service "HealthCheckService"
      '[ 'Method "setStatus" '[ 'ArgSingle (HS "HealthStatus") ] 'RetNothing
       , 'Method "check" '[ 'ArgSingle (HS "HealthCheck") ] ('RetSingle (HS "ServerStatus"))
       , 'Method "clearStatus" '[ 'ArgSingle (HS "HealthCheck") ] 'RetNothing
       , 'Method "checkAll" '[ ] ('RetSingle (HS "AllStatus"))
       , 'Method "cleanAll" '[ ] 'RetNothing
       , 'Method "watch" '[ 'ArgSingle (HS "HealthCheck") ] ('RetStream (HS "ServerStatus"))
       ]