{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             DeriveGeneric, DeriveAnyClass,
             FlexibleInstances, FlexibleContexts,
             TemplateHaskell #-}
module Common where

import Data.Text as T

import Mu.Schema
import Mu.Schema.Adapter.ProtoBuf
import Mu.Schema.TH
import Mu.Rpc

-- Schema for data serialization
type HealthCheckSchema
  = '[ 'DRecord "HealthCheck" '[]
                '[ 'FieldDef "nameService" '[ ProtoBufId 1] ('TPrimitive T.Text) ]
     , 'DRecord "ServerStatus" '[]
                '[ 'FieldDef "status" '[ ProtoBufId 1 ] ('TPrimitive T.Text) ]
     , 'DRecord "HealthStatus" '[]
                '[ 'FieldDef "hc" '[ ProtoBufId 1 ] ('TSchematic "HealthCheck")
                 , 'FieldDef "status" '[ ProtoBufId 2 ] ('TSchematic "ServerStatus") ]
     , 'DRecord "AllStatus" '[]
                '[ 'FieldDef "all" '[ ProtoBufId 1 ] ('TList ('TSchematic "HealthStatus")) ]
     ]

-- Haskell types for serialization
$(generateTypesFromSchema (++ "Msg") ''HealthCheckSchema)
{-
newtype HealthCheckMsg = HealthCheckMsg { healthCheckNameMsgService :: T.Text }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "HealthCheck")

newtype ServerStatusMsg = ServerStatusMsg { serverStatusMsgStatus :: T.Text }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "ServerStatus")

data HealthStatusMsg = HealthStatusMsg { healthStatusMsgHealthCheck  :: HealthCheckMsg,
                                       , healthStatusMsgServerStatus :: ServerStatusMsg }
  deriving (Show, Eq, Ord, Generic, HasSchema "HealthStatus")

newtype AllStatusMsg = AllStatusMsg { allStatusMsgAll :: [HealthStatusMsg] }
  deriving (Show, Eq, Ord, Generic, HasSchema HealthCheckSchema "AllStatus")
-}

-- Service definition
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/service.scala
type HS = 'FromSchema HealthCheckSchema
type HealthCheckService
  = 'Service "HealthCheckService" '[Package "healthcheck"]
      '[ 'Method "setStatus" '[] '[ 'ArgSingle (HS "HealthStatus") ] 'RetNothing
       , 'Method "check" '[] '[ 'ArgSingle (HS "HealthCheck") ] ('RetSingle (HS "ServerStatus"))
       , 'Method "clearStatus" '[] '[ 'ArgSingle (HS "HealthCheck") ] 'RetNothing
       , 'Method "checkAll" '[] '[ ] ('RetSingle (HS "AllStatus"))
       , 'Method "cleanAll" '[] '[ ] 'RetNothing
       , 'Method "watch" '[] '[ 'ArgSingle (HS "HealthCheck") ] ('RetStream (HS "ServerStatus"))
       ]