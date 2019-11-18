{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             FlexibleInstances, FlexibleContexts,
             DeriveGeneric, DeriveAnyClass,
             DuplicateRecordFields, OverloadedLabels,
             TemplateHaskell #-}
module Definition where

import GHC.Generics
import Data.Text as T

import Mu.Schema
import Mu.GRpc.Quasi

$(grpc "HealthCheckSchema" id "healthcheck.proto")

newtype HealthCheckMsg
  = HealthCheckMsg { nameService :: T.Text }
  deriving (Eq, Show, Ord, Generic, HasSchema HealthCheckSchema "HealthCheck")
newtype ServerStatusMsg
  = ServerStatusMsg { status :: T.Text }
  deriving (Eq, Show, Ord, Generic, HasSchema HealthCheckSchema "ServerStatus")
data HealthStatusMsg
  = HealthStatusMsg { hc :: HealthCheckMsg, status :: ServerStatusMsg }
  deriving (Eq, Show, Ord, Generic, HasSchema HealthCheckSchema "HealthStatus")
newtype AllStatusMsg
  = AllStatusMsg { all :: [HealthStatusMsg] }
  deriving (Eq, Show, Ord, Generic, HasSchema HealthCheckSchema "AllStatus")

{-
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
-}