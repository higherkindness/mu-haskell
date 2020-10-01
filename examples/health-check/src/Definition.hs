{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingVia           #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLabels      #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
module Definition where

import qualified Data.Aeson      as J
import qualified Data.Swagger    as Swagger
import           Data.Text       as T
import           GHC.Generics

import           Mu.Adapter.Json ()
import           Mu.Quasi.GRpc
import           Mu.Schema

#if __GHCIDE__
grpc "HealthCheckSchema" id "examples/health-check/protobuf/healthcheck.proto"
#else
grpc "HealthCheckSchema" id "healthcheck.proto"
#endif

type HealthCheckService = HealthCheckServiceFS2

newtype HealthCheckMsg
  = HealthCheckMsg { nameService :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "HealthCheck"
           , FromSchema HealthCheckSchema "HealthCheck"
           , Swagger.ToSchema )
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema HealthCheckSchema "HealthCheck" HealthCheckMsg)
newtype ServerStatusMsg
  = ServerStatusMsg { status :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "ServerStatus"
           , FromSchema HealthCheckSchema "ServerStatus"
           , Swagger.ToSchema )
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema HealthCheckSchema "ServerStatus" ServerStatusMsg)
data HealthStatusMsg
  = HealthStatusMsg { hc :: Maybe HealthCheckMsg, status :: Maybe ServerStatusMsg }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "HealthStatus"
           , FromSchema HealthCheckSchema "HealthStatus"
           , Swagger.ToSchema )
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema HealthCheckSchema "HealthStatus" HealthStatusMsg)
newtype AllStatusMsg
  = AllStatusMsg { all :: [HealthStatusMsg] }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "AllStatus"
           , FromSchema HealthCheckSchema "AllStatus"
           , Swagger.ToSchema )
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema HealthCheckSchema "AllStatus" AllStatusMsg)
