{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
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

import           Data.Text             as T
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

#if __GHCIDE__
grpc "HealthCheckSchema" id "examples/health-check/healthcheck.proto"
#else
grpc "HealthCheckSchema" id "healthcheck.proto"
#endif

type HealthCheckService = HealthCheckServiceFS2

newtype HealthCheckMsg
  = HealthCheckMsg { nameService :: Maybe T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe HealthCheckSchema "HealthCheck"
           , FromSchema Maybe HealthCheckSchema "HealthCheck" )
newtype ServerStatusMsg
  = ServerStatusMsg { status :: Maybe T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe HealthCheckSchema "ServerStatus"
           , FromSchema Maybe HealthCheckSchema "ServerStatus" )
data HealthStatusMsg
  = HealthStatusMsg { hc :: Maybe HealthCheckMsg, status :: Maybe ServerStatusMsg }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe HealthCheckSchema "HealthStatus"
           , FromSchema Maybe HealthCheckSchema "HealthStatus" )
newtype AllStatusMsg
  = AllStatusMsg { all :: Maybe [HealthStatusMsg] }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe HealthCheckSchema "AllStatus"
           , FromSchema Maybe HealthCheckSchema "AllStatus" )
