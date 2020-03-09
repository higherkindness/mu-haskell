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

import           Data.Text     as T
import           GHC.Generics

import           Mu.Quasi.Avro
import           Mu.Schema

#if __GHCIDE__
avdl "HealthCheckSchema" "HealthCheckService" "examples/health-check/avro" "healthcheck.avdl"
#else
avdl "HealthCheckSchema" "HealthCheckService" "." "healthcheck.avdl"
#endif

newtype HealthCheckMsg
  = HealthCheckMsg { nameService :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "HealthCheck"
           , FromSchema HealthCheckSchema "HealthCheck" )
newtype ServerStatusMsg
  = ServerStatusMsg { status :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "ServerStatus"
           , FromSchema HealthCheckSchema "ServerStatus" )
data HealthStatusMsg
  = HealthStatusMsg { hc :: HealthCheckMsg, status :: ServerStatusMsg }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "HealthStatus"
           , FromSchema HealthCheckSchema "HealthStatus" )
newtype AllStatusMsg
  = AllStatusMsg { all :: [HealthStatusMsg] }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   HealthCheckSchema "AllStatus"
           , FromSchema HealthCheckSchema "AllStatus" )
