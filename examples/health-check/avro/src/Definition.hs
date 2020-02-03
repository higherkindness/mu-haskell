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

import           Data.Functor.Identity
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
  = HealthCheckMsg { nameService :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Identity HealthCheckSchema "HealthCheck"
           , FromSchema Identity HealthCheckSchema "HealthCheck" )
newtype ServerStatusMsg
  = ServerStatusMsg { status :: T.Text }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Identity HealthCheckSchema "ServerStatus"
           , FromSchema Identity HealthCheckSchema "ServerStatus" )
data HealthStatusMsg
  = HealthStatusMsg { hc :: HealthCheckMsg, status :: ServerStatusMsg }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Identity HealthCheckSchema "HealthStatus"
           , FromSchema Identity HealthCheckSchema "HealthStatus" )
newtype AllStatusMsg
  = AllStatusMsg { all :: [HealthStatusMsg] }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Identity HealthCheckSchema "AllStatus"
           , FromSchema Identity HealthCheckSchema "AllStatus" )
