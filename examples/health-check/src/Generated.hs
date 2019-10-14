{-# language TemplateHaskell, DataKinds,
             MultiParamTypeClasses, FlexibleInstances,
             TypeFamilies, DeriveGeneric #-}
module Generated where

import Mu.Schema.TH

import Definition

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