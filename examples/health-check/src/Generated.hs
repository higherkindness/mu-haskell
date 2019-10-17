{-# language TemplateHaskell, DataKinds,
             MultiParamTypeClasses, FlexibleInstances,
             TypeFamilies, DeriveGeneric #-}
module Generated where

import Mu.Schema.TH
import Mu.Client.GRpc.Record

import Definition

-- Haskell types for serialization
$(generateTypesFromSchema (++ "Msg") ''HealthCheckSchema)
$(generateRecordFromService "HealthCall" "" (++ "Msg") ''HealthCheckService)

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

{-
data HealthCall
  = HealthCall
    { setStatus :: HealthStatusMsg -> IO (GRpcReply ()) 
    , check :: HealthCheckMsg -> IO (GRpcReply ServerStatusMsg) 
    , clearStatus :: HealthCheckMsg -> IO (GRpcReply ()) 
    , checkAll :: IO (GRpcReply AllStatusMsg) 
    , cleanAll :: IO (GRpcReply ()) 
    , watch :: HealthCheckMsg -> IO (ConduitT () (GRpcReply ServerStatusMsg) IO ()) }
  deriving (Generic)
-}