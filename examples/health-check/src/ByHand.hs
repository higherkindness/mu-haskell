{-# language DataKinds, TypeFamilies, TypeOperators,
             MultiParamTypeClasses, FlexibleInstances,
             DeriveGeneric, TypeApplications #-}
module ByHand where

import Data.Conduit
import qualified Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Client.GRpc
import Mu.Client.GRpc.Record

import Definition

newtype HealthCheckMsg = HealthCheckMsg { healthCheckNameMsgService :: T.Text }
  deriving (Show, Eq, Ord, Generic)
instance HasSchema HealthCheckSchema "HealthCheck" HealthCheckMsg where
  type FieldMapping HealthCheckSchema "HealthCheck" HealthCheckMsg
    = '[ "healthCheckNameMsgService" ':-> "nameService" ]

newtype ServerStatusMsg = ServerStatusMsg { serverStatusMsgStatus :: T.Text }
  deriving (Show, Eq, Ord, Generic)
instance HasSchema HealthCheckSchema "ServerStatus" ServerStatusMsg where
  type FieldMapping HealthCheckSchema "ServerStatus" ServerStatusMsg
    = '[ "serverStatusMsgStatus" ':-> "status" ]

data HealthStatusMsg = HealthStatusMsg { healthStatusMsgHealthCheck  :: HealthCheckMsg
                                       , healthStatusMsgServerStatus :: ServerStatusMsg }
  deriving (Show, Eq, Ord, Generic)
instance HasSchema HealthCheckSchema "HealthStatus" HealthStatusMsg where
  type FieldMapping HealthCheckSchema "HealthStatus" HealthStatusMsg
    = '[ "healthStatusMsgHealthCheck"  ':-> "hc"
       , "healthStatusMsgServerStatus" ':-> "status" ]

newtype AllStatusMsg = AllStatusMsg { allStatusMsgAll :: [HealthStatusMsg] }
  deriving (Show, Eq, Ord, Generic)
instance HasSchema HealthCheckSchema "AllStatus" AllStatusMsg where
  type FieldMapping HealthCheckSchema "AllStatus" AllStatusMsg
    = '[ "allStatusMsgAll" ':-> "all" ]

data HealthCall
  = HealthCall
    { setStatus :: HealthStatusMsg -> IO (GRpcReply ()) 
    , check :: HealthCheckMsg -> IO (GRpcReply ServerStatusMsg) 
    , clearStatus :: HealthCheckMsg -> IO (GRpcReply ()) 
    , checkAll :: IO (GRpcReply AllStatusMsg) 
    , cleanAll :: IO (GRpcReply ()) 
    , watch :: HealthCheckMsg -> IO (ConduitT () (GRpcReply ServerStatusMsg) IO ()) }
  deriving (Generic)

buildHealthCall :: GrpcClient -> HealthCall
buildHealthCall = buildService @HealthCheckService @""