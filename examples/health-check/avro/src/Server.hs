{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent.STM
import           Data.Conduit.TMChan
import           Data.Functor.Identity
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import           DeferredFolds.UnfoldlM
import qualified StmContainers.Map      as M

import           Mu.GRpc.Server
import           Mu.Server

import           Definition

main :: IO ()
main = do
  m <- M.newIO
  upd <- newTBMChanIO 100
  putStrLn "running health check application"
  runGRpcApp msgAvro 50051 (server m upd)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/handler/HealthServiceImpl.scala

type StatusMap = M.Map T.Text T.Text
type StatusUpdates = TBMChan HealthStatusMsg

server :: StatusMap -> StatusUpdates -> ServerIO Identity HealthCheckService _
server m upd = Server (
  checkH_ m :<|>:
  checkAll_ m :<|>:
  cleanAll_ m :<|>:
  clearStatus_ m :<|>:
  setStatus_ m upd :<|>:
  {- watch_ upd :<|>: -} H0)

setStatus_ :: StatusMap -> StatusUpdates -> HealthStatusMsg -> ServerErrorIO ()
setStatus_ m upd s@(HealthStatusMsg (HealthCheckMsg nm) (ServerStatusMsg ss))
  = alwaysOk $ do
      putStr "setStatus: " >> print (nm, ss)
      atomically $ do
        M.insert ss nm m
        writeTBMChan upd s
      print =<< atomically (M.lookup nm m)

checkH_ :: StatusMap -> HealthCheckMsg -> ServerErrorIO ServerStatusMsg
checkH_ m (HealthCheckMsg nm) = alwaysOk $ do
  putStr "check: " >> print nm
  ss <- atomically $ M.lookup nm m
  print ss
  return $ ServerStatusMsg (fromMaybe "<unknown>" ss)

clearStatus_ :: StatusMap -> HealthCheckMsg -> ServerErrorIO ()
clearStatus_ m (HealthCheckMsg nm) = alwaysOk $ do
  putStr "clearStatus: " >> print nm
  atomically $ M.delete nm m

checkAll_ :: StatusMap -> ServerErrorIO AllStatusMsg
checkAll_ m = alwaysOk $ do
    putStrLn "checkAll"
    AllStatusMsg <$> atomically (consumeValues kvToStatus (M.unfoldlM m))
  where
    consumeValues :: Monad m => (k -> v -> a) -> UnfoldlM m (k,v) -> m [a]
    consumeValues f = foldlM' (\xs (x,y) -> pure (f x y:xs)) []
    kvToStatus k v = HealthStatusMsg (HealthCheckMsg k) (ServerStatusMsg v)

cleanAll_ :: StatusMap -> ServerErrorIO ()
cleanAll_ m = alwaysOk $ do
  putStrLn "cleanAll"
  atomically $ M.reset m

{- Note: no "streams" in avro
watch_ :: StatusUpdates
       -> HealthCheckMsg
       -> ConduitT ServerStatusMsg Void ServerErrorIO ()
       -> ServerErrorIO ()
watch_ upd hcm@(HealthCheckMsg nm) sink = do
  alwaysOk (putStr "watch: " >> print nm)
  runConduit $ sourceTBMChan upd
            .| C.filter (\(HealthStatusMsg c _) -> hcm == c)
            .| C.map (\(HealthStatusMsg _ s) -> s)
            .| sink
-}
