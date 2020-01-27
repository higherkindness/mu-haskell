{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent.STM
import           Data.Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.TMChan
import qualified Data.Text                as T
import           DeferredFolds.UnfoldlM
import qualified StmContainers.Map        as M

import           Mu.GRpc.Server
import           Mu.Server

import           Definition

main :: IO ()
main = do
  m <- M.newIO
  upd <- newTBMChanIO 100
  putStrLn "running health check application"
  runGRpcApp msgProtoBuf 8080 (server m upd)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/handler/HealthServiceImpl.scala

type StatusMap = M.Map T.Text T.Text
type StatusUpdates = TBMChan HealthStatusMsg

server :: StatusMap -> StatusUpdates -> ServerIO Maybe HealthCheckService _
server m upd = Server (setStatus_ m upd :<|>: checkH_ m :<|>: clearStatus_ m :<|>:
  checkAll_ m :<|>: cleanAll_ m :<|>: watch_ upd :<|>: H0)

setStatus_ :: StatusMap -> StatusUpdates -> HealthStatusMsg -> ServerErrorIO ()
setStatus_ m upd
           s@(HealthStatusMsg (Just (HealthCheckMsg (Just nm))) (Just (ServerStatusMsg (Just ss))))
  = alwaysOk $ do
      putStr "setStatus: " >> print (nm, ss)
      atomically $ do
        M.insert ss nm m
        writeTBMChan upd s
setStatus_ _ _ _ = serverError (ServerError Invalid "name or status missing")

checkH_ :: StatusMap -> HealthCheckMsg -> ServerErrorIO ServerStatusMsg
checkH_ m (HealthCheckMsg (Just nm)) = alwaysOk $ do
  putStr "check: " >> print nm
  ss <- atomically $ M.lookup nm m
  return $ ServerStatusMsg ss
checkH_ _ _ = serverError (ServerError Invalid "no server name given")

clearStatus_ :: StatusMap -> HealthCheckMsg -> ServerErrorIO ()
clearStatus_ m (HealthCheckMsg (Just nm)) = alwaysOk $ do
  putStr "clearStatus: " >> print nm
  atomically $ M.delete nm m
clearStatus_ _ _ = return ()

checkAll_ :: StatusMap -> ServerErrorIO AllStatusMsg
checkAll_ m = alwaysOk $ do
    putStrLn "checkAll"
    AllStatusMsg . Just <$> atomically (consumeValues kvToStatus (M.unfoldlM m))
  where
    consumeValues :: Monad m => (k -> v -> a) -> UnfoldlM m (k,v) -> m [a]
    consumeValues f = foldlM' (\xs (x,y) -> pure (f x y:xs)) []
    kvToStatus k v = HealthStatusMsg (Just (HealthCheckMsg (Just k))) (Just (ServerStatusMsg (Just v)))

cleanAll_ :: StatusMap -> ServerErrorIO ()
cleanAll_ m = alwaysOk $ do
  putStrLn "cleanAll"
  atomically $ M.reset m

watch_ :: StatusUpdates
       -> HealthCheckMsg
       -> ConduitT ServerStatusMsg Void ServerErrorIO ()
       -> ServerErrorIO ()
watch_ upd hcm@(HealthCheckMsg nm) sink = do
  alwaysOk (putStr "watch: " >> print nm)
  runConduit $ sourceTBMChan upd
            .| C.filter (\(HealthStatusMsg c _) -> Just hcm == c)
            .| C.map (\(HealthStatusMsg _ s) -> s)
            .| catMaybesC
            .| sink
  where
    catMaybesC = do x <- await
                    case x of
                      Just (Just y) -> yield y >> catMaybesC
                      Just Nothing  -> catMaybesC
                      Nothing       -> return ()
