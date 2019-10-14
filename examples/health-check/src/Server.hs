{-# language OverloadedStrings, PartialTypeSignatures #-}
module Main where

import Control.Concurrent.STM
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Conduit.TMChan
import Data.Maybe
import qualified Data.Text as T
import DeferredFolds.UnfoldlM
import qualified StmContainers.Map as M

import Mu.Server
import Mu.Server.GRpc

import Common

main :: IO ()
main = do 
  m <- M.newIO
  upd <- newTBMChanIO 100
  putStrLn "running health check application"
  runGRpcApp 8080 (server m upd)

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/handler/HealthServiceImpl.scala

type StatusMap = M.Map T.Text T.Text
type StatusUpdates = TBMChan HealthStatus

server :: StatusMap -> StatusUpdates -> ServerIO HealthCheckService _
server m upd
  = Server (setStatus m upd :<|>:
            checkH m :<|>:
            clearStatus m :<|>:
            checkAll m :<|>:
            cleanAll m :<|>:
            watch upd :<|>: H0)

setStatus :: StatusMap -> StatusUpdates -> HealthStatus -> IO ()
setStatus m upd s@(HealthStatus (HealthCheck nm) (ServerStatus ss))
  = do putStr "setStatus: " >> print (nm, ss)
       atomically $ do M.insert ss nm m
                       writeTBMChan upd s

checkH :: StatusMap -> HealthCheck -> IO ServerStatus
checkH m (HealthCheck nm)
  = do putStr "check: " >> print nm
       ss <- atomically $ M.lookup nm m
       return $ ServerStatus (fromMaybe "UNKNOWN" ss)

clearStatus :: StatusMap -> HealthCheck -> IO ()
clearStatus m (HealthCheck nm)
  = do putStr "clearStatus: " >> print nm
       atomically $ M.delete nm m

checkAll :: StatusMap -> IO AllStatus
checkAll m
  = do putStrLn "checkAll"
       AllStatus <$> atomically (consumeValues kvToStatus (M.unfoldlM m))
  where consumeValues :: Monad m => (k -> v -> a) -> UnfoldlM m (k,v) -> m [a]
        consumeValues f = foldlM' (\xs (x,y) -> pure (f x y:xs)) []
        kvToStatus k v = HealthStatus (HealthCheck k) (ServerStatus v)

cleanAll :: StatusMap -> IO ()
cleanAll m
  = do putStrLn "cleanAll"
       atomically $ M.reset m

watch :: StatusUpdates -> HealthCheck -> ConduitT ServerStatus Void IO () -> IO ()
watch upd hc@(HealthCheck nm) sink
  = do putStr "watch: " >> print nm
       runConduit $ sourceTBMChan upd
                 .| C.filter (\(HealthStatus c _) -> hc == c)
                 .| C.map (\(HealthStatus _ s) -> s)
                 .| sink