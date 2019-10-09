{-# language OverloadedStrings, PartialTypeSignatures #-}
module Main where

import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Text as T
import DeferredFolds.UnfoldlM
import qualified StmContainers.Map as M

import Mu.Server

import Common

main :: IO ()
main = putStrLn "Hello Mu!"

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/health-check-unary/src/main/scala/higherkindness/mu/rpc/healthcheck/unary/handler/HealthServiceImpl.scala

type StatusMap = M.Map T.Text ServerStatus

server :: StatusMap -> ServerIO HealthCheckService _
server m = Server (setStatus m :<|>: checkH m :<|>: clearStatus m :<|>: checkAll m :<|>: cleanAll m :<|>: H0)

setStatus :: StatusMap -> HealthStatus -> IO ()
setStatus m (HealthStatus (HealthCheck nm) ss)
  = atomically $ M.insert ss nm m

checkH :: StatusMap -> HealthCheck -> IO ServerStatus
checkH m (HealthCheck nm)
  = do ss <- atomically $ M.lookup nm m
       return $ fromMaybe (ServerStatus "UNKNOWN") ss

clearStatus :: StatusMap -> HealthCheck -> IO ()
clearStatus m (HealthCheck nm)
  = atomically $ M.delete nm m

checkAll :: StatusMap -> IO AllStatus
checkAll m
  = AllStatus <$> atomically (consumeValues kvToStatus (M.unfoldlM m))
  where consumeValues :: Monad m => (k -> v -> a) -> UnfoldlM m (k,v) -> m [a]
        consumeValues f = foldlM' (\xs (x,y) -> pure (f x y:xs)) []
        kvToStatus k = HealthStatus (HealthCheck k)

cleanAll :: StatusMap -> IO ()
cleanAll m
  = atomically $ M.reset m

