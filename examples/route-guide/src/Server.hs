{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMChan
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Angle
import           Data.Conduit
import qualified Data.Conduit.Combinators       as C
import           Data.Conduit.Lift              (runExceptC)
import           Data.Conduit.List              (sourceList)
import           Data.Function                  ((&))
import           Data.Int
import           Data.List                      (find)
import           Data.Maybe
import           Data.Time.Clock

import           Mu.GRpc.Server
import           Mu.Server

import           Definition

main :: IO ()
main = do
  putStrLn "running route guide application"
  let features = []
  routeNotes <- newTBMChanIO 100
  runGRpcApp 8080 (server features routeNotes)

-- Utilities
-- https://github.com/higherkindness/mu/blob/master/modules/examples/routeguide/common/src/main/scala/Utils.scala

type Features = [Feature]

findFeatureIn :: Features -> Point -> Maybe Feature
findFeatureIn features p = find (\(Feature _ loc) -> loc == p) features

withinBounds :: Rectangle -> Point -> Bool
withinBounds (Rectangle (Point lox loy) (Point hix hiy)) (Point x y)
  = x >= lox && x <= hix && y >= loy && y <= hiy

featuresWithinBounds :: Features -> Rectangle -> Features
featuresWithinBounds fs rect = filter (\(Feature _ loc) -> withinBounds rect loc) fs

calcDistance :: Point -> Point -> Int32
calcDistance (Point lat1 lon1) (Point lat2 lon2)
  = let r = 6371000
        Radians (phi1 :: Double) = radians (Degrees (int32ToDouble lat1))
        Radians (phi2 :: Double) = radians (Degrees (int32ToDouble lat2))
        Radians (deltaPhi :: Double) = radians (Degrees (int32ToDouble $ lat2 - lat1))
        Radians (deltaLambda :: Double) = radians (Degrees (int32ToDouble $ lon2 - lon1))
        a = sin (deltaPhi / 2) * sin (deltaPhi / 2)
            + cos phi1 * cos phi2 * sin (deltaLambda / 2) * sin (deltaLambda / 2)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in fromInteger $ r * ceiling c
  where int32ToDouble :: Int32 -> Double
        int32ToDouble = fromInteger . toInteger

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/routeguide/server/src/main/scala/handlers/RouteGuideServiceHandler.scala

server :: Features -> TBMChan RouteNote -> ServerIO RouteGuideService _
server f m = Server
  (getFeature f :<|>: listFeatures f  :<|>: recordRoute f :<|>: routeChat m :<|>: H0)

getFeature :: Features -> Point -> ServerErrorIO Feature
getFeature fs p = return $ fromMaybe (Feature "" (Point 0 0)) (findFeatureIn fs p)

listFeatures :: Features -> Rectangle
             -> ConduitT Feature Void ServerErrorIO ()
             -> ServerErrorIO ()
listFeatures fs rect result = runConduit $ sourceList (featuresWithinBounds fs rect) .| result

recordRoute :: Features
            -> ConduitT () Point ServerErrorIO ()
            -> ServerErrorIO RouteSummary
recordRoute fs ps = do
    initialTime <- liftIO getCurrentTime
    (\(rs, _, _) -> rs) <$> runConduit (ps .| C.foldM step (RouteSummary 0 0 0 0, Nothing, initialTime))
  where
    step :: (RouteSummary, Maybe Point, UTCTime) -> Point
         -> ServerErrorIO (RouteSummary, Maybe Point, UTCTime)
    step (summary, previous, startTime) point = do
      currentTime <- liftIO getCurrentTime
      let feature = findFeatureIn fs point
          new_distance = fmap (`calcDistance` point) previous & fromMaybe 0
          new_elapsed = diffUTCTime currentTime startTime
          new_summary = RouteSummary (point_count summary + 1)
                                     (feature_count summary + if isJust feature then 1 else 0)
                                     (distance summary + new_distance)
                                     (floor new_elapsed)
      return (new_summary, Just point, startTime)

routeChat :: TBMChan RouteNote
          -> ConduitT () RouteNote ServerErrorIO ()
          -> ConduitT RouteNote Void ServerErrorIO ()
          -> ServerErrorIO ()
routeChat notesMap inS outS = do
    toWatch <- liftIO newEmptyTMVarIO
    -- Start two threads, one to listen, one to send
    let inA  = runConduit $ runExceptC $ inS .| C.mapM_ (addNoteToMap toWatch)
        outA = runConduit $ runExceptC $
                 readStmMap (\l1 (RouteNote _ l2)-> l1 == l2) toWatch notesMap .| outS
    res <- liftIO $ concurrently inA outA
    case res of
      (Right _, Right _) -> return ()
      (Left e, _)        -> serverError e
      (_, Left e)        -> serverError e
  where
    addNoteToMap :: TMVar Point -> RouteNote -> ServerErrorIO ()
    addNoteToMap toWatch newNote@(RouteNote _ loc) = liftIO $ atomically $ do
      _ <- tryTakeTMVar toWatch
      putTMVar toWatch loc
      writeTBMChan notesMap newNote

readStmMap :: (MonadIO m, Show b) => (a -> b -> Bool) -> TMVar a -> TBMChan b -> ConduitT () b m ()
readStmMap p toWatch m = go
  where
    go = do
      v <- liftIO $ atomically $ (,) <$> readTBMChan m <*> tryReadTMVar toWatch
      case v of
        (Nothing, _)                 -> return ()
        (Just v', Just e') | p e' v' -> liftIO (print v') >> yield v' >> go
        _                            -> go
