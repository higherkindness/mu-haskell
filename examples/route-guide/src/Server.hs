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
  runGRpcApp msgProtoBuf 8080 (server features routeNotes)

-- Utilities
-- https://github.com/higherkindness/mu/blob/master/modules/examples/routeguide/common/src/main/scala/Utils.scala

type Features = [Feature]

findFeatureIn :: Features -> Point -> Maybe Feature
findFeatureIn features p = find (\(Feature _ loc) -> loc == Just p) features

withinBounds :: Rectangle -> Point -> Bool
withinBounds (Rectangle (Just (Point (Just lox) (Just loy))) (Just (Point (Just hix) (Just hiy))))
             (Point (Just x) (Just y))
  = x >= lox && x <= hix && y >= loy && y <= hiy
withinBounds _ _
  = False

featuresWithinBounds :: Features -> Rectangle -> Features
featuresWithinBounds fs rect = filter (\(Feature _ loc) -> maybe False (withinBounds rect) loc) fs

calcDistance :: Point -> Point -> Maybe Int32
calcDistance (Point (Just lat1) (Just lon1)) (Point (Just lat2) (Just lon2))
  = let r = 6371000
        Radians (phi1 :: Double) = radians (Degrees (int32ToDouble lat1))
        Radians (phi2 :: Double) = radians (Degrees (int32ToDouble lat2))
        Radians (deltaPhi :: Double) = radians (Degrees (int32ToDouble $ lat2 - lat1))
        Radians (deltaLambda :: Double) = radians (Degrees (int32ToDouble $ lon2 - lon1))
        a = sin (deltaPhi / 2) * sin (deltaPhi / 2)
            + cos phi1 * cos phi2 * sin (deltaLambda / 2) * sin (deltaLambda / 2)
        c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    in Just (fromInteger $ r * ceiling c)
  where int32ToDouble :: Int32 -> Double
        int32ToDouble = fromInteger . toInteger
calcDistance _ _ = Nothing

-- Server implementation
-- https://github.com/higherkindness/mu/blob/master/modules/examples/routeguide/server/src/main/scala/handlers/RouteGuideServiceHandler.scala

server :: Features -> TBMChan RouteNote -> ServerIO Maybe RouteGuideService _
server f m = Server
  (getFeature f :<|>: listFeatures f  :<|>: recordRoute f :<|>: routeChat m :<|>: H0)

getFeature :: Features -> Point -> ServerErrorIO Feature
getFeature fs p = return $ fromMaybe nilFeature (findFeatureIn fs p)
  where nilFeature = Feature (Just "") (Just (Point (Just 0) (Just 0)))

listFeatures :: Features -> Rectangle
             -> ConduitT Feature Void ServerErrorIO ()
             -> ServerErrorIO ()
listFeatures fs rect result = runConduit $ sourceList (featuresWithinBounds fs rect) .| result

recordRoute :: Features
            -> ConduitT () Point ServerErrorIO ()
            -> ServerErrorIO RouteSummary
recordRoute fs ps = do
    initialTime <- liftIO getCurrentTime
    (\(rs, _, _) -> rs) <$> runConduit (ps .| C.foldM step (initial, Nothing, initialTime))
  where
    initial = RouteSummary (Just 0) (Just 0) (Just 0) (Just 0)
    step :: (RouteSummary, Maybe Point, UTCTime) -> Point
         -> ServerErrorIO (RouteSummary, Maybe Point, UTCTime)
    step (summary, previous, startTime) point = do
      currentTime <- liftIO getCurrentTime
      let feature = findFeatureIn fs point
          new_distance = case previous of
                           Nothing -> Just 0
                           Just d  -> d `calcDistance` point
          new_elapsed = diffUTCTime currentTime startTime
          update_feature_count = if isJust feature then 1 else 0
          new_summary = RouteSummary ((1 +) <$> point_count summary)
                                     ((update_feature_count +) <$> feature_count summary)
                                     ((+) <$> distance summary <*> new_distance)
                                     (Just $ floor new_elapsed)
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
                 readStmMap (\l1 (RouteNote _ l2)-> Just l1 == l2) toWatch notesMap .| outS
    res <- liftIO $ concurrently inA outA
    case res of
      (Right _, Right _) -> return ()
      (Left e, _)        -> serverError e
      (_, Left e)        -> serverError e
  where
    addNoteToMap :: TMVar Point -> RouteNote -> ServerErrorIO ()
    addNoteToMap toWatch newNote@(RouteNote _ (Just loc)) = liftIO $ atomically $ do
      _ <- tryTakeTMVar toWatch
      putTMVar toWatch loc
      writeTBMChan notesMap newNote
    addNoteToMap _toWatch _ = return ()

readStmMap :: (MonadIO m, Show b) => (a -> b -> Bool) -> TMVar a -> TBMChan b -> ConduitT () b m ()
readStmMap p toWatch m = go
  where
    go = do
      v <- liftIO $ atomically $ (,) <$> readTBMChan m <*> tryReadTMVar toWatch
      case v of
        (Nothing, _)                 -> return ()
        (Just v', Just e') | p e' v' -> liftIO (print v') >> yield v' >> go
        _                            -> go
