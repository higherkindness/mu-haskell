{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
module Definition where

import           Data.Hashable
import           Data.Int
import           Data.Text     as T
import           GHC.Generics

import           Mu.Quasi.GRpc
import           Mu.Schema

#if __GHCIDE__
grpc "RouteGuideSchema" id "examples/route-guide/routeguide.proto"
#else
grpc "RouteGuideSchema" id "routeguide.proto"
#endif

data Point
  = Point { latitude, longitude :: Int32 }
  deriving ( Eq, Show, Ord, Generic, Hashable
           , ToSchema   RouteGuideSchema "Point"
           , FromSchema RouteGuideSchema "Point" )
data Rectangle
  = Rectangle { lo, hi :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   RouteGuideSchema "Rectangle"
           , FromSchema RouteGuideSchema "Rectangle" )
data Feature
  = Feature { name :: T.Text, location :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   RouteGuideSchema "Feature"
           , FromSchema RouteGuideSchema "Feature" )
-- Not used in the service
-- newtype FeatureDb
--   = FeatureDb { feature :: [Feature] }
--   deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "FeatureDatabase")
data RouteNote
  = RouteNote { message :: T.Text, location :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   RouteGuideSchema "RouteNote"
           , FromSchema RouteGuideSchema "RouteNote" )
data RouteSummary
  = RouteSummary { point_count, feature_count, distance, elapsed_time :: Int32 }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   RouteGuideSchema "RouteSummary"
           , FromSchema RouteGuideSchema "RouteSummary" )
