{-# language PolyKinds, DataKinds, TypeOperators,
             MultiParamTypeClasses, TypeFamilies,
             FlexibleInstances, FlexibleContexts,
             DeriveGeneric, DeriveAnyClass,
             DuplicateRecordFields, TemplateHaskell #-}
module Definition where

import GHC.Generics
import Data.Hashable
import Data.Int
import Data.Text as T

import Mu.Schema
import Mu.Quasi.GRpc

$(grpc "RouteGuideSchema" id "routeguide.proto")

data Point
  = Point { latitude, longitude :: Int32 }
  deriving (Eq, Show, Ord, Generic, Hashable, HasSchema RouteGuideSchema "Point")
data Rectangle
  = Rectangle { lo, hi :: Point }
  deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "Rectangle")
data Feature
  = Feature { name :: T.Text, location :: Point }
  deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "Feature")
-- Not used in the service
-- newtype FeatureDb
--   = FeatureDb { feature :: [Feature] }
--   deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "FeatureDatabase")
data RouteNote
  = RouteNote { message :: T.Text, location :: Point }
  deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "RouteNote")
data RouteSummary
  = RouteSummary { point_count, feature_count, distance, elapsed_time :: Int32 }
  deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "RouteSummary")

{-
type RG = 'FromSchema RouteGuideSchema
type RouteGuideService
  = 'Service "RouteGuideService" '[Package "routeguide"]
      '[ 'Method "GetFeature"   '[] '[ 'ArgSingle (RG "Point") ] ('RetSingle (RG "Feature"))
       , 'Method "ListFeatures" '[] '[ 'ArgSingle (RG "Rectangle") ] ('RetStream (RG "Feature"))
       , 'Method "RecordRoute"  '[] '[ 'ArgStream (RG "Point") ] ('RetSingle (RG "RouteSummary"))
       , 'Method "RouteChat"    '[] '[ 'ArgStream (RG "RouteNote") ] ('RetStream (RG "RouteNote"))
       ]

type RouteGuideSchema
  = '[ 'DRecord "Point" '[]
                '[ 'FieldDef "latitude"  '[ProtoBufId 1] ('TPrimitive Int32)
                 , 'FieldDef "longitude" '[ProtoBufId 2] ('TPrimitive Int32) ]
     , 'DRecord "Rectangle" '[]
                '[ 'FieldDef "lo" '[ProtoBufId 1] ('TSchematic "Point")
                 , 'FieldDef "hi" '[ProtoBufId 2] ('TSchematic "Point") ]
     , 'DRecord "Feature" '[]
                '[ 'FieldDef "name"     '[ProtoBufId 1] ('TPrimitive T.Text)
                 , 'FieldDef "location" '[ProtoBufId 2] ('TSchematic "Point") ]
     , 'DRecord "FeatureDatabase" '[]
                '[ 'FieldDef "feature" '[ProtoBufId 1] ('TList ('TSchematic "Feature")) ]
     , 'DRecord "RouteNote" '[]
                '[ 'FieldDef "message"  '[ProtoBufId 2] ('TPrimitive T.Text)
                 , 'FieldDef "location" '[ProtoBufId 1] ('TSchematic "Point") ]
     , 'DRecord "RouteSummary" '[]
                '[ 'FieldDef "point_count"   '[ProtoBufId 1] ('TPrimitive Int32)
                 , 'FieldDef "feature_count" '[ProtoBufId 2] ('TPrimitive Int32)
                 , 'FieldDef "distance"      '[ProtoBufId 3] ('TPrimitive Int32)
                 , 'FieldDef "elapsed_time"  '[ProtoBufId 4] ('TPrimitive Int32) ]
     ]
-}