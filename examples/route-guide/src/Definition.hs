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

grpc "RouteGuideSchema" id "routeguide.proto"

data Point
  = Point { latitude, longitude :: Maybe Int32 }
  deriving ( Eq, Show, Ord, Generic, Hashable
           , ToSchema   Maybe RouteGuideSchema "Point"
           , FromSchema Maybe RouteGuideSchema "Point" )
data Rectangle
  = Rectangle { lo, hi :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe RouteGuideSchema "Rectangle"
           , FromSchema Maybe RouteGuideSchema "Rectangle" )
data Feature
  = Feature { name :: Maybe T.Text, location :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe RouteGuideSchema "Feature"
           , FromSchema Maybe RouteGuideSchema "Feature" )
-- Not used in the service
-- newtype FeatureDb
--   = FeatureDb { feature :: [Feature] }
--   deriving (Eq, Show, Ord, Generic, HasSchema RouteGuideSchema "FeatureDatabase")
data RouteNote
  = RouteNote { message :: Maybe T.Text, location :: Maybe Point }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe RouteGuideSchema "RouteNote"
           , FromSchema Maybe RouteGuideSchema "RouteNote" )
data RouteSummary
  = RouteSummary { point_count, feature_count, distance, elapsed_time :: Maybe Int32 }
  deriving ( Eq, Show, Ord, Generic
           , ToSchema   Maybe RouteGuideSchema "RouteSummary"
           , FromSchema Maybe RouteGuideSchema "RouteSummary" )

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
