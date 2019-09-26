{-# language PolyKinds, DataKinds, GADTs,
             TypeFamilies, TypeOperators, RecordWildCards,
             MultiParamTypeClasses, FlexibleInstances,
             TypeSynonymInstances, TypeApplications,
             DeriveGeneric, DerivingVia, DeriveAnyClass #-}
module Mu.Schema.Examples where

import qualified Data.Aeson as J
import qualified Data.Avro as A
import qualified Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Schema.Adapter.Avro ()
import Mu.Schema.Adapter.ProtoBuf
import Mu.Schema.Adapter.Json ()

import qualified Proto3.Wire.Encode as PBEnc

data Person
  = Person { firstName :: T.Text
           , lastName  :: T.Text 
           , age       :: Maybe Int
           , gender    :: Maybe Gender
           , address   :: Address }
  deriving (Eq, Show, Generic)
  deriving (HasSchema ExampleSchema "person")
  deriving (A.HasAvroSchema, A.FromAvro, A.ToAvro, J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "person" Person)

personToProtoBuf :: Person -> PBEnc.MessageBuilder
personToProtoBuf = toProtoViaSchema @ExampleSchema

data Address
  = Address { postcode :: T.Text
            , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (HasSchema ExampleSchema "address")
  deriving (A.HasAvroSchema, A.FromAvro, A.ToAvro, J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "address" Address)

data Gender = Male | Female | NonBinary
  deriving (Eq, Show, Generic)
  deriving (A.HasAvroSchema, A.FromAvro, A.ToAvro, J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "gender" Gender)

-- Schema for these data types
type ExampleSchema
  = '[ 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address") ]
     , 'DEnum   "gender" '["male", "female", "nb"]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     ]

-- we can give a custom field mapping via a custom instance
instance HasSchema ExampleSchema "gender" Gender where
  type FieldMapping ExampleSchema "gender" Gender
    = '[ "Male"      ':<->: "male"
       , "Female"    ':<->: "female"
       , "NonBinary" ':<->: "nb" ]

-- Additional information for protocol buffers
type instance ProtoBufFieldIds ExampleSchema "person"
  = '[ "firstName" ':<->: 1, "lastName" ':<->: 2
     , "age" ':<->: 3, "gender" ':<->: 4, "address" ':<->: 5]
type instance ProtoBufFieldIds ExampleSchema "gender"
  = '[ "male" ':<->: 1, "female" ':<->: 2, "nb" ':<->: 3]
type instance ProtoBufFieldIds ExampleSchema "address"
  = '[ "postcode" ':<->: 1, "country" ':<->: 2]