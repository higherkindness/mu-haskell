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
import Mu.Schema.Registry as R
import Mu.Schema.Adapter.Avro ()
import Mu.Schema.Adapter.ProtoBuf
import Mu.Schema.Adapter.Json ()

import Mu.Schema.FromTypes

import qualified Proto3.Wire.Encode as PBEnc
import qualified Proto3.Wire.Decode as PBDec

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

protoBufToPerson :: PBDec.Parser PBDec.RawMessage Person
protoBufToPerson = fromProtoViaSchema @ExampleSchema

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
  = '[ 'DEnum   "gender" '["male", "female", "nb"]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address") ]
     ]

type GenderFieldMapping
  = '[ "Male"      ':<->: "male"
     , "Female"    ':<->: "female"
     , "NonBinary" ':<->: "nb" ]

-- we can give a custom field mapping via a custom instance
instance HasSchema ExampleSchema "gender" Gender where
  type FieldMapping ExampleSchema "gender" Gender = GenderFieldMapping

-- Additional information for protocol buffers
type instance ProtoBufFieldIds ExampleSchema "person"
  = '[ "firstName" ':<->: 1, "lastName" ':<->: 2
     , "age" ':<->: 3, "gender" ':<->: 4, "address" ':<->: 5 ]
type instance ProtoBufFieldIds ExampleSchema "gender"
  = '[ "male" ':<->: 1, "female" ':<->: 2, "nb" ':<->: 0 ]
type instance ProtoBufFieldIds ExampleSchema "address"
  = '[ "postcode" ':<->: 1, "country" ':<->: 2 ]

{-
type ExampleSchema2
  = SchemaFromTypes '[ AsRecord Person "person"
                     , AsRecord Address "address"
                     , AsEnum Gender "gender" ]
-}
type ExampleSchema2
  = '[ 'DEnum   "gender" '["Male", "Female", "NonBinary"]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address") ]
     ]

type instance R.Registry "example"
  = '[ 2 ':<->: ExampleSchema2, 1 ':<->: ExampleSchema]