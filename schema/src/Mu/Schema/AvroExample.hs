{-# language DataKinds         #-}
{-# language QuasiQuotes       #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Mu.Schema.AvroExample where

import Mu.Schema.Quasi (avro, avroFile)

type Example = [avro|
[ { "type": "enum",
    "name": "gender",
    "symbols" : ["male", "female", "nb"]
  }
, { "type": "record"
  , "name": "address"
  , "fields": [
      {"name": "postcode", "type": "string"},
      {"name": "country", "type": "string"}
    ]
  }
,{ "type": "record",
    "name": "person",
    "fields": [
      {"name": "firstName", "type": "string"},
      {"name": "lastName", "type": "string"},
      {"name": "age",  "type": ["long", "null"]},
      {"name": "gender", "type": ["gender", "null"]},
      {"name": "address", "type": "address"}
    ]
  }
]
|]

type ExampleFromFile = [avroFile|schema/test/avro/example.avsc|]
