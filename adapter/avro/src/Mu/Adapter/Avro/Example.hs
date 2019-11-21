{-# language DataKinds   #-}
{-# language QuasiQuotes #-}

module Mu.Adapter.Avro.Example where

import           Mu.Quasi.Avro (avro, avroFile)

type Example = [avro|
{
  "type": "record",
  "name": "person",
  "fields": [
    { "name": "firstName", "type": "string" },
    { "name": "lastName", "type": "string" },
    { "name": "age", "type": ["long", "null"] },
    { "name": "gender", "type": [
        {
          "type": "enum",
          "name": "gender",
          "symbols": [ "male", "female", "nb"]
        },
        "null"
      ]
    },
    { "name": "address", "type": {
        "type": "record",
        "name": "address",
        "fields": [
          { "name": "postcode", "type": "string" },
          { "name": "country", "type": "string" }
        ]
      }
    }
  ]
}
|]

type ExampleFromFile = [avroFile|test/avro/example.avsc|]
