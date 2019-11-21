{-# language DataKinds   #-}
{-# language QuasiQuotes #-}
module Mu.Adapter.ProtoBuf.Example where

import           Mu.Quasi.ProtoBuf

type ExampleProtoBufSchema = [protobuf|
enum gender {
  male      = 1;
  female    = 2;
  nonbinary = 3;
}
message person {
  repeated string names = 1;
  int age = 2;
  gender gender = 3;
}
|]
