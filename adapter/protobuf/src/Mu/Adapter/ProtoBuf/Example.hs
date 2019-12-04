{-# language DataKinds       #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}
module Mu.Adapter.ProtoBuf.Example where

import           Mu.Quasi.ProtoBuf

protobuf "ExampleProtoBufSchema"  "test/protobuf/example.proto"
protobuf "Example2ProtoBufSchema" "test/protobuf/example2.proto"
