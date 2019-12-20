{-# language CPP             #-}
{-# language DataKinds       #-}
{-# language TemplateHaskell #-}
{-# language TypeFamilies    #-}
module Mu.Adapter.ProtoBuf.Example where

import           Mu.Quasi.ProtoBuf

#if __GHCIDE__
protobuf "ExampleProtoBufSchema"  "adapter/protobuf/test/protobuf/example.proto"
protobuf "Example2ProtoBufSchema" "adapter/protobuf/test/protobuf/example2.proto"
#else
protobuf "ExampleProtoBufSchema"  "test/protobuf/example.proto"
protobuf "Example2ProtoBufSchema" "test/protobuf/example2.proto"
#endif
