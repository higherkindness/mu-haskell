{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module Schema where

import           Mu.Quasi.GRpc

#if __GHCIDE__
grpc "SeedSchema" id "examples/seed/protobuf/seed.proto"
#else
grpc "SeedSchema" id "seed.proto"
#endif
