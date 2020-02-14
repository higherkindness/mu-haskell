{-# language CPP                   #-}
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
module Schema where

import           Mu.Quasi.Avro

#if __GHCIDE__
avdl "SeedSchema" "PeopleService" "examples/seed/avro" "seed.avdl"
#else
avdl "SeedSchema" "PeopleService" "." "seed.avdl"
#endif
