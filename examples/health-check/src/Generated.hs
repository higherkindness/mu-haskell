{-# language TemplateHaskell, DataKinds,
             MultiParamTypeClasses, FlexibleInstances,
             TypeFamilies, DeriveGeneric #-}
module Generated where

import Mu.Schema.TH
import Mu.Client.GRpc.Record

import Definition

$(generateTypesFromSchema (++ "Msg") ''HealthCheckSchema)
$(generateRecordFromService "HealthCall" "" (++ "Msg") ''HealthCheckService)