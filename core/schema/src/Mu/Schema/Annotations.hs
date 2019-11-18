{-# language DataKinds, KindSignatures #-}
module Mu.Schema.Annotations where

import GHC.TypeLits

-- ANNOTATION FOR CONVERSION

data ProtoBufId (n :: Nat)
data ProtoBufOneOfIds (ns :: [Nat])