{-# language DataKinds #-}
{-# language PolyKinds #-}
module Mu.GraphQL.Annotations (
  GQL.ValueConst(..)
, DefaultValue
) where

import qualified Language.GraphQL.Draft.Syntax as GQL

-- | Specifies the default value of an argument.
--   To be used as an annotation.
data DefaultValue (v :: GQL.ValueConst)
