{-# language DataKinds                  #-}
{-# language DerivingStrategies         #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures             #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeApplications           #-}
module Data.Avro.Decimal where

import qualified Data.Avro             as A
import qualified Data.Avro.Schema      as ASch
import qualified Data.Avro.Types.Value as AVal
import           Data.BigDecimal
import qualified Data.Binary           as B
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import           Data.Proxy
import           Data.Tagged
import           GHC.TypeLits

newtype Decimal (scale :: Nat)
  = Decimal BigDecimal
  deriving newtype (Eq, Ord, Show, Read, Num, Real)

underlyingValue
  :: forall s. KnownNat s
  => Decimal s -> Integer
underlyingValue (Decimal d)
  = let rounded = roundBD d (halfUp scale)
    in case (getValue rounded, getScale rounded) of
         (v, s)
           | s == scale -> v
           | otherwise  -> v * 10 ^ (scale - s)
  where scale = natVal (Proxy @s)

instance A.HasAvroSchema (Decimal s) where
  schema = Tagged ASch.Bytes
instance KnownNat s => A.ToAvro (Decimal s) where
  toAvro = AVal.Bytes . toStrict . B.encode . underlyingValue
instance KnownNat s => A.FromAvro (Decimal s) where
  fromAvro (AVal.Bytes b)
    = let v = B.decode (fromStrict b)
          s = natVal (Proxy @s)
      in return $ Decimal (BigDecimal v s)
  fromAvro v = A.badValue v "decimal"
