{-# language GeneralizedNewtypeDeriving #-}
{-|
Description : Time differences in milliseconds

Avro defines a specific logical type for time
differences expessed in milliseconds. This module
provides a type which wraps the 'DiffTime' from
the @time@ library (which uses nanoseconds),
offering a millisecond-based interface.
-}
module Data.Time.Millis where

import           Control.DeepSeq             (NFData)
import           Data.Avro.Encoding.FromAvro
import           Data.Avro.Encoding.ToAvro
import           Data.Avro.HasAvroSchema
import qualified Data.Avro.Schema.Schema     as S
import           Data.Int                    (Int32)
import           Data.Tagged
import           Data.Time

-- | Wrapper for time difference expressed in milliseconds
newtype DiffTimeMs = DiffTimeMs { unDiffTimeMs :: DiffTime }
  deriving (Show, Eq, Ord, Enum, Num, Fractional, Real, RealFrac, NFData)

instance HasAvroSchema DiffTimeMs where
  schema = Tagged $ S.Int (Just S.TimeMillis)

instance ToAvro DiffTimeMs where
  toAvro s = toAvro s . (fromIntegral :: Integer -> Int32) . diffTimeToMillis

instance FromAvro DiffTimeMs where
  fromAvro (Int _ v) = pure $ millisToDiffTime (toInteger v)
  fromAvro _         = Left "expecting time_ms"

-- | Obtain the underlying time in milliseconds from a 'DiffTimeMs'.
diffTimeToMillis :: DiffTimeMs -> Integer
diffTimeToMillis = (`div` 1000000000) . diffTimeToPicoseconds . unDiffTimeMs

-- | Build a 'DiffTimeMs' from an amount expressed in milliseconds.
millisToDiffTime :: Integer -> DiffTimeMs
millisToDiffTime = DiffTimeMs . picosecondsToDiffTime . (* 1000000000)
