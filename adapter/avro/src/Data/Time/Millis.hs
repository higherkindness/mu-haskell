{-# language GeneralizedNewtypeDeriving #-}
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

diffTimeToMillis :: DiffTimeMs -> Integer
diffTimeToMillis = (`div` 1000000000) . diffTimeToPicoseconds . unDiffTimeMs

millisToDiffTime :: Integer -> DiffTimeMs
millisToDiffTime = DiffTimeMs . picosecondsToDiffTime . (* 1000000000)
