{-# language GeneralizedNewtypeDeriving #-}
module Data.Time.Millis where


import           Control.DeepSeq         (NFData)
import           Data.Avro.Encode
import           Data.Avro.EncodeRaw
import           Data.Avro.FromAvro
import           Data.Avro.HasAvroSchema
import           Data.Avro.Schema        as S
import           Data.Avro.ToAvro
import           Data.Avro.Types         as T
import           Data.Tagged
import           Data.Time
import           Unsafe.Coerce

-- | Wrapper for time difference expressed in milliseconds
newtype DiffTimeMs = DiffTimeMs { unDiffTimeMs :: DiffTime }
  deriving (Show, Eq, Ord, Enum, Num, Fractional, Real, RealFrac, NFData)

instance EncodeAvro DiffTimeMs where
  avro d
      -- Unfortunately, the AvroM constructor is not exposed :(
    = unsafeCoerce ( encodeRaw (fromIntegral $ diffTimeToMillis d :: Int)
                   , S.Long (Just TimeMicros) )

instance HasAvroSchema DiffTimeMs where
  schema = Tagged $ S.Int (Just TimeMillis)

instance ToAvro DiffTimeMs where
  toAvro = T.Int . fromIntegral . diffTimeToMillis

instance FromAvro DiffTimeMs where
  fromAvro (T.Int  v) = pure $ millisToDiffTime (toInteger v)
  fromAvro v          = badValue v "TimeMicros"

diffTimeToMillis :: DiffTimeMs -> Integer
diffTimeToMillis = (`div` 1000000000) . diffTimeToPicoseconds . unDiffTimeMs

millisToDiffTime :: Integer -> DiffTimeMs
millisToDiffTime = DiffTimeMs . picosecondsToDiffTime . (* 1000000000)
