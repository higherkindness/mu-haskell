{-# language DeriveGeneric    #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies     #-}
{-|
Description : streams of Mu terms as Kafka producers

This module allows you to open a "sink" to Kafka.
Every value you sent to the sink will be sent over
to the corresponding Kafka instance.

This module is a wrapper over 'Kafka.Conduit.Sink'
from the (awesome) package @hw-kafka-client@.
-}
module Mu.Kafka.Producer (
  ProducerRecord'(..)
, kafkaSink
, kafkaSinkAutoClose
, kafkaSinkNoClose
, kafkaBatchSinkNoClose
, module X
) where

import           Conduit                      (mapC)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Avro                    as A
import           Data.ByteString
import           Data.Conduit
import           Data.Typeable
import           GHC.Generics
import           Mu.Schema

import qualified Kafka.Conduit.Sink           as S
import           Kafka.Producer               (ProducerRecord (..))

import           Kafka.Conduit.Combinators    as X
import           Kafka.Consumer               as X (KafkaConsumer)
import           Kafka.Producer               as X (KafkaError, KafkaProducer, ProducePartition,
                                                    ProducerProperties, TopicName)

import           Mu.Kafka.Internal

data ProducerRecord' k v = ProducerRecord'
  { prTopic     :: !TopicName
  , prPartition :: !ProducePartition
  , prKey       :: Maybe k
  , prValue     :: Maybe v
  } deriving (Eq, Show, Typeable, Generic)

toPR
  :: ( ToSchema sch sty t
     , A.ToAvro (WithSchema sch sty t)
     , A.HasAvroSchema (WithSchema sch sty t) )
  => Proxy sch -> ProducerRecord' ByteString t -> ProducerRecord
toPR proxy (ProducerRecord' t p k v)
  = ProducerRecord t p k (toBS proxy <$> v)

-- | Creates a kafka producer for given properties and returns a Sink.
--
-- This method of creating a Sink represents a simple case
-- and does not provide access to `KafkaProducer`. For more complex scenarious
-- 'kafkaSinkAutoClose' or 'kafkaSinkNoClose' can be used.
kafkaSink
  :: ( MonadResource m
     , ToSchema sch sty t
     , A.ToAvro (WithSchema sch sty t)
     , A.HasAvroSchema (WithSchema sch sty t) )
  => Proxy sch -> X.ProducerProperties
  -> ConduitT (ProducerRecord' ByteString t) Void m (Maybe KafkaError)
kafkaSink proxy prod
  = mapC (toPR proxy) .| S.kafkaSink prod

-- | Creates a Sink for a given `KafkaProducer`.
-- The producer will be closed when the Sink is closed.
kafkaSinkAutoClose
  :: ( MonadResource m
     , ToSchema sch sty t
     , A.ToAvro (WithSchema sch sty t)
     , A.HasAvroSchema (WithSchema sch sty t) )
  => Proxy sch -> KafkaProducer
  -> ConduitT (ProducerRecord' ByteString t) Void m (Maybe X.KafkaError)
kafkaSinkAutoClose proxy prod
  = mapC (toPR proxy) .| S.kafkaSinkAutoClose prod

-- | Creates a Sink for a given `KafkaProducer`.
-- The producer will NOT be closed automatically.
kafkaSinkNoClose
  :: ( MonadIO m
     , ToSchema sch sty t
     , A.ToAvro (WithSchema sch sty t)
     , A.HasAvroSchema (WithSchema sch sty t) )
  => Proxy sch -> KafkaProducer
  -> ConduitT (ProducerRecord' ByteString t) Void m (Maybe X.KafkaError)
kafkaSinkNoClose proxy prod
  = mapC (toPR proxy) .| S.kafkaSinkNoClose prod

-- | Creates a batching Sink for a given `KafkaProducer`.
-- The producer will NOT be closed automatically.
kafkaBatchSinkNoClose
  :: ( MonadIO m
     , ToSchema sch sty t
     , A.ToAvro (WithSchema sch sty t)
     , A.HasAvroSchema (WithSchema sch sty t) )
  => Proxy sch -> KafkaProducer
  -> ConduitT [ProducerRecord' ByteString t] Void m [(ProducerRecord, KafkaError)]
kafkaBatchSinkNoClose proxy prod
  = mapC (fmap (toPR proxy)) .| S.kafkaBatchSinkNoClose prod
