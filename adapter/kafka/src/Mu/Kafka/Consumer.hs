{-# language FlexibleContexts #-}
{-# language TypeFamilies     #-}
{-# language TypeOperators    #-}
{-|
Description : Kafka consumers as streams of Mu terms

This module allows you to receive values from
a Kafka topic, and treat then as Mu terms, or
your Haskell types if a conversion exists.

This module is a wrapper over 'Kafka.Conduit.Source'
from the (awesome) package @hw-kafka-client@.
-}
module Mu.Kafka.Consumer (
  kafkaSource
, kafkaSourceNoClose
, kafkaSourceAutoClose
, module X
) where

import           Conduit                      (mapC)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.Avro                    as A
import           Data.ByteString
import           Data.Conduit
import           Mu.Schema

import qualified Kafka.Conduit.Source         as S

import           Kafka.Conduit.Combinators    as X
import           Kafka.Consumer               as X

import           Mu.Kafka.Internal

fromCR
  :: ( FromSchema sch sty t
     , A.FromAvro (Term sch (sch :/: sty)) )
  => Proxy sch
  -> ConsumerRecord (Maybe ByteString) (Maybe ByteString)
  -> ConsumerRecord (Maybe ByteString) (Maybe t)
fromCR proxy (ConsumerRecord t p o ts k v)
  = ConsumerRecord t p o ts k (v >>= fromBS proxy)

-- | Creates a kafka producer for given properties and returns a `Source`.
--
-- This method of creating a `Source` represents a simple case
-- and does not provide access to `KafkaProducer`. For more complex scenarious
-- 'kafkaSinkNoClose' or 'kafkaSinkAutoClose' can be used.
kafkaSource
  :: ( MonadResource m
     , FromSchema sch sty t
     , A.FromAvro (Term sch (sch :/: sty)) )
  => Proxy sch
  -> ConsumerProperties -> Subscription -> Timeout
  -> ConduitT () (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe t))) m ()
kafkaSource proxy props sub ts =
  S.kafkaSource props sub ts .| mapC (fmap (fromCR proxy))

-- | Create a `Source` for a given `KafkaConsumer`.
-- The consumer will NOT be closed automatically when the `Source` is closed.
kafkaSourceNoClose
  :: ( MonadIO m
     , FromSchema sch sty t
     , A.FromAvro (Term sch (sch :/: sty)) )
  => Proxy sch
  -> KafkaConsumer -> Timeout
  -> ConduitT () (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe t))) m ()
kafkaSourceNoClose proxy c t
  = S.kafkaSourceNoClose c t .| mapC (fmap (fromCR proxy))


-- | Create a `Source` for a given `KafkaConsumer`.
-- The consumer will be closed automatically when the `Source` is closed.
kafkaSourceAutoClose
  :: ( MonadResource m
     , FromSchema sch sty t
     , A.FromAvro (Term sch (sch :/: sty)) )
  => Proxy sch
  -> KafkaConsumer -> Timeout
  -> ConduitT () (Either KafkaError (ConsumerRecord (Maybe ByteString) (Maybe t))) m ()
kafkaSourceAutoClose proxy c t
  = S.kafkaSourceAutoClose c t .| mapC (fmap (fromCR proxy))
