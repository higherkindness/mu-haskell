{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeFamilies        #-}
{-# language TypeOperators       #-}
module Mu.Kafka.Internal where

import qualified Data.Avro            as A
import           Data.ByteString
import           Data.ByteString.Lazy (fromStrict, toStrict)

import           Mu.Schema

toBS :: forall sch sty t.
        ( ToSchema sch sty t
        , A.ToAvro (Term sch (sch :/: sty))
        , A.HasAvroSchema (Term sch (sch :/: sty)) )
     => Proxy sch -> t -> ByteString
toBS _ = toStrict . A.encodeValue . toSchema @_ @_ @sch

fromBS :: forall sch sty t.
          ( FromSchema sch sty t
          , A.FromAvro (Term sch (sch :/: sty))
          , A.HasAvroSchema (Term sch (sch :/: sty)) )
       => Proxy sch -> ByteString -> Maybe t
fromBS _ x = fromSchema @_ @_ @sch @sty <$> resultToMaybe (A.decodeValue (fromStrict x))
  where
    resultToMaybe (Left  _) = Nothing
    resultToMaybe (Right y) = Just y
