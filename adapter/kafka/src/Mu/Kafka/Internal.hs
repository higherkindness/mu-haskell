{-# language FlexibleContexts    #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeFamilies        #-}
module Mu.Kafka.Internal where

import qualified Data.Avro            as A
import           Data.ByteString
import           Data.ByteString.Lazy (fromStrict, toStrict)

import           Mu.Schema

toBS :: forall sch sty t.
        ( ToSchema sch sty t
        , A.HasAvroSchema (WithSchema sch sty t)
        , A.ToAvro (WithSchema sch sty t) )
     => Proxy sch -> t -> ByteString
toBS _ = toStrict . A.encodeValue . WithSchema @_ @_ @sch @sty @t

fromBS :: forall sch sty t.
          ( FromSchema sch sty t
          , A.FromAvro (WithSchema sch sty t)
          , A.HasAvroSchema (WithSchema sch sty t) )
       => Proxy sch -> ByteString -> Maybe t
fromBS _ x = unWithSchema @_ @_ @sch @sty @t <$> resultToMaybe (A.decodeValue (fromStrict x))
  where
    resultToMaybe (Left  _) = Nothing
    resultToMaybe (Right y) = Just y
