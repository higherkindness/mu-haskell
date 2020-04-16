{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# options_ghc -fno-warn-orphans -fno-warn-simplifiable-class-constraints  #-}
{-|
Description : (Internal) Wrappers for Avro serialization

Intended for internal use.

This module provides the required instances of
the common type classes from 'Mu.GRpc.Bridge'
to make it work with Avro.
-}
module Mu.GRpc.Avro (
  AvroRPC(..)
, ViaFromAvroTypeRef(..)
, ViaToAvroTypeRef(..)
) where

import           Data.Avro
import           Data.Binary.Builder         (fromByteString, putWord32be, singleton)
import           Data.Binary.Get             (Decoder (..), getByteString, getInt8, getWord32be,
                                              runGetIncremental)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as ByteString
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Kind
import           GHC.TypeLits
import           Network.GRPC.HTTP2.Encoding
import           Network.GRPC.HTTP2.Types

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Monoid                 ((<>))
#endif

import           Mu.Adapter.Avro             ()
import           Mu.Rpc
import           Mu.Schema

-- | A proxy type for giving static information about RPCs.
--   Intended for internal use.
data AvroRPC = AvroRPC { pkg :: ByteString, srv :: ByteString, meth :: ByteString }

instance IsRPC AvroRPC where
  path rpc = "/" <> pkg rpc <> "." <> srv rpc <> "/" <> meth rpc
  {-# INLINE path #-}

-- | Wrapper used to tag a type with its corresponding
--   'TypeRef' used for deserialization from Avro.
--   Intended for internal use.
newtype ViaFromAvroTypeRef (ref :: TypeRef snm) t
  = ViaFromAvroTypeRef { unViaFromAvroTypeRef :: t }
-- | Wrapper used to tag a type with its corresponding
--   'TypeRef' used for serialization to Avro.
--   Intended for internal use.
newtype ViaToAvroTypeRef (ref :: TypeRef snm) t
  = ViaToAvroTypeRef { unViaToAvroTypeRef :: t }

instance GRPCInput AvroRPC () where
  encodeInput _ _ () = mempty
  decodeInput _ _ = runGetIncremental $ pure $ Right ()

instance GRPCOutput AvroRPC () where
  encodeOutput _ _ () = mempty
  decodeOutput _ _ = runGetIncremental $ pure $ Right ()

instance forall (sch :: Schema') (sty :: Symbol) (i :: Type).
         ( HasAvroSchema (WithSchema sch sty i)
         , FromAvro (WithSchema sch sty i) )
         => GRPCInput AvroRPC (ViaFromAvroTypeRef ('SchemaRef sch sty) i) where
  encodeInput = error "eif/you should not call this"
  decodeInput _ i = (ViaFromAvroTypeRef . unWithSchema @_ @_ @sch @sty @i <$>) <$> decoder i

instance forall (sch :: Schema') (sty :: Symbol) (i :: Type).
         ( HasAvroSchema (WithSchema sch sty i)
         , FromAvro (WithSchema sch sty i) )
         => GRPCOutput AvroRPC (ViaFromAvroTypeRef ('SchemaRef sch sty) i) where
  encodeOutput = error "eof/you should not call this"
  decodeOutput _ i = (ViaFromAvroTypeRef . unWithSchema @_ @_ @sch @sty @i <$>) <$> decoder i

instance forall (sch :: Schema') (sty :: Symbol) (o :: Type).
         ( HasAvroSchema (WithSchema sch sty o)
         , ToAvro (WithSchema sch sty o) )
         => GRPCInput AvroRPC (ViaToAvroTypeRef ('SchemaRef sch sty) o) where
  encodeInput _ compression
    = encoder compression . WithSchema @_ @_ @sch @sty . unViaToAvroTypeRef
  decodeInput = error "dit/you should not call this"

instance forall (sch :: Schema') (sty :: Symbol) (o :: Type).
         ( HasAvroSchema (WithSchema sch sty o)
         , ToAvro (WithSchema sch sty o) )
         => GRPCOutput AvroRPC (ViaToAvroTypeRef ('SchemaRef sch sty) o) where
  encodeOutput _ compression
    = encoder compression . WithSchema @_ @_ @sch @sty . unViaToAvroTypeRef
  decodeOutput = error "dot/you should not call this"

encoder :: (HasAvroSchema m, ToAvro m)
        => Compression -> m -> Builder
encoder compression plain =
    mconcat [ singleton (if _compressionByteSet compression then 1 else 0)
            , putWord32be (fromIntegral $ ByteString.length bin)
            , fromByteString bin
            ]
  where
    bin = _compressionFunction compression $ toStrict $ encodeValue plain

decoder :: (HasAvroSchema a, FromAvro a)
        => Compression -> Decoder (Either String a)
decoder compression = runGetIncremental $ do
    isCompressed <- getInt8      -- 1byte
    let decompress = if isCompressed == 0 then pure else _decompressionFunction compression
    n <- getWord32be             -- 4bytes
    decodeValue . fromStrict <$> (decompress =<< getByteString (fromIntegral n))

-- Based on https://hackage.haskell.org/package/binary/docs/Data-Binary-Get-Internal.html
instance Functor Decoder where
  fmap f (Done b s a)   = Done b s (f a)
  fmap f (Partial k)    = Partial (fmap f . k)
  fmap _ (Fail b s msg) = Fail b s msg
