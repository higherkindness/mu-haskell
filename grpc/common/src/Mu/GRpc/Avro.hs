{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language KindSignatures        #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# options_ghc -fno-warn-orphans  #-}
module Mu.GRpc.Avro where

import           Data.Avro
import           Data.Binary.Builder         (fromByteString, putWord32be, singleton)
import           Data.Binary.Get             (getByteString, getInt8, getWord32be,
                                              runGetIncremental, Decoder(..))
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as ByteString
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Functor.Identity
import           Network.GRPC.HTTP2.Encoding
import           Network.GRPC.HTTP2.Types

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Monoid                 ((<>))
#endif

import           Mu.Rpc
import           Mu.Schema

-- | A proxy type for giving static information about RPCs.
data AvroRPC = AvroRPC { pkg :: ByteString, srv :: ByteString, meth :: ByteString }

instance IsRPC AvroRPC where
  path rpc = "/" <> pkg rpc <> "." <> srv rpc <> "/" <> meth rpc
  {-# INLINE path #-}

newtype ViaAvroTypeRef (ref :: TypeRef) t
  = ViaAvroTypeRef { unViaAvroTypeRef :: t }

instance GRPCInput AvroRPC () where
  encodeInput = error "you should not call this"
  decodeInput _ _ = runGetIncremental $ pure $ Right ()

instance GRPCOutput AvroRPC () where
  encodeOutput _ _ = mempty
  decodeOutput = error "you should not call this"

instance (FromSchema Identity sch sty i, FromAvro (Term Identity sch (sch :/: sty)))
         => GRPCInput AvroRPC (ViaAvroTypeRef ('ViaSchema sch sty) i) where
  encodeInput = error "you should not call this"
  decodeInput _ i = (ViaAvroTypeRef . fromSchema' @_ @_ @sch @Identity <$>) <$> decoder i

instance (ToSchema Identity sch sty o, ToAvro (Term Identity sch (sch :/: sty)))
         => GRPCOutput AvroRPC (ViaAvroTypeRef ('ViaSchema sch sty) o) where
  encodeOutput _ compression
    = encoder compression . toSchema' @_ @_ @sch @Identity . unViaAvroTypeRef
  decodeOutput = error "you should not call this"

encoder :: ToAvro m => Compression -> m -> Builder
encoder compression plain =
    mconcat [ singleton (if _compressionByteSet compression then 1 else 0)
            , putWord32be (fromIntegral $ ByteString.length bin)
            , fromByteString bin
            ]
  where
    bin = _compressionFunction compression $ toStrict $ encode plain

decoder :: FromAvro a => Compression -> Decoder (Either String a)
decoder compression = runGetIncremental $ do
    isCompressed <- getInt8      -- 1byte
    let decompress = if isCompressed == 0 then pure else _decompressionFunction compression
    n <- getWord32be             -- 4bytes
    decode' . fromStrict <$> (decompress =<< getByteString (fromIntegral n))
  where
    decode' x = case decode x of
                  Success y -> Right y
                  Error   e -> Left e

-- Based on https://hackage.haskell.org/package/binary/docs/Data-Binary-Get-Internal.html
instance Functor Decoder where
  fmap f (Done b s a) = Done b s (f a)
  fmap f (Partial k) = Partial (fmap f . k)
  fmap _ (Fail b s msg) = Fail b s msg
