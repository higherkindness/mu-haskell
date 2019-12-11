{-# language DataKinds           #-}
{-# language DeriveAnyClass      #-}
{-# language DeriveGeneric       #-}
{-# language DerivingStrategies  #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeFamilies        #-}
module Main where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           GHC.Generics
import qualified Proto3.Wire.Decode   as PBDec
import qualified Proto3.Wire.Encode   as PBEnc
import           System.Environment

import           Mu.Adapter.ProtoBuf
import           Mu.Schema
import           Mu.Schema.Examples

data MPerson
  = MPerson { firstName :: Maybe T.Text
            , lastName  :: Maybe T.Text
            , age       :: Maybe (Maybe Int)
            , gender    :: Maybe (Maybe Gender)
            , address   :: Maybe MAddress }
  deriving (Eq, Show, Generic)
  deriving (HasSchema Maybe ExampleSchema "person")

data MAddress
  = MAddress { postcode :: Maybe T.Text
             , country  :: Maybe T.Text }
  deriving (Eq, Show, Generic)
  deriving (HasSchema Maybe ExampleSchema "address")

type instance AnnotatedSchema ProtoBufAnnotation ExampleSchema
  = '[ 'AnnField "gender" "male"   ('ProtoBufId 1)
     , 'AnnField "gender" "female" ('ProtoBufId 2)
     , 'AnnField "gender" "nb"     ('ProtoBufId 3)
     , 'AnnField "address" "postcode" ('ProtoBufId 1)
     , 'AnnField "address" "country"  ('ProtoBufId 2)
     , 'AnnField "person" "firstName" ('ProtoBufId 1)
     , 'AnnField "person" "lastName"  ('ProtoBufId 2)
     , 'AnnField "person" "age"       ('ProtoBufId 3)
     , 'AnnField "person" "gender"    ('ProtoBufId 4)
     , 'AnnField "person" "address"   ('ProtoBufId 5) ]

exampleAddress :: MAddress
exampleAddress = MAddress (Just "1111BB") (Just "Spain")

examplePerson1, examplePerson2 :: MPerson
examplePerson1 = MPerson (Just "Haskellio") (Just "Gómez")
                         (Just $ Just 30) (Just $ Just Male)
                         (Just exampleAddress)
examplePerson2 = MPerson (Just "Cuarenta") (Just "Siete")
                         (Just Nothing) (Just Nothing)
                         (Just exampleAddress)

main :: IO ()
main = do -- Obtain the filenames
  [genFile, conFile] <- getArgs
  -- Read the file produced by Python
  putStrLn "haskell/consume"
  cbs <- BS.readFile conFile
  let Right people = PBDec.parse (fromProtoViaSchema @_ @_ @ExampleSchema) cbs
  print (people :: MPerson)
  -- Encode a couple of values
  putStrLn "haskell/generate"
  print examplePerson1
  let gbs = PBEnc.toLazyByteString (toProtoViaSchema @_ @_ @ExampleSchema examplePerson1)
  LBS.writeFile genFile gbs
