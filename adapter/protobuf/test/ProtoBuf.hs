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
  = MPerson { firstName     :: T.Text
            , lastName      :: T.Text
            , age           :: Maybe Int
            , gender        :: Maybe Gender
            , address       :: MAddress
            , lucky_numbers :: [Int] }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "person")
  deriving (FromSchema ExampleSchema "person")

data MAddress
  = MAddress { postcode :: T.Text
             , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "address")
  deriving (FromSchema ExampleSchema "address")

type instance AnnotatedSchema ProtoBufAnnotation ExampleSchema
  = '[ 'AnnField "gender" "male"   ('ProtoBufId 1 'True)
     , 'AnnField "gender" "female" ('ProtoBufId 2 'True)
     , 'AnnField "gender" "nb"     ('ProtoBufId 3 'True)
     , 'AnnField "address" "postcode" ('ProtoBufId 1 'True)
     , 'AnnField "address" "country"  ('ProtoBufId 2 'True)
     , 'AnnField "person" "firstName" ('ProtoBufId 1 'True)
     , 'AnnField "person" "lastName"  ('ProtoBufId 2 'True)
     , 'AnnField "person" "age"       ('ProtoBufId 3 'True)
     , 'AnnField "person" "gender"    ('ProtoBufId 4 'True)
     , 'AnnField "person" "address"   ('ProtoBufId 5 'True)
     , 'AnnField "person" "lucky_numbers" ('ProtoBufId 6 'True) ]

exampleAddress :: MAddress
exampleAddress = MAddress "1111BB" "Spain"

examplePerson1, examplePerson2 :: MPerson
examplePerson1 = MPerson "Haskellio" "Gómez"
                         (Just 30) (Just Male)
                         exampleAddress [1,2,3]
examplePerson2 = MPerson "Cuarenta" "Siete"
                         Nothing Nothing
                         exampleAddress []

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
