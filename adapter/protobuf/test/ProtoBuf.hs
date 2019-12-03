{-# language DataKinds           #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications    #-}
{-# language TypeFamilies        #-}
module Main where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Proto3.Wire.Decode   as PBDec
import qualified Proto3.Wire.Encode   as PBEnc
import           System.Environment

import           Mu.Adapter.ProtoBuf
import           Mu.Schema
import           Mu.Schema.Examples

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

exampleAddress :: Address
exampleAddress = Address "1111BB" "Spain"

examplePerson1, examplePerson2 :: Person
examplePerson1 = Person "Haskellio" "Gómez" (Just 30) (Just Male) exampleAddress
examplePerson2 = Person "Cuarenta" "Siete" Nothing Nothing exampleAddress

main :: IO ()
main = do -- Obtain the filenames
  [genFile, conFile] <- getArgs
  -- Read the file produced by Python
  putStrLn "haskell/consume"
  cbs <- BS.readFile conFile
  let Right people = PBDec.parse (fromProtoViaSchema @_ @_ @ExampleSchema) cbs
  print (people :: Person)
  -- Encode a couple of values
  putStrLn "haskell/generate"
  print examplePerson1
  let gbs = PBEnc.toLazyByteString (toProtoViaSchema @_ @_ @ExampleSchema examplePerson1)
  LBS.writeFile genFile gbs
