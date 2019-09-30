{-# language OverloadedStrings, TypeApplications,
             NamedFieldPuns #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Proto3.Wire.Decode as PBDec
import qualified Proto3.Wire.Encode as PBEnc
import System.Environment

import Mu.Schema ()
import Mu.Schema.Adapter.ProtoBuf ()
import Mu.Schema.Examples

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
          let Right people = PBDec.parse protoBufToPerson cbs
          print people
          -- Encode a couple of values
          putStrLn "haskell/generate"
          print examplePerson1
          let gbs = PBEnc.toLazyByteString (personToProtoBuf examplePerson1)
          LBS.writeFile genFile gbs