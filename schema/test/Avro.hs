{-# language OverloadedStrings, TypeApplications #-}
module Main where

import Data.Avro
import qualified Data.ByteString.Lazy as BS
import System.Environment

import Mu.Schema ()
import Mu.Schema.Adapter.Avro ()
import Mu.Schema.Examples

exampleAddress :: Address
exampleAddress = Address "1111BB" "Spain"

examplePerson1, examplePerson2 :: Person
examplePerson1 = Person "Haskellio" "Gómez" (Just 30) (Just Male) exampleAddress
examplePerson2 = Person "Cuarenta" "Siete" Nothing Nothing exampleAddress

main :: IO ()
main = do -- Obtain the filenames
          [genFile, _conFile] <- getArgs
          -- Encode a couple of values
          bs <- encodeContainer [[examplePerson1, examplePerson2]]
          BS.writeFile genFile bs
          putStrLn "done!"