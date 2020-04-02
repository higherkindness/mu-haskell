{-# language DataKinds          #-}
{-# language DerivingVia        #-}
{-# language OverloadedStrings  #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications   #-}
{-# options_ghc -fno-warn-orphans #-}
module Main where

import           Data.Avro
import qualified Data.ByteString.Lazy as BS
import           System.Environment

import           Mu.Adapter.Avro      ()
import           Mu.Schema            (WithSchema (..))
import           Mu.Schema.Examples

exampleAddress :: Address
exampleAddress = Address "1111BB" "Spain"

examplePerson1, examplePerson2 :: Person
examplePerson1 = Person "Haskellio" "Gomez" (Just 30) (Just Male) exampleAddress [1,2,3]
examplePerson2 = Person "Cuarenta" "Siete" Nothing Nothing exampleAddress []

deriving via (WithSchema ExampleSchema "person" Person) instance HasAvroSchema Person
deriving via (WithSchema ExampleSchema "person" Person) instance FromAvro Person
deriving via (WithSchema ExampleSchema "person" Person) instance ToAvro Person

main :: IO ()
main = do -- Obtain the filenames
  [genFile, conFile] <- getArgs
  -- Read the file produced by Python
  putStrLn "haskell/consume"
  cbs <- BS.readFile conFile
  let people = decodeContainer @Person cbs
  print people
  -- Encode a couple of values
  putStrLn "haskell/generate"
  print [examplePerson1, examplePerson2]
  gbs <- encodeContainer nullCodec [[examplePerson1, examplePerson2]]
  BS.writeFile genFile gbs
