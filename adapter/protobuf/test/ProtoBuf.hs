{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingVia           #-}
{-# language EmptyCase             #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language ScopedTypeVariables   #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
module Main where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as M
import qualified Data.Text            as T
import           GHC.Generics
import qualified Proto3.Wire.Decode   as PBDec
import qualified Proto3.Wire.Encode   as PBEnc
import           System.Environment

import           Data.Int
import           Mu.Adapter.ProtoBuf
import           Mu.Quasi.ProtoBuf
import           Mu.Schema

#if __GHCIDE__
protobuf "ExampleSchema" "adapter/protobuf/test/protobuf/example.proto"
#else
protobuf "ExampleSchema" "test/protobuf/example.proto"
#endif

data MGender = NB | Male | Female
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "gender", FromSchema ExampleSchema "gender")
  via CustomFieldMapping "gender"
        ["NB" ':-> "nb", "Male" ':-> "male", "Female" ':-> "female" ] MGender

data MPerson
  = MPerson { firstName     :: T.Text
            , lastName      :: T.Text
            , age           :: Int32
            , gender        :: MGender
            , address       :: Maybe MAddress
            , lucky_numbers :: [Int32]
            , things        :: M.Map T.Text Int32
            , foo           :: Maybe MFoo
            }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "person")
  deriving (FromSchema ExampleSchema "person")

newtype MFoo
  = MFoo { fooChoice :: Either Int32 T.Text }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "Foo")
  deriving (FromSchema ExampleSchema "Foo")

data MAddress
  = MAddress { postcode :: T.Text
             , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "address")
  deriving (FromSchema ExampleSchema "address")

exampleAddress :: Maybe MAddress
exampleAddress = Just $ MAddress "0000AA" "Nederland"

examplePerson1, examplePerson2 :: MPerson
examplePerson1 = MPerson "Pythonio" "van Gogh"
                         30 Male
                         exampleAddress [1,2,3]
                         (M.fromList [("hola", 1), ("hello", 2), ("hallo", 3)])
                         (Just $ MFoo $ Right "blah")
examplePerson2 = MPerson "Cuarenta" "Siete"
                         0 NB
                         exampleAddress [] M.empty
                         (Just $ MFoo $ Left 3)

main :: IO ()
main = do -- Obtain the filenames
  [genFile, conFile] <- getArgs
  -- Read the file produced by Python
  putStrLn "haskell/consume"
  cbs <- BS.readFile conFile
  let Right parsedPerson1 = PBDec.parse (fromProtoViaSchema @_ @_ @ExampleSchema) cbs
  if parsedPerson1 == examplePerson1
    then putStrLn $ "Parsed correctly as: \n" <> show parsedPerson1
    else putStrLn $ "Parsed person does not match expected person\n"
                    <> "Parsed person: \n" <> show parsedPerson1
                    <> "\nExpected person: \n" <> show examplePerson1
  -- Encode a couple of values
  putStrLn "haskell/generate"
  print examplePerson1
  let gbs = PBEnc.toLazyByteString (toProtoViaSchema @_ @_ @ExampleSchema examplePerson1)
  LBS.writeFile genFile gbs
