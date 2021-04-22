{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingStrategies    #-}
{-# language OverloadedStrings     #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE TypeOperators         #-}
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
import           Mu.Adapter.ProtoBuf
import           Mu.Quasi.ProtoBuf
import           Mu.Schema

protobuf "ExampleSchema" "test/protobuf/example.proto"

data MGender = NB | Male | Female
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "gender", FromSchema ExampleSchema "gender")
  via CustomFieldMapping "gender" ["NB" ':-> "nb", "Male" ':-> "male", "Female" ':-> "female" ] MGender

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

data MFoo
  = MFooInt Int32
  | MFooStr T.Text
  deriving (Eq, Show, Generic)

instance ToSchema ExampleSchema "Foo" MFoo where
  toSchema r =
    let protoChoice = case r of
                        MFooInt i -> Z (FPrimitive i)
                        MFooStr t -> S (Z (FPrimitive t))
    in TRecord (Field (FUnion protoChoice) :* Nil)

instance FromSchema ExampleSchema "Foo" MFoo where
  fromSchema (TRecord (Field (FUnion protoChoice) :* Nil)) =
    case protoChoice of
      Z (FPrimitive i) -> MFooInt i
      S (Z (FPrimitive t)) -> MFooStr t
      S (S x) -> case x of

data MAddress
  = MAddress { postcode :: T.Text
             , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "address")
  deriving (FromSchema ExampleSchema "address")

exampleAddress :: Maybe MAddress
exampleAddress = Just $ MAddress "1111BB" "Spain"

examplePerson1, examplePerson2 :: MPerson
examplePerson1 = MPerson "Haskellio" "Gómez"
                         30 Male
                         exampleAddress [1,2,3]
                         (M.fromList [("pepe", 1), ("juan", 2)])
                         (Just $ MFooInt 3)
examplePerson2 = MPerson "Cuarenta" "Siete"
                         0 NB
                         exampleAddress [] M.empty
                         (Just $ MFooStr "fasdfasdf")

main :: IO ()
main = do -- Obtain the filenames
  [genFile, conFile] <- getArgs
  -- Read the file produced by Python
  putStrLn "haskell/consume"
  cbs <- BS.readFile conFile
  let Right parsedPerson1 = PBDec.parse (fromProtoViaSchema @_ @_ @ExampleSchema) cbs
  if parsedPerson1 == examplePerson1
    then putStrLn $ "Parsed correctly as: \n" <> show parsedPerson1
    else putStrLn $ "Parsed person does not match expected person\nParsed person: \n" <> show parsedPerson <> "\nExpected person: " <> show examplePerson1
  -- Encode a couple of values
  putStrLn "haskell/generate"
  print examplePerson1
  let gbs = PBEnc.toLazyByteString (toProtoViaSchema @_ @_ @ExampleSchema examplePerson1)
  LBS.writeFile genFile gbs
