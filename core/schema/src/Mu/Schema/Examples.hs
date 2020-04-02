{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DerivingVia           #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language QuasiQuotes           #-}
{-# language StandaloneDeriving    #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-|
Description : Examples for schema definitions.

Look at the source code of this module.
-}
module Mu.Schema.Examples where

import qualified Data.Aeson                         as J
import qualified Data.Text                          as T
import           GHC.Generics

import           Mu.Adapter.Json                    ()
import           Mu.Schema
import           Mu.Schema.Conversion.SchemaToTypes

data Person
  = Person { firstName     :: T.Text
           , lastName      :: T.Text
           , age           :: Maybe Int
           , gender        :: Maybe Gender
           , address       :: Address
           , lucky_numbers :: [Int] }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "person", FromSchema ExampleSchema "person")
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "person" Person)

data Address
  = Address { postcode :: T.Text
            , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "address", FromSchema ExampleSchema "address")
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "address" Address)

type GenderFieldMapping
  = '[ "Male"      ':-> "male"
     , "Female"    ':-> "female"
     , "NonBinary" ':-> "nb" ]

data Gender = Male | Female | NonBinary
  deriving (Eq, Show, Generic)
  deriving (ToSchema ExampleSchema "gender", FromSchema ExampleSchema "gender")
    via (CustomFieldMapping "gender" GenderFieldMapping Gender)
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "gender" Gender)

-- Schema for these data types
type ExampleSchema
  = '[ 'DEnum   "gender"
               '[ 'ChoiceDef "male"
                , 'ChoiceDef "female"
                , 'ChoiceDef "nb" ]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address")
                 , 'FieldDef "lucky_numbers" ('TList ('TPrimitive Int)) ]
     ]

$(generateTypesFromSchema (++"Msg") ''ExampleSchema)

{-
type ExampleSchema2
  = SchemaFromTypes '[ AsRecord Person "person"
                     , AsRecord Address "address"
                     , AsEnum Gender "gender" ]
-}
type ExampleSchema2
  = '[ 'DEnum   "gender"
               '[ 'ChoiceDef "Male"
                , 'ChoiceDef "Female"
                , 'ChoiceDef "NonBinary" ]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address") ]
     ]

type ExampleRegistry
  = '[ 2 ':-> ExampleSchema2, 1 ':-> ExampleSchema]
