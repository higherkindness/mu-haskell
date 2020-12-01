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
import qualified Data.Map                           as M
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
           , lucky_numbers :: [Int]
           , things        :: M.Map T.Text Int }
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
  = '[ "Male"        ':-> "male"
     , "Female"      ':-> "female"
     , "NonBinary"   ':-> "nb"
     , "Gender0"     ':-> "gender0"
     , "Gender1"     ':-> "gender1"
     , "Gender2"     ':-> "gender2"
     , "Gender3"     ':-> "gender3"
     , "Gender4"     ':-> "gender4"
     , "Gender5"     ':-> "gender5"
     , "Gender6"     ':-> "gender6"
     , "Gender7"     ':-> "gender7"
     , "Gender8"     ':-> "gender8"
     , "Gender9"     ':-> "gender9"
     , "Unspecified" ':-> "unspecified"]

data Gender
  = Male
  | Female
  | NonBinary
  | Gender0
  | Gender1
  | Gender2
  | Gender3
  | Gender4
  | Gender5
  | Gender6
  | Gender7
  | Gender8
  | Gender9
  | Unspecified
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
                , 'ChoiceDef "nb"
                , 'ChoiceDef "gender0"
                , 'ChoiceDef "gender1"
                , 'ChoiceDef "gender2"
                , 'ChoiceDef "gender3"
                , 'ChoiceDef "gender4"
                , 'ChoiceDef "gender5"
                , 'ChoiceDef "gender6"
                , 'ChoiceDef "gender7"
                , 'ChoiceDef "gender8"
                , 'ChoiceDef "gender9"
                , 'ChoiceDef "unspecified" ]
     , 'DRecord "address"
               '[ 'FieldDef "postcode" ('TPrimitive T.Text)
                , 'FieldDef "country"  ('TPrimitive T.Text) ]
     , 'DRecord "person"
                '[ 'FieldDef "firstName" ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  ('TPrimitive T.Text)
                 , 'FieldDef "age"       ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   ('TSchematic "address")
                 , 'FieldDef "lucky_numbers" ('TList ('TPrimitive Int))
                 , 'FieldDef "things"    ('TMap ('TPrimitive T.Text) ('TPrimitive Int)) ]
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
