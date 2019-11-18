{-# language PolyKinds, DataKinds, GADTs,
             TypeFamilies, TypeOperators,
             MultiParamTypeClasses, FlexibleInstances,
             TypeApplications,
             DeriveGeneric, DerivingVia, DeriveAnyClass,
             TemplateHaskell, QuasiQuotes #-}
-- | Look at my source code!
module Mu.Schema.Examples where

import qualified Data.Aeson as J
import qualified Data.Text as T
import GHC.Generics

import Mu.Schema
import Mu.Schema.Conversion.SchemaToTypes
import Mu.Adapter.Json ()

data Person
  = Person { firstName :: T.Text
           , lastName  :: T.Text 
           , age       :: Maybe Int
           , gender    :: Maybe Gender
           , address   :: Address }
  deriving (Eq, Show, Generic)
  deriving (HasSchema ExampleSchema "person")
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "person" Person)

data Address
  = Address { postcode :: T.Text
            , country  :: T.Text }
  deriving (Eq, Show, Generic)
  deriving (HasSchema ExampleSchema "address")
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "address" Address)

data Gender = Male | Female | NonBinary
  deriving (Eq, Show, Generic)
  deriving (J.ToJSON, J.FromJSON)
    via (WithSchema ExampleSchema "gender" Gender)

-- Schema for these data types
type ExampleSchema
  = '[ 'DEnum   "gender" '[]
               '[ 'ChoiceDef "male"   '[ ProtoBufId 1 ]
                , 'ChoiceDef "female" '[ ProtoBufId 2 ]
                , 'ChoiceDef "nb"     '[ ProtoBufId 3 ] ]
     , 'DRecord "address" '[]
               '[ 'FieldDef "postcode" '[ ProtoBufId 1 ] ('TPrimitive T.Text)
                , 'FieldDef "country"  '[ ProtoBufId 2 ] ('TPrimitive T.Text) ]
     , 'DRecord "person" '[]
                '[ 'FieldDef "firstName" '[ ProtoBufId 1 ] ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  '[ ProtoBufId 2 ] ('TPrimitive T.Text)
                 , 'FieldDef "age"       '[ ProtoBufId 3 ] ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    '[ ProtoBufId 4 ] ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   '[ ProtoBufId 5 ] ('TSchematic "address") ]
     ]

type GenderFieldMapping
  = '[ "Male"      ':-> "male"
     , "Female"    ':-> "female"
     , "NonBinary" ':-> "nb" ]

-- we can give a custom field mapping via a custom instance
instance HasSchema ExampleSchema "gender" Gender where
  type FieldMapping ExampleSchema "gender" Gender = GenderFieldMapping

$(generateTypesFromSchema (++"Msg") ''ExampleSchema)

{-
type ExampleSchema2
  = SchemaFromTypes '[ AsRecord Person "person"
                     , AsRecord Address "address"
                     , AsEnum Gender "gender" ]
-}
type ExampleSchema2
  = '[ 'DEnum   "gender" '[]
               '[ 'ChoiceDef "Male"      '[ ProtoBufId 1 ]
                , 'ChoiceDef "Female"    '[ ProtoBufId 2 ]
                , 'ChoiceDef "NonBinary" '[ ProtoBufId 3 ] ]
     , 'DRecord "address" '[]
               '[ 'FieldDef "postcode" '[ ProtoBufId 1 ] ('TPrimitive T.Text)
                , 'FieldDef "country"  '[ ProtoBufId 2 ] ('TPrimitive T.Text) ]
     , 'DRecord "person" '[]
                '[ 'FieldDef "firstName" '[ ProtoBufId 1 ] ('TPrimitive T.Text)
                 , 'FieldDef "lastName"  '[ ProtoBufId 2 ] ('TPrimitive T.Text)
                 , 'FieldDef "age"       '[ ProtoBufId 3 ] ('TOption ('TPrimitive Int))
                 , 'FieldDef "gender"    '[ ProtoBufId 4 ] ('TOption ('TSchematic "gender"))
                 , 'FieldDef "address"   '[ ProtoBufId 5 ] ('TSchematic "address") ]
     ]

type ExampleRegistry
  = '[ 2 ':-> ExampleSchema2, 1 ':-> ExampleSchema]