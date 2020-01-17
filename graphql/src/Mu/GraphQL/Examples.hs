module Mu.GraphQL.Examples where

import Data.SOP

import Mu.GraphQL
import Mu.Schema.Examples

exampleResolver :: SchemaResolverD IO ExampleSchema
exampleResolver = ER_ :* RR_ addressResolver :* RR_ personResolver :* Nil
  where
    personResolver
      =  FR_ (return . firstName)
      :* FR_ (return . lastName)
      :* FR_ (return . age)
      :* FR_ (return . gender)
      :* FR_ (return . address)
      :* Nil
    addressResolver
      =  FR_ (return . postcode)
      :* FR_ (return . country)
      :* Nil
