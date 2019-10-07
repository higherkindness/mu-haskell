{-# language PolyKinds, DataKinds, TypeFamilies,
             TypeOperators,
             UndecidableInstances #-}
module Mu.Schema.FromTypes (
  FromTypes, FromType(..)
, AsRecord, AsEnum
, SchemaFromTypes
) where

import Data.Kind
import Data.Map as M
import Data.SOP
import GHC.Generics
import GHC.TypeLits

import Mu.Schema.Definition

type FromTypes = [FromType Symbol Symbol]
data FromType tn fn
  = AsRecord' Type tn (Mappings Symbol fn)
  | AsEnum'   Type tn (Mappings Symbol fn)

type AsRecord t tn = 'AsRecord' t tn '[]
type AsEnum   t tn = 'AsEnum'   t tn '[]

type family SchemaFromTypes (f :: [FromType tn fn]) :: Schema tn fn where
  SchemaFromTypes f = SchemaFromTypes' f f

type family SchemaFromTypes' (all :: [FromType tn fn]) (f :: [FromType tn fn]) :: Schema tn fn where
  SchemaFromTypes' all '[] = '[]
  SchemaFromTypes' all (t ': ts) = TypeDefFromType all t ': SchemaFromTypes' all ts

type family TypeDefFromType (all :: [FromType tn fn]) (info :: FromType tn fn)
  :: TypeDef tn fn where
  TypeDefFromType all ('AsRecord' t name mp) = 'DRecord name (FieldsFromType  all mp (Rep t))
  TypeDefFromType all ('AsEnum'   t name mp) = 'DEnum   name (ChoicesFromType all mp (Rep t))

type family FieldsFromType (all :: [FromType tn fn]) (mp :: Mappings Symbol fn) (f :: * -> *)
  :: [FieldDef tn fn] where
  FieldsFromType all mp (x :+: y)
    = TypeError ('Text "sum types cannot be converted to record schemas")
  FieldsFromType all mp (D1 meta f)
    = FieldsFromType all mp f  -- go through data info
  FieldsFromType all mp (C1 meta f)
    = FieldsFromType all mp f  -- go through constructor info
  FieldsFromType all mp (x :*: y)
    = ConcatList (FieldsFromType all mp x) (FieldsFromType all mp y)
  FieldsFromType all mp (S1 ('MetaSel ('Just x) u ss ds) (K1 i t))
    = '[ 'FieldDef (MappingRight mp x) (ChooseFieldType all t) ]
  FieldsFromType all mp v
    = TypeError ('Text "unsupported conversion from " ':<>: 'ShowType v ':<>: 'Text " to record schema")

type family ConcatList (xs :: [k]) (ys :: [k]) :: [k] where
  ConcatList '[]       ys = ys
  ConcatList (x ': xs) ys = x ': (ConcatList xs ys)

type family ChooseFieldType (all :: [FromType tn fn]) (t :: Type)
  :: FieldType tn where
  ChooseFieldType all () = 'TNull
  ChooseFieldType all (Maybe t) = 'TOption (ChooseFieldType all t)
  ChooseFieldType all [t] = 'TList (ChooseFieldType all t)
  ChooseFieldType all (M.Map k v) = 'TMap (ChooseFieldType all k) (ChooseFieldType all v)
  ChooseFieldType all (NS I choices) = 'TUnion (ChooseFieldUnion all choices)
  ChooseFieldType all t = ChooseFieldPrimitiveOrSchematic t (FindTypeName all t)

type family ChooseFieldUnion (all :: [FromType tn fn]) (t :: [Type])
  :: [FieldType tn] where
  ChooseFieldType all '[] = '[]
  ChooseFieldType all (t ': ts) = ChooseFieldType all t ': ChooseFieldUnion all ts

type family FindTypeName (all :: [FromType tn fn]) (t :: Type)
  :: Maybe tn where
  FindTypeName '[] t = 'Nothing
  FindTypeName ('AsRecord' t tn mp ': rest) t = 'Just tn
  FindTypeName ('AsEnum'   t tn mp ': rest) t = 'Just tn
  FindTypeName (other ': rest) t = FindTypeName rest t

type family ChooseFieldPrimitiveOrSchematic (t :: Type) (ref :: Maybe tn)
  :: FieldType tn where
  ChooseFieldPrimitiveOrSchematic t ('Just name) = 'TSchematic name
  ChooseFieldPrimitiveOrSchematic t 'Nothing     = 'TPrimitive t

type family ChoicesFromType (all :: [FromType tn fn]) (mp :: Mappings Symbol fn) (f :: * -> *)
  :: [fn] where
  ChoicesFromType all mp (D1 meta f)
    = ChoicesFromType all mp f  -- go through data info
  ChoicesFromType all mp (x :+: y)
    = ConcatList (ChoicesFromType all mp x) (ChoicesFromType all mp y)
  ChoicesFromType all mp (C1 ('MetaCons cname p s) U1)
    = '[ MappingRight mp cname ]  -- go through constructor info
  ChoicesFromType all mp (C1 ('MetaCons cname p s) f)
    = TypeError ('Text "constructor " ':<>: 'ShowType cname ':<>: 'Text "has fields and cannot be turned into an enumeration schema")
  ChoicesFromType all mp v
    = TypeError ('Text "unsupported conversion from " ':<>: 'ShowType v ':<>: 'Text " to enumeration schema")