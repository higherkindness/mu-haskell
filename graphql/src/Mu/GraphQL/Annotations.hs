{-# language DataKinds           #-}
{-# language FlexibleInstances   #-}
{-# language PolyKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language TupleSections       #-}
{-# language TypeApplications    #-}
{-# language TypeOperators       #-}
{-|
Description : Annotations for GraphQL services

GraphQL schemas may contain some information which
cannot be directly represented in a Mu schema or
service definition. The types in this module
can be used with the annotation mechanism in Mu
to provide this additional information.
-}
module Mu.GraphQL.Annotations (
  ValueConst(..)
, DefaultValue(..)
, ReflectValueConst(..)
, fromGQLValueConst
, module Mu.Rpc.Annotations
) where

import           Control.Applicative  (Alternative (..))
import           Data.Proxy
import qualified Data.Text            as T
import           GHC.TypeLits
import qualified Language.GraphQL.AST as GQL

import           Mu.Rpc.Annotations

-- | Specifies the default value of an argument.
--   To be used as an annotation.
newtype DefaultValue
  = DefaultValue (ValueConst Nat Symbol)

-- | Type-level GraphQL constant values.
--   Due to limitations in type-level literal values
--   floating point constants cannot be represented.
data ValueConst nat symbol
  = VCInt nat        -- ^ Integer.
  | VCString symbol  -- ^ String.
  | VCBoolean Bool   -- ^ Boolean.
  | VCNull           -- ^ Null.
  | VCEnum symbol    -- ^ Enumeration value.
  | VCList [ValueConst nat symbol]  -- ^ List of constant values.
  | VCObject [(symbol, ValueConst nat symbol)]
      -- ^ Object represented by (key, value) tuples.

-- | Turn a 'GQL.ValueConst' coming from parsing
--   in the annotation data type. Mostly used
--   internally to generate Mu schemas from GraphQL schemas.
fromGQLValueConst :: forall f. Alternative f
                  => GQL.ConstValue -> f (ValueConst Integer String)
fromGQLValueConst (GQL.ConstInt n)
  = pure $ VCInt (fromIntegral n)
fromGQLValueConst (GQL.ConstString s)
  = pure $ VCString $ T.unpack s
fromGQLValueConst (GQL.ConstBoolean b)
  = pure $ VCBoolean b
fromGQLValueConst GQL.ConstNull
  = pure VCNull
fromGQLValueConst (GQL.ConstEnum s)
  = pure $ VCEnum $ T.unpack s
fromGQLValueConst (GQL.ConstList xs)
  = VCList <$> traverse (fromGQLValueConst . GQL.node) xs
fromGQLValueConst (GQL.ConstObject o)
  = VCObject <$> traverse fromGQLField o
  where fromGQLField :: GQL.ObjectField GQL.ConstValue
                     -> f (String, ValueConst Integer String)
        fromGQLField (GQL.ObjectField n (GQL.Node v _) _)
          = (T.unpack n,) <$> fromGQLValueConst v
fromGQLValueConst _ = empty

-- | Obtain the GraphQL constant corresponding
--   to a type-level constant. Inhabited by any
--   'ValueConst', but still required to please
--   the type checker.
class ReflectValueConst (v :: ValueConst nat symbol) where
  -- | Obtain the GraphQL constant corresponding
  --   to a type-level constant.
  reflectValueConst :: proxy v -> GQL.ConstValue
instance KnownNat n => ReflectValueConst ('VCInt n) where
  reflectValueConst _ = GQL.ConstInt $ fromInteger $ natVal (Proxy @n)
instance KnownSymbol s => ReflectValueConst ('VCString s) where
  reflectValueConst _ = GQL.ConstString $ T.pack $ symbolVal (Proxy @s)
instance ReflectValueConst ('VCBoolean 'True) where
  reflectValueConst _ = GQL.ConstBoolean True
instance ReflectValueConst ('VCBoolean 'False) where
  reflectValueConst _ = GQL.ConstBoolean False
instance ReflectValueConst 'VCNull where
  reflectValueConst _ = GQL.ConstNull
instance KnownSymbol e => ReflectValueConst ('VCEnum e) where
  reflectValueConst _ = GQL.ConstString $ T.pack $ symbolVal (Proxy @e)
instance ReflectValueConstList xs => ReflectValueConst ('VCList xs) where
  reflectValueConst _ = GQL.ConstList $
    map (`GQL.Node` GQL.Location 0 0) $ reflectValueConstList (Proxy @xs)
instance ReflectValueConstObject xs => ReflectValueConst ('VCObject xs) where
  reflectValueConst _ = GQL.ConstObject $ reflectValueConstObject (Proxy @xs)

class ReflectValueConstList xs where
  reflectValueConstList :: proxy xs -> [GQL.ConstValue]
instance ReflectValueConstList '[] where
  reflectValueConstList _ = []
instance (ReflectValueConst x, ReflectValueConstList xs)
         => ReflectValueConstList (x ': xs) where
  reflectValueConstList _
    = reflectValueConst (Proxy @x) : reflectValueConstList (Proxy @xs)

class ReflectValueConstObject xs where
  reflectValueConstObject :: proxy xs -> [GQL.ObjectField GQL.ConstValue]
instance ReflectValueConstObject '[] where
  reflectValueConstObject _ = []
instance (KnownSymbol a, ReflectValueConst x, ReflectValueConstObject xs)
         => ReflectValueConstObject ( '(a, x) ': xs) where
  reflectValueConstObject _
    = GQL.ObjectField (T.pack $ symbolVal (Proxy @a))
                      (GQL.Node (reflectValueConst (Proxy @x)) zl)
                      zl
      : reflectValueConstObject (Proxy @xs)
    where zl = GQL.Location 0 0
