{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language ViewPatterns          #-}

module Mu.GraphQL.Query.Parse where

import           Data.Functor.Identity
import           Data.Int                      (Int32)
import           Data.List                     (find)
import           Data.Proxy
import           Data.SOP.NS
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Mu.GraphQL.Query.Definition
import           Mu.Rpc
import           Mu.Schema

-- TODO: turn Hasura's `ExecutableDefinition` into a service query
-- Hint: start with the following function, and then move up
-- (OperationDefinition -> ExecutableDefinition -> ExecutableDocument)
parseQuery ::
  forall (p :: Package') (s :: Symbol) pname ss sname sanns methods.
  ( p ~ 'Package pname ss,
    LookupService ss s ~ 'Service sname sanns methods,
    ParseMethod p methods
  ) =>
  Proxy p ->
  Proxy s ->
  GQL.SelectionSet ->
  Maybe (ServiceQuery p (LookupService ss s))
parseQuery _ _ = traverse toOneMethod
  where
    toOneMethod :: GQL.Selection -> Maybe (OneMethodQuery p ('Service sname sanns methods))
    toOneMethod (GQL.SelectionField fld)        = fieldToMethod fld
    toOneMethod (GQL.SelectionFragmentSpread _) = Nothing -- FIXME:
    toOneMethod (GQL.SelectionInlineFragment _) = Nothing -- FIXME:
    fieldToMethod :: GQL.Field -> Maybe (OneMethodQuery p ('Service sname sanns methods))
    fieldToMethod (GQL.Field alias name args _ sels) =
      OneMethodQuery (GQL.unName . GQL.unAlias <$> alias) <$> selectMethod name args sels

class ParseMethod (p :: Package') (ms :: [Method Symbol Symbol]) where
  selectMethod ::
    GQL.Name ->
    [GQL.Argument] ->
    GQL.SelectionSet ->
    Maybe (NS (ChosenMethodQuery p) ms)

instance ParseMethod p '[] where
  selectMethod _ _ _ = Nothing

instance
  (KnownSymbol mname, ParseMethod p ms, ParseArgs p args, ParseReturn p r) =>
  ParseMethod p ('Method mname manns args ('RetSingle r) ': ms)
  where
  selectMethod (GQL.unName -> wanted) args sels
    | wanted == mname = Z <$> (ChosenMethodQuery <$> parseArgs args <*> parseReturn sels)
    | otherwise = S <$> selectMethod (GQL.Name wanted) args sels
    where
      mname = T.pack $ nameVal (Proxy @mname)

class ParseArgs (p :: Package') (args :: [Argument Symbol]) where
  parseArgs :: [GQL.Argument] -> Maybe (NP (ArgumentValue p) args)

instance ParseArgs p '[] where
  parseArgs _ = pure Nil

instance (ParseArg p a, ParseArgs p as) => ParseArgs p ('ArgSingle a ': as) where
  parseArgs (GQL.Argument _ x : xs) = (:*) <$> (ArgumentValue <$> parseArg x) <*> parseArgs xs
  parseArgs _                       = Nothing

class ParseArg (p :: Package') (a :: TypeRef Symbol) where
  parseArg :: GQL.Value -> Maybe (ArgumentValue' p a)

instance (ParseArg p r) => ParseArg p ('ListRef r) where
  parseArg (GQL.VList (GQL.ListValueG xs)) = ArgList <$> traverse parseArg xs
  parseArg _                               = Nothing

instance ParseArg p ('PrimitiveRef Bool) where
  parseArg (GQL.VBoolean b) = pure (ArgPrimitive b)
  parseArg _                = Nothing

instance ParseArg p ('PrimitiveRef Int32) where
  parseArg (GQL.VInt b) = pure (ArgPrimitive b)
  parseArg _            = Nothing

instance ParseArg p ('PrimitiveRef Integer) where
  parseArg (GQL.VInt b) = pure $ ArgPrimitive $ fromIntegral b
  parseArg _            = Nothing

instance ParseArg p ('PrimitiveRef Double) where
  parseArg (GQL.VFloat b) = pure (ArgPrimitive b)
  parseArg _              = Nothing

instance ParseArg p ('PrimitiveRef T.Text) where
  parseArg (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive b
  parseArg _                                 = Nothing

instance ParseArg p ('PrimitiveRef String) where
  parseArg (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive $ T.unpack b
  parseArg _                                 = Nothing

instance ParseArg p ('PrimitiveRef ()) where
  parseArg GQL.VNull = pure $ ArgPrimitive ()
  parseArg _         = Nothing

instance ParseArg p ('SchemaRef sch sty) where
  parseArg (GQL.VObject (GQL.ObjectValueG _)) = undefined -- TODO: ArgSchema <$> (TRecord <$> objectParser vs)
  parseArg _                                  = Nothing

class ObjectParser sch args where
  objectParser :: [GQL.ObjectFieldG GQL.Value] -> Maybe (NP (Field Identity sch) args)

instance ObjectParser sch '[] where
  objectParser _ = pure Nil

instance
  (ObjectParser sch args, ValueParser v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser args = do
    GQL.ObjectFieldG _ v <- find ((== nameVal (Proxy @nm)) . T.unpack . GQL.unName . GQL._ofName) args
    (:*) <$> valueParser v <*> objectParser args

class ValueParser v where
  valueParser :: GQL.Value -> Maybe (Field Identity sch ('FieldDef nm v))

-- TODO: define instances for ValueParser!

-- newtype ListValueG a
--   = ListValueG {unListValue :: [a]}

-- data Value
--   = VVariable !Variable
--   | VInt !Int32
--   | VFloat !Double
--   | VString !StringValue
--   | VBoolean !Bool
--   | VNull
--   | VEnum !EnumValue
--   | VList !ListValue
--   | VObject !ObjectValue
--   deriving (Ord, Show, Eq, Lift, Generic)

-- newtype ObjectValueG a
--   = ObjectValueG {unObjectValue :: [ObjectFieldG a]}
--   deriving (Ord, Show, Eq, Lift, Hashable)
-- type ObjectValue = ObjectValueG Value
-- type ObjectValueC = ObjectValueG ValueConst
-- data ObjectFieldG a
--   = ObjectFieldG
--   { _ofName  :: Name
--   , _ofValue :: a
--   } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

-- data Argument
--   = Argument
--   { _aName  :: !Name
--   , _aValue :: !Value
--   } deriving (Ord, Show, Eq, Lift, Generic)

class ParseReturn (p :: Package') (r :: TypeRef Symbol) where
  parseReturn :: GQL.SelectionSet -> Maybe (ReturnQuery p r)
