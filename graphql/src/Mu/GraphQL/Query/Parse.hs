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

import           Control.Applicative
import           Control.Monad
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
  selectMethod w@(GQL.unName -> wanted) args sels
    | wanted == mname = Z <$> (ChosenMethodQuery <$> parseArgs args <*> parseReturn sels)
    | otherwise = S <$> selectMethod w args sels
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
instance (ObjectOrEnumParser sch (sch :/: sty))
         => ParseArg p ('SchemaRef sch sty) where
  parseArg v = ArgSchema <$> parseObjectOrEnum v

class ObjectOrEnumParser sch (t :: TypeDef Symbol Symbol) where
  parseObjectOrEnum :: GQL.Value -> Maybe (Term Identity sch t)

instance (ObjectParser sch args)
         => ObjectOrEnumParser sch ('DRecord name args) where
  parseObjectOrEnum (GQL.VObject (GQL.ObjectValueG vs)) = TRecord <$> objectParser vs
  parseObjectOrEnum _                                   = Nothing

instance (EnumParser choices)
         => ObjectOrEnumParser sch ('DEnum name choices) where
  parseObjectOrEnum (GQL.VEnum (GQL.EnumValue nm)) = TEnum <$> enumParser nm
  parseObjectOrEnum _                              = Nothing

class ObjectParser sch args where
  objectParser :: [GQL.ObjectFieldG GQL.Value] -> Maybe (NP (Field Identity sch) args)

instance ObjectParser sch '[] where
  objectParser _ = pure Nil
instance
  (ObjectParser sch args, ValueParser sch v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser args = do
    GQL.ObjectFieldG _ v <- find ((== nameVal (Proxy @nm)) . T.unpack . GQL.unName . GQL._ofName) args
    (:*) <$> (Field . Identity <$> valueParser v) <*> objectParser args

class EnumParser (choices :: [ChoiceDef Symbol]) where
  enumParser :: GQL.Name -> Maybe (NS Proxy choices)

instance EnumParser '[] where
  enumParser _ = empty
instance (KnownName name, EnumParser choices)
         => EnumParser ('ChoiceDef name ': choices) where
  enumParser w@(GQL.unName -> wanted)
    | wanted == mname = pure (Z Proxy)
    | otherwise = S <$> enumParser w
    where
      mname = T.pack $ nameVal (Proxy @name)

class ValueParser sch v where
  valueParser :: GQL.Value -> Maybe (FieldValue Identity sch v)

instance ValueParser sch 'TNull where
  valueParser GQL.VNull = pure FNull
  valueParser _         = empty
instance ValueParser sch ('TPrimitive Bool) where
  valueParser (GQL.VBoolean b) = pure (FPrimitive b)
  valueParser _                = Nothing
instance ValueParser sch ('TPrimitive Int32) where
  valueParser (GQL.VInt b) = pure (FPrimitive b)
  valueParser _            = Nothing
instance ValueParser sch ('TPrimitive Integer) where
  valueParser (GQL.VInt b) = pure $ FPrimitive $ fromIntegral b
  valueParser _            = Nothing
instance ValueParser sch ('TPrimitive Double) where
  valueParser (GQL.VFloat b) = pure (FPrimitive b)
  valueParser _              = Nothing
instance ValueParser sch ('TPrimitive T.Text) where
  valueParser (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive b
  valueParser _                                 = Nothing
instance ValueParser sch ('TPrimitive String) where
  valueParser (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive $ T.unpack b
  valueParser _                                 = Nothing
instance (ValueParser sch r) => ValueParser sch ('TList r) where
  valueParser (GQL.VList (GQL.ListValueG xs)) = FList <$> traverse valueParser xs
  valueParser _                               = Nothing
instance (sch :/: sty ~ 'DRecord name args, ObjectParser sch args)
         => ValueParser sch ('TSchematic sty) where
  valueParser (GQL.VObject (GQL.ObjectValueG vs)) = FSchematic <$> (TRecord <$> objectParser vs)
  valueParser _                                   = Nothing

class ParseReturn (p :: Package') (r :: TypeRef Symbol) where
  parseReturn :: GQL.SelectionSet -> Maybe (ReturnQuery p r)

instance ParseReturn p ('PrimitiveRef t) where
  parseReturn s = guard (null s) >> pure RetPrimitive
instance ParseReturn p ('SchemaRef sch sty) where
  parseReturn _ = pure RetSchema
instance ParseReturn p r
         => ParseReturn p ('ListRef r) where
  parseReturn s = RetList <$> parseReturn s
instance ParseReturn p r
         => ParseReturn p ('OptionalRef r) where
  parseReturn s = RetOptional <$> parseReturn s
instance ( p ~ 'Package pname ss,
           LookupService ss s ~ 'Service sname sanns methods,
           ParseMethod p methods
         ) => ParseReturn p ('ObjectRef s) where
  parseReturn s = RetObject <$> parseQuery (Proxy @p) (Proxy @s) s
