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

parseDoc ::
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    ParseMethod p mmethods
  ) =>
  GQL.ExecutableDocument ->
  f (Document p qr mut)
parseDoc (GQL.ExecutableDocument defns)
  = case GQL.partitionExDefs defns of
      ([unnamed], [], _) -> QueryDoc <$> parseQuery Proxy Proxy unnamed
      ([], [named], _)   -> parseTypedDoc named
      _                  -> empty

parseTypedDoc ::
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    ParseMethod p mmethods
  ) =>
  GQL.TypedOperationDefinition ->
  f (Document p qr mut)
parseTypedDoc tod@GQL.TypedOperationDefinition { GQL._todType = GQL.OperationTypeQuery }
  = QueryDoc <$> parseQuery Proxy Proxy (GQL._todSelectionSet tod)
parseTypedDoc tod@GQL.TypedOperationDefinition { GQL._todType = GQL.OperationTypeMutation }
  = MutationDoc <$> parseQuery Proxy Proxy (GQL._todSelectionSet tod)
parseTypedDoc _ = empty

-- TODO: turn Hasura's `ExecutableDefinition` into a service query
-- Hint: start with the following function, and then move up
-- (OperationDefinition -> ExecutableDefinition -> ExecutableDocument)
parseQuery ::
  forall (p :: Package') (s :: Symbol) pname ss sanns methods f.
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss s ~ 'Service s sanns methods,
    ParseMethod p methods
  ) =>
  Proxy p ->
  Proxy s ->
  GQL.SelectionSet ->
  f (ServiceQuery p (LookupService ss s))
parseQuery _ _ = traverse toOneMethod
  where
    toOneMethod :: GQL.Selection -> f (OneMethodQuery p ('Service sname sanns methods))
    toOneMethod (GQL.SelectionField fld)        = fieldToMethod fld
    toOneMethod (GQL.SelectionFragmentSpread _) = empty -- FIXME:
    toOneMethod (GQL.SelectionInlineFragment _) = empty -- FIXME:
    fieldToMethod :: GQL.Field -> f (OneMethodQuery p ('Service sname sanns methods))
    fieldToMethod (GQL.Field alias name args _ sels) =
      OneMethodQuery (GQL.unName . GQL.unAlias <$> alias) <$> selectMethod name args sels

class ParseMethod (p :: Package') (ms :: [Method Symbol Symbol]) where
  selectMethod ::
    Alternative f =>
    GQL.Name ->
    [GQL.Argument] ->
    GQL.SelectionSet ->
    f (NS (ChosenMethodQuery p) ms)

instance ParseMethod p '[] where
  selectMethod _ _ _ = empty
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
  parseArgs :: Alternative f => [GQL.Argument] -> f (NP (ArgumentValue p) args)

instance ParseArgs p '[] where
  parseArgs _ = pure Nil
instance (ParseArg p a, ParseArgs p as) => ParseArgs p ('ArgSingle a ': as) where
  parseArgs (GQL.Argument _ x : xs) = (:*) <$> (ArgumentValue <$> parseArg x) <*> parseArgs xs
  parseArgs _                       = empty

class ParseArg (p :: Package') (a :: TypeRef Symbol) where
  parseArg :: Alternative f => GQL.Value -> f (ArgumentValue' p a)

instance (ParseArg p r) => ParseArg p ('ListRef r) where
  parseArg (GQL.VList (GQL.ListValueG xs)) = ArgList <$> traverse parseArg xs
  parseArg _                               = empty
instance ParseArg p ('PrimitiveRef Bool) where
  parseArg (GQL.VBoolean b) = pure (ArgPrimitive b)
  parseArg _                = empty
instance ParseArg p ('PrimitiveRef Int32) where
  parseArg (GQL.VInt b) = pure (ArgPrimitive b)
  parseArg _            = empty
instance ParseArg p ('PrimitiveRef Integer) where
  parseArg (GQL.VInt b) = pure $ ArgPrimitive $ fromIntegral b
  parseArg _            = empty
instance ParseArg p ('PrimitiveRef Double) where
  parseArg (GQL.VFloat b) = pure (ArgPrimitive b)
  parseArg _              = empty
instance ParseArg p ('PrimitiveRef T.Text) where
  parseArg (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive b
  parseArg _                                 = empty
instance ParseArg p ('PrimitiveRef String) where
  parseArg (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive $ T.unpack b
  parseArg _                                 = empty
instance ParseArg p ('PrimitiveRef ()) where
  parseArg GQL.VNull = pure $ ArgPrimitive ()
  parseArg _         = empty
instance (ObjectOrEnumParser sch (sch :/: sty))
         => ParseArg p ('SchemaRef sch sty) where
  parseArg v = ArgSchema <$> parseObjectOrEnum v

class ObjectOrEnumParser sch (t :: TypeDef Symbol Symbol) where
  parseObjectOrEnum :: Alternative f
                    => GQL.Value
                    -> f (Term Identity sch t)

instance (ObjectParser sch args)
         => ObjectOrEnumParser sch ('DRecord name args) where
  parseObjectOrEnum (GQL.VObject (GQL.ObjectValueG vs)) = TRecord <$> objectParser vs
  parseObjectOrEnum _                                   = empty
instance (EnumParser choices)
         => ObjectOrEnumParser sch ('DEnum name choices) where
  parseObjectOrEnum (GQL.VEnum (GQL.EnumValue nm)) = TEnum <$> enumParser nm
  parseObjectOrEnum _                              = empty

class ObjectParser sch args where
  objectParser :: Alternative f
               => [GQL.ObjectFieldG GQL.Value]
               -> f (NP (Field Identity sch) args)

instance ObjectParser sch '[] where
  objectParser _ = pure Nil
instance
  (ObjectParser sch args, ValueParser sch v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser args
    = case find ((== nameVal (Proxy @nm)) . T.unpack . GQL.unName . GQL._ofName) args of
        Just (GQL.ObjectFieldG _ v)
          -> (:*) <$> (Field . Identity <$> valueParser v) <*> objectParser args
        Nothing -> empty

class EnumParser (choices :: [ChoiceDef Symbol]) where
  enumParser :: Alternative f => GQL.Name -> f (NS Proxy choices)

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
  valueParser :: Alternative f
              => GQL.Value
              -> f (FieldValue Identity sch v)

instance ValueParser sch 'TNull where
  valueParser GQL.VNull = pure FNull
  valueParser _         = empty
instance ValueParser sch ('TPrimitive Bool) where
  valueParser (GQL.VBoolean b) = pure (FPrimitive b)
  valueParser _                = empty
instance ValueParser sch ('TPrimitive Int32) where
  valueParser (GQL.VInt b) = pure (FPrimitive b)
  valueParser _            = empty
instance ValueParser sch ('TPrimitive Integer) where
  valueParser (GQL.VInt b) = pure $ FPrimitive $ fromIntegral b
  valueParser _            = empty
instance ValueParser sch ('TPrimitive Double) where
  valueParser (GQL.VFloat b) = pure (FPrimitive b)
  valueParser _              = empty
instance ValueParser sch ('TPrimitive T.Text) where
  valueParser (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive b
  valueParser _                                 = empty
instance ValueParser sch ('TPrimitive String) where
  valueParser (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive $ T.unpack b
  valueParser _                                 = empty
instance (ValueParser sch r) => ValueParser sch ('TList r) where
  valueParser (GQL.VList (GQL.ListValueG xs)) = FList <$> traverse valueParser xs
  valueParser _                               = empty
instance (sch :/: sty ~ 'DRecord name args, ObjectParser sch args)
         => ValueParser sch ('TSchematic sty) where
  valueParser (GQL.VObject (GQL.ObjectValueG vs)) = FSchematic <$> (TRecord <$> objectParser vs)
  valueParser _                                   = empty

class ParseReturn (p :: Package') (r :: TypeRef Symbol) where
  parseReturn :: Alternative f
              => GQL.SelectionSet
              -> f (ReturnQuery p r)

instance ParseReturn p ('PrimitiveRef t) where
  parseReturn [] = pure RetPrimitive
  parseReturn _  = empty
instance ParseReturn p ('SchemaRef sch sty) where
  parseReturn _ = pure RetSchema
instance ParseReturn p r
         => ParseReturn p ('ListRef r) where
  parseReturn s = RetList <$> parseReturn s
instance ParseReturn p r
         => ParseReturn p ('OptionalRef r) where
  parseReturn s = RetOptional <$> parseReturn s
instance ( p ~ 'Package pname ss,
           LookupService ss s ~ 'Service s sanns methods,
           ParseMethod p methods
         ) => ParseReturn p ('ObjectRef s) where
  parseReturn s = RetObject <$> parseQuery (Proxy @p) (Proxy @s) s
