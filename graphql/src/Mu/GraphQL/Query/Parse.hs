{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language ViewPatterns          #-}
{-# OPTIONS_GHC -Wincomplete-patterns -fno-warn-orphans #-}

module Mu.GraphQL.Query.Parse where

import           Control.Monad.Except
import qualified Data.Aeson                    as A
import           Data.Coerce                   (coerce)
import qualified Data.Foldable                 as F
import qualified Data.HashMap.Strict           as HM
import           Data.Int                      (Int32)
import           Data.Kind
import           Data.List                     (find)
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific               (floatingOrInteger)
import           Data.SOP.NS
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Query.Definition
import           Mu.Rpc
import           Mu.Schema

type VariableMapC = HM.HashMap T.Text GQL.ValueConst
type VariableMap  = HM.HashMap T.Text GQL.Value
type FragmentMap  = HM.HashMap T.Text GQL.FragmentDefinition

instance A.FromJSON GQL.ValueConst where
  parseJSON A.Null       = pure GQL.VCNull
  parseJSON (A.Bool b)   = pure $ GQL.VCBoolean b
  parseJSON (A.String s) = pure $ GQL.VCString $ coerce s
  parseJSON (A.Number n) = pure $ either GQL.VCFloat GQL.VCInt $ floatingOrInteger n
  parseJSON (A.Array xs) = GQL.VCList . GQL.ListValueG . F.toList <$> traverse A.parseJSON xs
  parseJSON (A.Object o) = GQL.VCObject . GQL.ObjectValueG . fmap toObjFld . HM.toList <$> traverse A.parseJSON o
    where
      toObjFld :: (T.Text, GQL.ValueConst) -> GQL.ObjectFieldG GQL.ValueConst
      toObjFld (k, v) = GQL.ObjectFieldG (coerce k) v

parseDoc ::
  forall qr mut sub p f.
  ( MonadError T.Text f, ParseTypedDoc p qr mut sub ) =>
  Maybe T.Text -> VariableMapC ->
  GQL.ExecutableDocument ->
  f (Document p qr mut sub)
-- If there's no operation name, there must be only one query
parseDoc Nothing vmap (GQL.ExecutableDocument defns)
  = case GQL.partitionExDefs defns of
      ([unnamed], [], frs)
        -> parseTypedDocQuery HM.empty (fragmentsToMap frs) unnamed
      ([], [named], frs)
        -> parseTypedDoc vmap (fragmentsToMap frs) named
      ([], [], _) -> throwError "no operation to execute"
      (_,  [], _) -> throwError "more than one unnamed query"
      ([], _, _)  -> throwError "more than one named operation but no 'operationName' given"
      (_,  _, _)  -> throwError "both named and unnamed queries, but no 'operationName' given"
-- If there's an operation name, look in the named queries
parseDoc (Just operationName) vmap (GQL.ExecutableDocument defns)
  = case GQL.partitionExDefs defns of
      (_, named, frs) -> maybe notFound (parseTypedDoc vmap (fragmentsToMap frs)) (find isThis named)
    where isThis (GQL._todName -> Just nm)
            = GQL.unName nm == operationName
          isThis _ = False
          notFound :: MonadError T.Text f => f a
          notFound = throwError $ "operation '" <> operationName <> "' was not found"

fragmentsToMap :: [GQL.FragmentDefinition] -> FragmentMap
fragmentsToMap = HM.fromList . map fragmentToThingy
  where fragmentToThingy :: GQL.FragmentDefinition -> (T.Text, GQL.FragmentDefinition)
        fragmentToThingy f = (GQL.unName $ GQL._fdName f, f)

parseTypedDoc ::
  (MonadError T.Text f, ParseTypedDoc p qr mut sub) =>
  VariableMapC -> FragmentMap ->
  GQL.TypedOperationDefinition ->
  f (Document p qr mut sub)
parseTypedDoc vmap frmap tod
  = let defVmap = parseVariableMap (GQL._todVariableDefinitions tod)
        finalVmap = constToValue <$> HM.union vmap defVmap  -- first one takes precedence
    in case GQL._todType tod of
        GQL.OperationTypeQuery
          -> parseTypedDocQuery finalVmap frmap (GQL._todSelectionSet tod)
        GQL.OperationTypeMutation
          -> parseTypedDocMutation finalVmap frmap (GQL._todSelectionSet tod)
        GQL.OperationTypeSubscription
          -> parseTypedDocSubscription finalVmap frmap (GQL._todSelectionSet tod)

class ParseTypedDoc (p :: Package')
                    (qr :: Maybe Symbol) (mut :: Maybe Symbol) (sub :: Maybe Symbol) where
  parseTypedDocQuery ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    GQL.SelectionSet ->
    f (Document p qr mut sub)
  parseTypedDocMutation ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    GQL.SelectionSet ->
    f (Document p qr mut sub)
  parseTypedDocSubscription ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    GQL.SelectionSet ->
    f (Document p qr mut sub)

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    KnownName qr, ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    KnownName mut, ParseMethod p mmethods,
    LookupService ss sub ~ 'Service sub sanns smethods,
    KnownName sub, ParseMethod p smethods
  ) => ParseTypedDoc p ('Just qr) ('Just mut) ('Just sub) where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery Proxy Proxy vmap frmap sset
         case q of
           [one] -> pure $ SubscriptionDoc one
           _     -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    KnownName qr, ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    KnownName mut, ParseMethod p mmethods
  ) => ParseTypedDoc p ('Just qr) ('Just mut) 'Nothing where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    KnownName qr, ParseMethod p qmethods,
    LookupService ss sub ~ 'Service sub sanns smethods,
    KnownName sub, ParseMethod p smethods
  ) => ParseTypedDoc p ('Just qr) 'Nothing ('Just sub) where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery Proxy Proxy vmap frmap sset
         case q of
           [one] -> pure $ SubscriptionDoc one
           _     -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    KnownName qr, ParseMethod p qmethods
  ) => ParseTypedDoc p ('Just qr) 'Nothing 'Nothing where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss mut ~ 'Service mut manns mmethods,
    KnownName mut, ParseMethod p mmethods,
    LookupService ss sub ~ 'Service sub sanns smethods,
    KnownName sub, ParseMethod p smethods
  ) => ParseTypedDoc p 'Nothing ('Just mut) ('Just sub) where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery Proxy Proxy vmap frmap sset
         case q of
           [one] -> pure $ SubscriptionDoc one
           _     -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss mut ~ 'Service mut manns mmethods,
    KnownName mut, ParseMethod p mmethods
  ) => ParseTypedDoc p 'Nothing ('Just mut) 'Nothing where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery Proxy Proxy vmap frmap sset
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss sub ~ 'Service sub sanns smethods,
    KnownName sub, ParseMethod p smethods
  ) => ParseTypedDoc p 'Nothing 'Nothing ('Just sub) where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery Proxy Proxy vmap frmap sset
         case q of
           [one] -> pure $ SubscriptionDoc one
           _     -> throwError "subscriptions may only have one field"

instance
  ParseTypedDoc p 'Nothing 'Nothing 'Nothing where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

parseVariableMap :: [GQL.VariableDefinition] -> VariableMapC
parseVariableMap vmap
  = HM.fromList [(GQL.unName (GQL.unVariable v), def)
                | GQL.VariableDefinition v _ (Just def) <- vmap]

constToValue :: GQL.ValueConst -> GQL.Value
constToValue (GQL.VCInt n)     = GQL.VInt n
constToValue (GQL.VCFloat n)   = GQL.VFloat n
constToValue (GQL.VCString n)  = GQL.VString n
constToValue (GQL.VCBoolean n) = GQL.VBoolean n
constToValue GQL.VCNull        = GQL.VNull
constToValue (GQL.VCEnum n)    = GQL.VEnum n
constToValue (GQL.VCList (GQL.ListValueG n))
  = GQL.VList $ GQL.ListValueG $ constToValue <$> n
constToValue (GQL.VCObject (GQL.ObjectValueG n))
  = GQL.VObject $ GQL.ObjectValueG
      [ GQL.ObjectFieldG a (constToValue v) | GQL.ObjectFieldG a v <- n ]

parseQuery ::
  forall (p :: Package') (s :: Symbol) pname ss sanns methods f.
  ( MonadError T.Text f, p ~ 'Package pname ss,
    LookupService ss s ~ 'Service s sanns methods,
    KnownName s, ParseMethod p methods
  ) =>
  Proxy p ->
  Proxy s ->
  VariableMap -> FragmentMap -> GQL.SelectionSet ->
  f (ServiceQuery p (LookupService ss s))
parseQuery _ _ _ _ [] = pure []
parseQuery pp ps vmap frmap (GQL.SelectionField fld : ss)
  = (++) <$> (maybeToList <$> fieldToMethod fld)
         <*> parseQuery pp ps vmap frmap ss
  where
    fieldToMethod :: GQL.Field -> f (Maybe (OneMethodQuery p ('Service sname sanns methods)))
    fieldToMethod (GQL.Field alias name args dirs sels)
      | any (shouldSkip vmap) dirs
      = pure Nothing
      | otherwise
      = Just . OneMethodQuery (GQL.unName . GQL.unAlias <$> alias)
         <$> selectMethod (T.pack $ nameVal (Proxy @s)) vmap frmap name args sels
parseQuery pp ps vmap frmap (GQL.SelectionFragmentSpread (GQL.FragmentSpread nm dirs) : ss)
  | Just fr <- HM.lookup (GQL.unName nm) frmap
  = if not (any (shouldSkip vmap) dirs) && not (any (shouldSkip vmap) $ GQL._fdDirectives fr)
       then (++) <$> parseQuery pp ps vmap frmap (GQL._fdSelectionSet fr)
                 <*> parseQuery pp ps vmap frmap ss
       else parseQuery pp ps vmap frmap ss
  | otherwise  -- the fragment definition was not found
  = throwError $ "fragment '" <> GQL.unName nm <> "' was not found"
parseQuery _ _ _ _ (_ : _)  -- Inline fragments are not yet supported
  = throwError "inline fragments are not (yet) supported"

shouldSkip :: VariableMap -> GQL.Directive -> Bool
shouldSkip vmap (GQL.Directive (GQL.unName -> nm) [GQL.Argument (GQL.unName -> ifn) v])
  | nm == "skip", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap "" v of
      Right (FPrimitive b) -> b
      _                    -> False
  | nm == "include", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap "" v of
      Right (FPrimitive b) -> not b
      _                    -> False
shouldSkip _ _ = False

class ParseMethod (p :: Package') (ms :: [Method']) where
  selectMethod ::
    MonadError T.Text f =>
    T.Text ->
    VariableMap ->
    FragmentMap ->
    GQL.Name ->
    [GQL.Argument] ->
    GQL.SelectionSet ->
    f (NS (ChosenMethodQuery p) ms)

instance ParseMethod p '[] where
  selectMethod tyName _ _ (GQL.unName -> wanted) _ _
    = throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"
instance
  (KnownSymbol mname, ParseMethod p ms, ParseArgs p args, ParseDifferentReturn p r) =>
  ParseMethod p ('Method mname manns args r ': ms)
  where
  selectMethod tyName vmap frmap w@(GQL.unName -> wanted) args sels
    | wanted == mname
    = Z <$> (ChosenMethodQuery <$> parseArgs vmap args
                               <*> parseDiffReturn vmap frmap wanted sels)
    | otherwise
    = S <$> selectMethod tyName vmap frmap w args sels
    where
      mname = T.pack $ nameVal (Proxy @mname)

class ParseArgs (p :: Package') (args :: [Argument']) where
  parseArgs :: MonadError T.Text f
            => VariableMap
            -> [GQL.Argument]
            -> f (NP (ArgumentValue p) args)

instance ParseArgs p '[] where
  parseArgs _ _ = pure Nil
-- one single argument without name
instance ParseArg p a
         => ParseArgs p '[ 'ArgSingle 'Nothing anns a ] where
  parseArgs vmap [GQL.Argument _ x]
    = (\v -> ArgumentValue v :* Nil) <$> parseArg' vmap "arg" x
  parseArgs _ _
    = throwError "this field receives one single argument"
instance ParseArg p a
         => ParseArgs p '[ 'ArgStream 'Nothing anns a ] where
  parseArgs vmap [GQL.Argument _ x]
    = (\v -> ArgumentStream v :* Nil) <$> parseArg' vmap "arg" x
  parseArgs _ _
    = throwError "this field receives one single argument"
-- more than one argument
instance (KnownName aname, ParseArg p a, ParseArgs p as, FindDefaultArgValue aanns)
         => ParseArgs p ('ArgSingle ('Just aname) aanns a ': as) where
  parseArgs vmap args
    = let aname = T.pack $ nameVal (Proxy @aname)
      in case find ((== nameVal (Proxy @aname)) . T.unpack . GQL.unName . GQL._aName) args of
        Just (GQL.Argument _ x)
          -> (:*) <$> (ArgumentValue <$> parseArg' vmap aname x)
                  <*> parseArgs vmap args
        Nothing
          -> do x <- findDefaultArgValue (Proxy @aanns) aname
                (:*) <$> (ArgumentValue <$> parseArg' vmap aname (constToValue x))
                     <*> parseArgs vmap args
instance (KnownName aname, ParseArg p a, ParseArgs p as, FindDefaultArgValue aanns)
         => ParseArgs p ('ArgStream ('Just aname) aanns a ': as) where
  parseArgs vmap args
    = let aname = T.pack $ nameVal (Proxy @aname)
      in case find ((== nameVal (Proxy @aname)) . T.unpack . GQL.unName . GQL._aName) args of
        Just (GQL.Argument _ x)
          -> (:*) <$> (ArgumentStream <$> parseArg' vmap aname x)
                  <*> parseArgs vmap args
        Nothing
          -> do x <- findDefaultArgValue (Proxy @aanns) aname
                (:*) <$> (ArgumentStream <$> parseArg' vmap aname (constToValue x))
                     <*> parseArgs vmap args

class FindDefaultArgValue (vs :: [Type]) where
  findDefaultArgValue :: MonadError T.Text f
                      => Proxy vs
                      -> T.Text
                      -> f GQL.ValueConst
instance FindDefaultArgValue '[] where
  findDefaultArgValue _ aname
    = throwError $ "argument '" <> aname <> "' was not given a value, and has no default one"
instance {-# OVERLAPPABLE #-} FindDefaultArgValue xs
         => FindDefaultArgValue (x ': xs) where
  findDefaultArgValue _ = findDefaultArgValue (Proxy @xs)
instance {-# OVERLAPS #-} ReflectValueConst v
         => FindDefaultArgValue (DefaultValue v ': xs) where
  findDefaultArgValue _ _ = pure $ reflectValueConst (Proxy @v)

parseArg' :: (ParseArg p a, MonadError T.Text f)
          => VariableMap
          -> T.Text
          -> GQL.Value
          -> f (ArgumentValue' p a)
parseArg' vmap aname (GQL.VVariable (GQL.unName . GQL.unVariable -> x))
  = case HM.lookup x vmap of
      Nothing -> throwError $ "variable '" <> x <> "' was not found"
      Just v  -> parseArg vmap aname v
parseArg' vmap aname v = parseArg vmap aname v

class ParseArg (p :: Package') (a :: TypeRef Symbol) where
  parseArg :: MonadError T.Text f
           => VariableMap
           -> T.Text
           -> GQL.Value
           -> f (ArgumentValue' p a)

instance (ParseArg p r) => ParseArg p ('ListRef r) where
  parseArg vmap aname (GQL.VList (GQL.ListValueG xs))
    = ArgList <$> traverse (parseArg' vmap aname) xs
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Bool) where
  parseArg _ _ (GQL.VBoolean b)
    = pure (ArgPrimitive b)
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Int32) where
  parseArg _ _ (GQL.VInt b)
    = pure (ArgPrimitive b)
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Integer) where
  parseArg _ _ (GQL.VInt b)
    = pure $ ArgPrimitive $ fromIntegral b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Double) where
  parseArg _ _ (GQL.VFloat b)
    = pure (ArgPrimitive b)
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef T.Text) where
  parseArg _ _ (GQL.VString (GQL.StringValue b))
    = pure $ ArgPrimitive b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef String) where
  parseArg _ _ (GQL.VString (GQL.StringValue b))
    = pure $ ArgPrimitive $ T.unpack b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef ()) where
  parseArg _ _ GQL.VNull = pure $ ArgPrimitive ()
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance (ObjectOrEnumParser sch (sch :/: sty))
         => ParseArg p ('SchemaRef sch sty) where
  parseArg vmap aname v
    = ArgSchema <$> parseObjectOrEnum' vmap aname v

parseObjectOrEnum' :: (ObjectOrEnumParser sch t, MonadError T.Text f)
          => VariableMap
          -> T.Text
          -> GQL.Value
          -> f (Term sch t)
parseObjectOrEnum' vmap aname (GQL.VVariable (GQL.unName . GQL.unVariable -> x))
  = case HM.lookup x vmap of
      Nothing -> throwError $ "variable '" <> x <> "' was not found"
      Just v  -> parseObjectOrEnum vmap aname v
parseObjectOrEnum' vmap aname v
  = parseObjectOrEnum vmap aname v

class ObjectOrEnumParser (sch :: Schema') (t :: TypeDef Symbol Symbol) where
  parseObjectOrEnum :: MonadError T.Text f
                    => VariableMap
                    -> T.Text
                    -> GQL.Value
                    -> f (Term sch t)

instance (ObjectParser sch args, KnownName name)
         => ObjectOrEnumParser sch ('DRecord name args) where
  parseObjectOrEnum vmap _ (GQL.VObject (GQL.ObjectValueG vs))
    = TRecord <$> objectParser vmap (T.pack $ nameVal (Proxy @name)) vs
  parseObjectOrEnum _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance (EnumParser choices, KnownName name)
         => ObjectOrEnumParser sch ('DEnum name choices) where
  parseObjectOrEnum _ _ (GQL.VEnum (GQL.EnumValue nm))
    = TEnum <$> enumParser (T.pack $ nameVal (Proxy @name)) nm
  parseObjectOrEnum _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"

class ObjectParser (sch :: Schema') (args :: [FieldDef Symbol Symbol]) where
  objectParser :: MonadError T.Text f
               => VariableMap
               -> T.Text
               -> [GQL.ObjectFieldG GQL.Value]
               -> f (NP (Field sch) args)

instance ObjectParser sch '[] where
  objectParser _ _ _ = pure Nil
instance
  (ObjectParser sch args, ValueParser sch v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser vmap tyName args
    = let wanted = T.pack $ nameVal (Proxy @nm)
      in case find ((== wanted) . GQL.unName . GQL._ofName) args of
        Just (GQL.ObjectFieldG _ v)
          -> (:*) <$> (Field <$> valueParser' vmap wanted v) <*> objectParser vmap tyName args
        Nothing -> throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"

class EnumParser (choices :: [ChoiceDef Symbol]) where
  enumParser :: MonadError T.Text f
             => T.Text -> GQL.Name
             -> f (NS Proxy choices)

instance EnumParser '[] where
  enumParser tyName (GQL.unName -> wanted)
    = throwError $ "value '" <> wanted <> "' was not found on enum '" <> tyName <> "'"
instance (KnownName name, EnumParser choices)
         => EnumParser ('ChoiceDef name ': choices) where
  enumParser tyName w@(GQL.unName -> wanted)
    | wanted == mname = pure (Z Proxy)
    | otherwise = S <$> enumParser tyName w
    where
      mname = T.pack $ nameVal (Proxy @name)

valueParser' :: (ValueParser sch v, MonadError T.Text f)
             => VariableMap
             -> T.Text
             -> GQL.Value
             -> f (FieldValue sch v)
valueParser' vmap aname (GQL.VVariable (GQL.unName . GQL.unVariable -> x))
  = case HM.lookup x vmap of
      Nothing -> throwError $ "variable '" <> x <> "' was not found"
      Just v  -> valueParser vmap aname v
valueParser' vmap aname v = valueParser vmap aname v

class ValueParser (sch :: Schema') (v :: FieldType Symbol) where
  valueParser :: MonadError T.Text f
              => VariableMap
              -> T.Text
              -> GQL.Value
              -> f (FieldValue sch v)

instance ValueParser sch 'TNull where
  valueParser _ _ GQL.VNull = pure FNull
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Bool) where
  valueParser _ _ (GQL.VBoolean b) = pure (FPrimitive b)
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Int32) where
  valueParser _ _ (GQL.VInt b) = pure (FPrimitive b)
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Integer) where
  valueParser _ _ (GQL.VInt b) = pure $ FPrimitive $ fromIntegral b
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Double) where
  valueParser _ _ (GQL.VFloat b) = pure (FPrimitive b)
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive T.Text) where
  valueParser _ _ (GQL.VString (GQL.StringValue b))
    = pure $ FPrimitive b
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive String) where
  valueParser _ _ (GQL.VString (GQL.StringValue b))
    = pure $ FPrimitive $ T.unpack b
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance (ValueParser sch r) => ValueParser sch ('TList r) where
  valueParser vmap fname (GQL.VList (GQL.ListValueG xs))
    = FList <$> traverse (valueParser' vmap fname) xs
  valueParser _ fname _
    = throwError $ "field '" <> fname <> "' was not of right type"
instance (ValueParser sch r) => ValueParser sch ('TOption r) where
  valueParser _ _ GQL.VNull
    = pure $ FOption Nothing
  valueParser vmap fname v
    = FOption . Just <$> valueParser' vmap fname v
instance (ObjectOrEnumParser sch (sch :/: sty), KnownName sty)
         => ValueParser sch ('TSchematic sty) where
  valueParser vmap _ v
    = FSchematic <$> parseObjectOrEnum' vmap (T.pack $ nameVal (Proxy @sty)) v

class ParseDifferentReturn (p :: Package') (r :: Return Symbol) where
  parseDiffReturn :: MonadError T.Text f
                  => VariableMap
                  -> FragmentMap
                  -> T.Text
                  -> GQL.SelectionSet
                  -> f (ReturnQuery p r)
instance ParseDifferentReturn p 'RetNothing where
  parseDiffReturn _ _ _ [] = pure RNothing
  parseDiffReturn _ _ fname _
    = throwError $ "field '" <> fname <> "' should not have a selection of subfields"
instance ParseReturn p r => ParseDifferentReturn p ('RetSingle r) where
  parseDiffReturn vmap frmap fname s
    = RSingle <$> parseReturn vmap frmap fname s
instance ParseReturn p r => ParseDifferentReturn p ('RetStream r) where
  parseDiffReturn vmap frmap fname s
    = RStream <$> parseReturn vmap frmap fname s

class ParseReturn (p :: Package') (r :: TypeRef Symbol) where
  parseReturn :: MonadError T.Text f
              => VariableMap
              -> FragmentMap
              -> T.Text
              -> GQL.SelectionSet
              -> f (ReturnQuery' p r)

instance ParseReturn p ('PrimitiveRef t) where
  parseReturn _ _ _ []
    = pure RetPrimitive
  parseReturn _ _ fname _
    = throwError $ "field '" <> fname <> "' should not have a selection of subfields"
instance (ParseSchema sch (sch :/: sty))
         => ParseReturn p ('SchemaRef sch sty) where
  parseReturn vmap frmap fname s
    = RetSchema <$> parseSchema vmap frmap fname s
instance ParseReturn p r
         => ParseReturn p ('ListRef r) where
  parseReturn vmap frmap fname s
    = RetList <$> parseReturn vmap frmap fname s
instance ParseReturn p r
         => ParseReturn p ('OptionalRef r) where
  parseReturn vmap frmap fname s
    = RetOptional <$> parseReturn vmap frmap fname s
instance ( p ~ 'Package pname ss,
           LookupService ss s ~ 'Service s sanns methods,
           KnownName s, ParseMethod p methods
         ) => ParseReturn p ('ObjectRef s) where
  parseReturn vmap frmap _ s
    = RetObject <$> parseQuery (Proxy @p) (Proxy @s) vmap frmap s

class ParseSchema (s :: Schema') (t :: TypeDef Symbol Symbol) where
  parseSchema :: MonadError T.Text f
              => VariableMap
              -> FragmentMap
              -> T.Text
              -> GQL.SelectionSet
              -> f (SchemaQuery s t)
instance ParseSchema sch ('DEnum name choices) where
  parseSchema _ _ _ []
    = pure QueryEnum
  parseSchema _ _ fname _
    = throwError $ "field '" <> fname <> "' should not have a selection of subfields"
instance (KnownSymbol name, ParseField sch fields)
         => ParseSchema sch ('DRecord name fields) where
  parseSchema vmap frmap _ s
    = QueryRecord <$> parseSchemaQuery (Proxy @sch) (Proxy @('DRecord name fields)) vmap frmap s

parseSchemaQuery ::
  forall (sch :: Schema') t (rname :: Symbol) fields f.
  ( MonadError T.Text f
  , t ~  'DRecord rname fields
  , KnownSymbol rname
  , ParseField sch fields ) =>
  Proxy sch ->
  Proxy t ->
  VariableMap -> FragmentMap -> GQL.SelectionSet ->
  f [OneFieldQuery sch fields]
parseSchemaQuery _ _ _ _ [] = pure []
parseSchemaQuery pp ps vmap frmap (GQL.SelectionField fld : ss)
  = (++) <$> (maybeToList <$> fieldToMethod fld)
         <*> parseSchemaQuery pp ps vmap frmap ss
  where
    fieldToMethod :: GQL.Field -> f (Maybe (OneFieldQuery sch fields))
    fieldToMethod (GQL.Field alias name args dirs sels)
      | any (shouldSkip vmap) dirs
      = pure Nothing
      | _:_ <- args
      = throwError "this field does not support arguments"
      | otherwise
      = Just . OneFieldQuery (GQL.unName . GQL.unAlias <$> alias)
         <$> selectField (T.pack $ nameVal (Proxy @rname)) vmap frmap name sels
parseSchemaQuery pp ps vmap frmap (GQL.SelectionFragmentSpread (GQL.FragmentSpread nm dirs) : ss)
  | Just fr <- HM.lookup (GQL.unName nm) frmap
  = if not (any (shouldSkip vmap) dirs) && not (any (shouldSkip vmap) $ GQL._fdDirectives fr)
       then (++) <$> parseSchemaQuery pp ps vmap frmap (GQL._fdSelectionSet fr)
                 <*> parseSchemaQuery pp ps vmap frmap ss
       else parseSchemaQuery pp ps vmap frmap ss
  | otherwise  -- the fragment definition was not found
  = throwError $ "fragment '" <> GQL.unName nm <> "' was not found"
parseSchemaQuery _ _ _ _ (_ : _)  -- Inline fragments are not yet supported
  = throwError "inline fragments are not (yet) supported"

class ParseField (sch :: Schema') (fs :: [FieldDef Symbol Symbol]) where
  selectField ::
    MonadError T.Text f =>
    T.Text ->
    VariableMap ->
    FragmentMap ->
    GQL.Name ->
    GQL.SelectionSet ->
    f (NS (ChosenFieldQuery sch) fs)

instance ParseField sch '[] where
  selectField tyName _ _ (GQL.unName -> wanted) _
    = throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"
instance
  (KnownSymbol fname, ParseField sch fs, ParseSchemaReturn sch r) =>
  ParseField sch ('FieldDef fname r ': fs)
  where
  selectField tyName vmap frmap w@(GQL.unName -> wanted) sels
    | wanted == mname
    = Z <$> (ChosenFieldQuery <$> parseSchemaReturn vmap frmap wanted sels)
    | otherwise
    = S <$> selectField tyName vmap frmap w sels
    where
      mname = T.pack $ nameVal (Proxy @fname)

class ParseSchemaReturn (sch :: Schema') (r :: FieldType Symbol) where
  parseSchemaReturn :: MonadError T.Text f
                    => VariableMap
                    -> FragmentMap
                    -> T.Text
                    -> GQL.SelectionSet
                    -> f (ReturnSchemaQuery sch r)

instance ParseSchemaReturn sch ('TPrimitive t) where
  parseSchemaReturn _ _ _ []
    = pure RetSchPrimitive
  parseSchemaReturn _ _ fname _
    = throwError $ "field '" <> fname <> "' should not have a selection of subfields"
instance ( ParseSchema sch (sch :/: sty) )
         => ParseSchemaReturn sch ('TSchematic sty) where
  parseSchemaReturn vmap frmap fname s
    = RetSchSchema <$> parseSchema vmap frmap fname s
instance ParseSchemaReturn sch r
         => ParseSchemaReturn sch ('TList r) where
  parseSchemaReturn vmap frmap fname s
    = RetSchList <$> parseSchemaReturn vmap frmap fname s
instance ParseSchemaReturn sch r
         => ParseSchemaReturn sch ('TOption r) where
  parseSchemaReturn vmap frmap fname s
    = RetSchOptional <$> parseSchemaReturn vmap frmap fname s
