{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language ViewPatterns          #-}
{-# OPTIONS_GHC -Wincomplete-patterns -fno-warn-orphans #-}

module Mu.GraphQL.Query.Parse where

import           Control.Monad.Except
import qualified Data.Aeson                  as A
import qualified Data.Foldable               as F
import qualified Data.HashMap.Strict         as HM
import           Data.Int                    (Int32)
import           Data.List                   (find)
import           Data.Maybe
import           Data.Proxy
import           Data.SOP.NS
import           Data.Scientific             (Scientific, floatingOrInteger, fromFloatDigits)
import qualified Data.Text                   as T
import           GHC.TypeLits
import qualified Language.GraphQL.AST        as GQL

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Query.Definition
import           Mu.Rpc
import           Mu.Schema

type VariableMapC = HM.HashMap T.Text GQL.ConstValue
type VariableMap  = HM.HashMap T.Text GQL.Value
type FragmentMap  = HM.HashMap T.Text GQL.FragmentDefinition

instance A.FromJSON GQL.ConstValue where
  parseJSON A.Null       = pure GQL.ConstNull
  parseJSON (A.Bool b)   = pure $ GQL.ConstBoolean b
  parseJSON (A.String s) = pure $ GQL.ConstString s
  parseJSON (A.Number n) = case floatingOrInteger n :: Either Double Int32 of
                             Right i -> pure $ GQL.ConstInt i
                             Left  m -> pure $ GQL.ConstFloat m
  parseJSON (A.Array xs) = GQL.ConstList . map (`GQL.Node` GQL.Location 0 0) . F.toList
    <$> traverse A.parseJSON xs
  parseJSON (A.Object o) = GQL.ConstObject . fmap toObjFld . HM.toList <$> traverse A.parseJSON o
    where
      toObjFld :: (T.Text, GQL.ConstValue) -> GQL.ObjectField GQL.ConstValue
      toObjFld (k, v) = GQL.ObjectField k (GQL.Node v zl) zl
      zl = GQL.Location 0 0

parseDoc ::
  forall qr mut sub p f.
  ( MonadError T.Text f, ParseTypedDoc p qr mut sub ) =>
  Maybe T.Text -> VariableMapC ->
  [GQL.Definition] ->
  f (Document p qr mut sub)
-- If there's no operation name, there must be only one query
parseDoc Nothing vmap defns
  = case partitionExDefs defns of
      ([unnamed], [], frs)
        -> parseTypedDocQuery HM.empty (fragmentsToMap frs) unnamed
      ([], [named], frs)
        -> parseTypedDoc vmap (fragmentsToMap frs) named
      ([], [], _) -> throwError "no operation to execute"
      (_,  [], _) -> throwError "more than one unnamed query"
      ([], _, _)  -> throwError "more than one named operation but no 'operationName' given"
      (_,  _, _)  -> throwError "both named and unnamed queries, but no 'operationName' given"
-- If there's an operation name, look in the named queries
parseDoc (Just operationName) vmap defns
  = case partitionExDefs defns of
      (_, named, frs) -> maybe notFound
                               (parseTypedDoc vmap (fragmentsToMap frs))
                               (find isThis named)
    where isThis (GQL.OperationDefinition _ (Just nm) _ _ _ _)
            = nm == operationName
          isThis _ = False
          notFound :: MonadError T.Text f => f a
          notFound = throwError $ "operation '" <> operationName <> "' was not found"

partitionExDefs
 :: [GQL.Definition]
 -> ([[GQL.Selection]], [GQL.OperationDefinition], [GQL.FragmentDefinition])
partitionExDefs defs
  = ( [ F.toList ss
      | GQL.ExecutableDefinition (GQL.DefinitionOperation (GQL.SelectionSet ss _)) <- defs ]
    , [ od
      | GQL.ExecutableDefinition (GQL.DefinitionOperation od@GQL.OperationDefinition {}) <- defs ]
    , [ fr
      | GQL.ExecutableDefinition (GQL.DefinitionFragment fr) <- defs ])

parseTypedDoc ::
  (MonadError T.Text f, ParseTypedDoc p qr mut sub) =>
  VariableMapC -> FragmentMap ->
  GQL.OperationDefinition ->
  f (Document p qr mut sub)
parseTypedDoc _ _ GQL.SelectionSet {}
  = error "this should have been handled in parseDoc"
parseTypedDoc vmap frmap (GQL.OperationDefinition typ _ vdefs _ (F.toList -> ss) _)
  = let defVmap = parseVariableMap vdefs
        finalVmap = constToValue <$> HM.union vmap defVmap  -- first one takes precedence
    in case typ of
        GQL.Query        -> parseTypedDocQuery finalVmap frmap ss
        GQL.Mutation     -> parseTypedDocMutation finalVmap frmap ss
        GQL.Subscription -> parseTypedDocSubscription finalVmap frmap ss

fragmentsToMap :: [GQL.FragmentDefinition] -> FragmentMap
fragmentsToMap = HM.fromList . map fragmentToThingy
  where fragmentToThingy :: GQL.FragmentDefinition -> (T.Text, GQL.FragmentDefinition)
        fragmentToThingy f = (fdName f, f)

class ParseTypedDoc (p :: Package')
                    (qr :: Maybe Symbol) (mut :: Maybe Symbol) (sub :: Maybe Symbol) where
  parseTypedDocQuery ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    [GQL.Selection] ->
    f (Document p qr mut sub)
  parseTypedDocMutation ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    [GQL.Selection] ->
    f (Document p qr mut sub)
  parseTypedDocSubscription ::
    MonadError T.Text f =>
    VariableMap -> FragmentMap ->
    [GQL.Selection] ->
    f (Document p qr mut sub)

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qmethods,
    KnownName qr, ParseMethod p ('Service qr qmethods) qmethods,
    LookupService ss mut ~ 'Service mut mmethods,
    KnownName mut, ParseMethod p ('Service mut mmethods) mmethods,
    LookupService ss sub ~ 'Service sub smethods,
    KnownName sub, ParseMethod p ('Service sub smethods) smethods
  ) => ParseTypedDoc p ('Just qr) ('Just mut) ('Just sub) where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery (Proxy @p) (Proxy @qr) vmap frmap sset
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery (Proxy @p) (Proxy @mut) vmap frmap sset
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery (Proxy @p) (Proxy @sub) vmap frmap sset
         case q of
           ServiceQuery [one]
             -> pure $ SubscriptionDoc one
           _ -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qmethods,
    KnownName qr, ParseMethod p ('Service qr qmethods) qmethods,
    LookupService ss mut ~ 'Service mut mmethods,
    KnownName mut, ParseMethod p ('Service mut mmethods) mmethods
  ) => ParseTypedDoc p ('Just qr) ('Just mut) 'Nothing where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery (Proxy @p) (Proxy @qr) vmap frmap sset
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery (Proxy @p) (Proxy @mut) vmap frmap sset
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qmethods,
    KnownName qr, ParseMethod p ('Service qr qmethods) qmethods,
    LookupService ss sub ~ 'Service sub smethods,
    KnownName sub, ParseMethod p ('Service sub smethods) smethods
  ) => ParseTypedDoc p ('Just qr) 'Nothing ('Just sub) where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery (Proxy @p) (Proxy @qr) vmap frmap sset
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery (Proxy @p) (Proxy @sub) vmap frmap sset
         case q of
           ServiceQuery [one]
             -> pure $ SubscriptionDoc one
           _ -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qmethods,
    KnownName qr, ParseMethod p ('Service qr qmethods) qmethods
  ) => ParseTypedDoc p ('Just qr) 'Nothing 'Nothing where
  parseTypedDocQuery vmap frmap sset
    = QueryDoc <$> parseQuery (Proxy @p) (Proxy @qr) vmap frmap sset
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss mut ~ 'Service mut mmethods,
    KnownName mut, ParseMethod p ('Service mut mmethods) mmethods,
    LookupService ss sub ~ 'Service sub smethods,
    KnownName sub, ParseMethod p ('Service sub smethods) smethods
  ) => ParseTypedDoc p 'Nothing ('Just mut) ('Just sub) where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery (Proxy @p) (Proxy @mut) vmap frmap sset
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery (Proxy @p) (Proxy @sub) vmap frmap sset
         case q of
           ServiceQuery [one]
             -> pure $ SubscriptionDoc one
           _ -> throwError "subscriptions may only have one field"

instance
  ( p ~ 'Package pname ss,
    LookupService ss mut ~ 'Service mut mmethods,
    KnownName mut, ParseMethod p ('Service mut mmethods) mmethods
  ) => ParseTypedDoc p 'Nothing ('Just mut) 'Nothing where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation vmap frmap sset
    = MutationDoc <$> parseQuery (Proxy @p) (Proxy @mut) vmap frmap sset
  parseTypedDocSubscription _ _ _
    = throwError "no subscriptions are defined in the schema"

instance
  ( p ~ 'Package pname ss,
    LookupService ss sub ~ 'Service sub smethods,
    KnownName sub, ParseMethod p ('Service sub smethods) smethods
  ) => ParseTypedDoc p 'Nothing 'Nothing ('Just sub) where
  parseTypedDocQuery _ _ _
    = throwError "no queries are defined in the schema"
  parseTypedDocMutation _ _ _
    = throwError "no mutations are defined in the schema"
  parseTypedDocSubscription vmap frmap sset
    = do q <- parseQuery (Proxy @p) (Proxy @sub) vmap frmap sset
         case q of
           ServiceQuery [one]
             -> pure $ SubscriptionDoc one
           _ -> throwError "subscriptions may only have one field"

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
  = HM.fromList [(v, def)
                | GQL.VariableDefinition v _ (Just (GQL.Node def _)) _ <- vmap]

constToValue :: GQL.ConstValue -> GQL.Value
constToValue (GQL.ConstInt n)     = GQL.Int n
constToValue (GQL.ConstFloat n)   = GQL.Float n
constToValue (GQL.ConstString n)  = GQL.String n
constToValue (GQL.ConstBoolean n) = GQL.Boolean n
constToValue GQL.ConstNull        = GQL.Null
constToValue (GQL.ConstEnum n)    = GQL.Enum n
constToValue (GQL.ConstList n)
  = GQL.List $ flip map n $ \(GQL.Node x loc) -> GQL.Node (constToValue x) loc
constToValue (GQL.ConstObject n)
  = GQL.Object
      [ GQL.ObjectField a (GQL.Node (constToValue v) m) l
      | GQL.ObjectField a (GQL.Node v m) l <- n ]

class ParseQuery (p :: Package') (s :: Symbol) where
  parseQuery
    :: ( MonadError T.Text f, p ~ 'Package pname ss, KnownName s )
    => Proxy p -> Proxy s
    -> VariableMap -> FragmentMap -> [GQL.Selection]
    -> f (ServiceQuery p (LookupService ss s))

instance ( p ~ 'Package pname ss
         , KnownName s
         , ParseQuery' p s (LookupService ss s) )
         => ParseQuery p s where
  parseQuery pp ps = parseQuery' pp ps (Proxy @(LookupService ss s))

class ParseQuery' (p :: Package') (s :: Symbol) (svc :: Service') where
  parseQuery'
    :: ( MonadError T.Text f, p ~ 'Package pname ss
       , LookupService ss s ~ svc, KnownName s )
    => Proxy p -> Proxy s -> Proxy svc
    -> VariableMap -> FragmentMap -> [GQL.Selection]
    -> f (ServiceQuery p svc)

instance (ParseQueryOneOf p elts)
         => ParseQuery' p s ('OneOf s elts) where
  parseQuery' pp _ps _ vmap frmap fs
    = OneOfQuery <$> parseQueryOneOf pp (Proxy @elts) vmap frmap fs

class ParseQueryOneOf (p :: Package') (s :: [Symbol]) where
  parseQueryOneOf
    :: ( MonadError T.Text f, p ~ 'Package pname ss )
    => Proxy p -> Proxy s
    -> VariableMap -> FragmentMap -> [GQL.Selection]
    -> f (NP (ChosenOneOfQuery p) s)

instance ParseQueryOneOf p '[] where
  parseQueryOneOf _ _ _ _ _ = pure Nil
instance ( ParseQuery p s, KnownSymbol s
         , ParseQueryOneOf p ss)
         => ParseQueryOneOf p (s ': ss) where
  parseQueryOneOf pp _ps vmap frmap sel
    = do refinedSel <- refineSelection sel
         parsedQ    <- parseQuery pp (Proxy @s) vmap frmap refinedSel
         restQ      <- parseQueryOneOf pp (Proxy @ss) vmap frmap sel
         pure (ChosenOneOfQuery (Proxy @s) parsedQ :* restQ)
    where
      -- refineSelection :: [GQL.Selection] -> f [GQL.Selection]
      refineSelection [] = pure []
      refineSelection (f@GQL.FieldSelection {} : rest)
        = (f :) <$> refineSelection rest
      refineSelection (GQL.InlineFragmentSelection (GQL.InlineFragment ty dirs innerSs _) : rest)
        | any (shouldSkip vmap) dirs
        = refineSelection rest
        | Nothing <- ty
        = (++) <$> refineSelection (F.toList innerSs) <*> refineSelection rest
        | Just selectedTy <- ty, selectedTy == T.pack (nameVal (Proxy @s))
        = (++) <$> refineSelection (F.toList innerSs) <*> refineSelection rest
        | otherwise
        = refineSelection rest
      refineSelection (GQL.FragmentSpreadSelection (GQL.FragmentSpread nm dirs _) : rest)
        | any (shouldSkip vmap) dirs
        = refineSelection rest
        | Just (GQL.FragmentDefinition _ fTy fDirs fSel fLoc) <- HM.lookup nm frmap
        = refineSelection (GQL.InlineFragmentSelection (GQL.InlineFragment (Just fTy) fDirs fSel fLoc) : rest)
        | otherwise  -- the fragment definition was not found
        = throwError $ "fragment '" <> nm <> "' was not found"


instance ( ParseMethod p ('Service s methods) methods, KnownName s )
         => ParseQuery' p s ('Service s methods) where
  parseQuery' _pp _ps _psvc vmap frmap fs = ServiceQuery <$> go fs
    where
      go [] = pure []
      go (GQL.FieldSelection fld : ss)
            = (++) <$> (maybeToList <$> fieldToMethod fld) <*> go ss
      go (GQL.FragmentSpreadSelection (GQL.FragmentSpread nm dirs _) : ss)
        | any (shouldSkip vmap) dirs
        = go ss
        | Just (GQL.FragmentDefinition _ fTy fDirs fSel fLoc) <- HM.lookup nm frmap
        = go (GQL.InlineFragmentSelection (GQL.InlineFragment (Just fTy) fDirs fSel fLoc) : ss)
        | otherwise  -- the fragment definition was not found
        = throwError $ "fragment '" <> nm <> "' was not found"
      go (GQL.InlineFragmentSelection (GQL.InlineFragment ty dirs innerSs _) : ss)
        | any (shouldSkip vmap) dirs
        = go ss
        | Nothing <- ty
        = go (F.toList innerSs ++ ss)
        | Just selectedTy <- ty
        = let thisTy = T.pack (nameVal (Proxy @s))
          in if selectedTy == thisTy
             then go (F.toList innerSs ++ ss)
             else throwError $ "fragment for '" <> selectedTy <> "' used in '" <> thisTy <> "'"
      -- fieldToMethod :: GQL.Field -> f (Maybe (OneMethodQuery p ('Service sname methods)))
      fieldToMethod f@(GQL.Field alias name args dirs sels _)
        | any (shouldSkip vmap) dirs
        = pure Nothing
        | name == "__typename"
        = case (args, sels) of
            ([], []) -> pure $ Just $ TypeNameQuery alias
            _        -> throwError "__typename does not admit arguments nor selection of subfields"
        | name == "__schema"
        = case args of
            [] -> Just . SchemaQuery alias <$> unFragment frmap (F.toList sels)
            _  -> throwError "__schema does not admit selection of subfields"
        | name == "__type"
        = let getString (GQL.String s)   = Just s
              getString (GQL.Variable v) = HM.lookup v vmap >>= getString
              getString _                = Nothing
          in case args of
            [GQL.Argument _ (GQL.Node val _) _]
              -> case getString val of
                  Just s -> Just . TypeQuery alias s <$> unFragment frmap sels
                  _      -> throwError "__type requires a string argument"
            _ -> throwError "__type requires one single argument"
        | otherwise
        = Just . OneMethodQuery alias
          <$> selectMethod (Proxy @('Service s methods))
                            (T.pack $ nameVal (Proxy @s))
                            vmap frmap f

shouldSkip :: VariableMap -> GQL.Directive -> Bool
shouldSkip vmap (GQL.Directive nm [GQL.Argument ifn (GQL.Node v _) _] _)
  | nm == "skip", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap "" v of
      Right (FPrimitive b) -> b
      _                    -> False
  | nm == "include", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap "" v of
      Right (FPrimitive b) -> not b
      _                    -> False
shouldSkip _ _ = False

unFragment :: MonadError T.Text f
           => FragmentMap -> [GQL.Selection] -> f [GQL.Selection]
unFragment _ [] = pure []
unFragment frmap (GQL.FragmentSpreadSelection (GQL.FragmentSpread nm _ _) : ss)
  | Just fr <- HM.lookup nm frmap
  = (++) <$> unFragment frmap (fdSelectionSet fr)
         <*> unFragment frmap ss
  | otherwise  -- the fragment definition was not found
  = throwError $ "fragment '" <> nm <> "' was not found"
unFragment frmap (GQL.FieldSelection (GQL.Field al nm args dir innerss loc) : ss)
  = (:) <$> (GQL.FieldSelection . flip (GQL.Field al nm args dir) loc
                <$> unFragment frmap innerss)
        <*> unFragment frmap ss
unFragment _ _
  = throwError "inline fragments are not (yet) supported"

class ParseMethod (p :: Package') (s :: Service') (ms :: [Method']) where
  selectMethod ::
    MonadError T.Text f =>
    Proxy s ->
    T.Text ->
    VariableMap ->
    FragmentMap ->
    GQL.Field ->
    {- GQL.Name ->
    [GQL.Argument] ->
    GQL.SelectionSet -> -}
    f (NS (ChosenMethodQuery p) ms)

instance ParseMethod p s '[] where
  selectMethod _ tyName _ _ (fName -> wanted)
    = throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"
instance
  ( KnownName mname, ParseMethod p s ms
  , ParseArgs p s ('Method mname args r) args
  , ParseDifferentReturn p r) =>
  ParseMethod p s ('Method mname args r ': ms)
  where
  selectMethod s tyName vmap frmap f@(GQL.Field _ wanted args _ sels _)
    | wanted == mname
    = Z <$> (ChosenMethodQuery f
               <$> parseArgs (Proxy @s) (Proxy @('Method mname args r)) vmap args
               <*> parseDiffReturn vmap frmap wanted sels)
    | otherwise
    = S <$> selectMethod s tyName vmap frmap f
    where
      mname = T.pack $ nameVal (Proxy @mname)

class ParseArgs (p :: Package') (s :: Service') (m :: Method') (args :: [Argument']) where
  parseArgs :: MonadError T.Text f
            => Proxy s -> Proxy m
            -> VariableMap
            -> [GQL.Argument]
            -> f (NP (ArgumentValue p) args)

instance ParseArgs p s m '[] where
  parseArgs _ _ _ _ = pure Nil
-- one single argument without name
instance ParseArg p a
         => ParseArgs p s m '[ 'ArgSingle 'Nothing a ] where
  parseArgs _ _ vmap [GQL.Argument _ (GQL.Node x _) _]
    = (\v -> ArgumentValue v :* Nil) <$> parseArg' vmap "arg" x
  parseArgs _ _ _ _
    = throwError "this field receives one single argument"
instance ParseArg p a
         => ParseArgs p s m '[ 'ArgStream 'Nothing a ] where
  parseArgs _ _ vmap [GQL.Argument _ (GQL.Node x _) _]
    = (\v -> ArgumentStream v :* Nil) <$> parseArg' vmap "arg" x
  parseArgs _ _ _ _
    = throwError "this field receives one single argument"
-- more than one argument
instance ( KnownName aname, ParseMaybeArg p a, ParseArgs p s m as
         , s ~ 'Service snm sms, m ~ 'Method mnm margs mr
         , ann ~ GetArgAnnotationMay (AnnotatedPackage DefaultValue p) snm mnm aname
         , FindDefaultArgValue ann )
         => ParseArgs p s m ('ArgSingle ('Just aname) a ': as) where
  parseArgs ps pm vmap args
    = let aname = T.pack $ nameVal (Proxy @aname)
      in case find ((== nameVal (Proxy @aname)) . T.unpack . argName) args of
        Just (GQL.Argument _ (GQL.Node x _) _)
          -> (:*) <$> (ArgumentValue <$> parseMaybeArg vmap aname (Just x))
                  <*> parseArgs ps pm vmap args
        Nothing
          -> do let x = findDefaultArgValue (Proxy @ann)
                (:*) <$> (ArgumentValue <$> parseMaybeArg vmap aname (constToValue <$> x))
                     <*> parseArgs ps pm vmap args
instance ( KnownName aname, ParseArg p a, ParseArgs p s m as
         , s ~ 'Service snm sms, m ~ 'Method mnm margs mr
         , ann ~ GetArgAnnotationMay (AnnotatedPackage DefaultValue p) snm mnm aname
         , FindDefaultArgValue ann )
         => ParseArgs p s m ('ArgStream ('Just aname) a ': as) where
  parseArgs ps pm vmap args
    = let aname = T.pack $ nameVal (Proxy @aname)
      in case find ((== nameVal (Proxy @aname)) . T.unpack . argName) args of
        Just (GQL.Argument _ (GQL.Node x _) _)
          -> (:*) <$> (ArgumentStream <$> parseMaybeArg vmap aname (Just x))
                  <*> parseArgs ps pm vmap args
        Nothing
          -> do let x = findDefaultArgValue (Proxy @ann)
                (:*) <$> (ArgumentStream <$> parseMaybeArg vmap aname (constToValue <$> x))
                     <*> parseArgs ps pm vmap args

class FindDefaultArgValue (vs :: Maybe DefaultValue) where
  findDefaultArgValue :: Proxy vs
                      -> Maybe GQL.ConstValue
instance FindDefaultArgValue 'Nothing where
  findDefaultArgValue _ = Nothing
instance ReflectValueConst v
         => FindDefaultArgValue ('Just ('DefaultValue v)) where
  findDefaultArgValue _ = Just $ reflectValueConst (Proxy @v)

class ParseMaybeArg (p :: Package') (a :: TypeRef Symbol) where
  parseMaybeArg :: MonadError T.Text f
                => VariableMap
                -> T.Text
                -> Maybe GQL.Value
                -> f (ArgumentValue' p a)

instance {-# OVERLAPS #-} (ParseArg p a)
         => ParseMaybeArg p ('OptionalRef a) where
  parseMaybeArg vmap aname (Just x)
    = ArgOptional . Just <$> parseArg' vmap aname x
  parseMaybeArg _ _ Nothing
    = pure $ ArgOptional Nothing
instance {-# OVERLAPS #-} (ParseArg p a)
         => ParseMaybeArg p ('ListRef a) where
  parseMaybeArg vmap aname (Just x)
    = parseArg' vmap aname x
  parseMaybeArg _ _ Nothing
    = pure $ ArgList []
instance {-# OVERLAPPABLE #-} (ParseArg p a)
         => ParseMaybeArg p a where
  parseMaybeArg vmap aname (Just x)
    = parseArg' vmap aname x
  parseMaybeArg _ aname Nothing
    = throwError $ "argument '" <> aname <>
                   "' was not given a value, and has no default one"


parseArg' :: (ParseArg p a, MonadError T.Text f)
          => VariableMap
          -> T.Text
          -> GQL.Value
          -> f (ArgumentValue' p a)
parseArg' vmap aname (GQL.Variable x)
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
  parseArg vmap aname (GQL.List xs)
    = ArgList <$> traverse (parseArg' vmap aname . GQL.node) xs
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Bool) where
  parseArg _ _ (GQL.Boolean b)
    = pure $ ArgPrimitive b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Int32) where
  parseArg _ _ (GQL.Int b)
    = pure $ ArgPrimitive $ fromIntegral b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Integer) where
  parseArg _ _ (GQL.Int b)
    = pure $ ArgPrimitive (toInteger b)
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Scientific) where
  parseArg _ _ (GQL.Float b)
    = pure $ ArgPrimitive $ fromFloatDigits b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef Double) where
  parseArg _ _ (GQL.Float b)
    = pure $ ArgPrimitive b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef T.Text) where
  parseArg _ _ (GQL.String b)
    = pure $ ArgPrimitive b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef String) where
  parseArg _ _ (GQL.String b)
    = pure $ ArgPrimitive $ T.unpack b
  parseArg _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance ParseArg p ('PrimitiveRef ()) where
  parseArg _ _ GQL.Null = pure $ ArgPrimitive ()
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
parseObjectOrEnum' vmap aname (GQL.Variable x)
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
  parseObjectOrEnum vmap _ (GQL.Object vs)
    = TRecord <$> objectParser vmap (T.pack $ nameVal (Proxy @name)) vs
  parseObjectOrEnum _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"
instance (EnumParser choices, KnownName name)
         => ObjectOrEnumParser sch ('DEnum name choices) where
  parseObjectOrEnum _ _ (GQL.Enum nm)
    = TEnum <$> enumParser (T.pack $ nameVal (Proxy @name)) nm
  parseObjectOrEnum _ aname _
    = throwError $ "argument '" <> aname <> "' was not of right type"

class ObjectParser (sch :: Schema') (args :: [FieldDef Symbol Symbol]) where
  objectParser :: MonadError T.Text f
               => VariableMap
               -> T.Text
               -> [GQL.ObjectField GQL.Value]
               -> f (NP (Field sch) args)

instance ObjectParser sch '[] where
  objectParser _ _ _ = pure Nil
instance
  (ObjectParser sch args, ValueParser sch v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser vmap tyName args
    = let wanted = T.pack $ nameVal (Proxy @nm)
      in case find ((== wanted) . GQL.name) args of
        Just (GQL.ObjectField _ (GQL.Node v _) _)
          -> (:*) <$> (Field <$> valueParser' vmap wanted v) <*> objectParser vmap tyName args
        Nothing -> throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"

class EnumParser (choices :: [ChoiceDef Symbol]) where
  enumParser :: MonadError T.Text f
             => T.Text -> GQL.Name
             -> f (NS Proxy choices)

instance EnumParser '[] where
  enumParser tyName wanted
    = throwError $ "value '" <> wanted <> "' was not found on enum '" <> tyName <> "'"
instance (KnownName name, EnumParser choices)
         => EnumParser ('ChoiceDef name ': choices) where
  enumParser tyName wanted
    | wanted == mname = pure (Z Proxy)
    | otherwise = S <$> enumParser tyName wanted
    where
      mname = T.pack $ nameVal (Proxy @name)

valueParser' :: (ValueParser sch v, MonadError T.Text f)
             => VariableMap
             -> T.Text
             -> GQL.Value
             -> f (FieldValue sch v)
valueParser' vmap aname (GQL.Variable x)
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
  valueParser _ _ GQL.Null = pure FNull
  valueParser _ fname _    = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Bool) where
  valueParser _ _ (GQL.Boolean b) = pure $ FPrimitive b
  valueParser _ fname _           = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Int32) where
  valueParser _ _ (GQL.Int b) = pure $ FPrimitive $ fromIntegral b
  valueParser _ fname _       = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Integer) where
  valueParser _ _ (GQL.Int b) = pure $ FPrimitive $ toInteger b
  valueParser _ fname _       = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Scientific) where
  valueParser _ _ (GQL.Float b) = pure $ FPrimitive $ fromFloatDigits b
  valueParser _ fname _         = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive Double) where
  valueParser _ _ (GQL.Float b) = pure $ FPrimitive b
  valueParser _ fname _         = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive T.Text) where
  valueParser _ _ (GQL.String b) = pure $ FPrimitive b
  valueParser _ fname _          = throwError $ "field '" <> fname <> "' was not of right type"
instance ValueParser sch ('TPrimitive String) where
  valueParser _ _ (GQL.String b) = pure $ FPrimitive $ T.unpack b
  valueParser _ fname _          = throwError $ "field '" <> fname <> "' was not of right type"
instance (ValueParser sch r) => ValueParser sch ('TList r) where
  valueParser vmap fname (GQL.List xs) = FList <$> traverse (valueParser' vmap fname . GQL.node) xs
  valueParser _ fname _                = throwError $ "field '" <> fname <> "' was not of right type"
instance (ValueParser sch r) => ValueParser sch ('TOption r) where
  valueParser _ _ GQL.Null = pure $ FOption Nothing
  valueParser vmap fname v = FOption . Just <$> valueParser' vmap fname v
instance (ObjectOrEnumParser sch (sch :/: sty), KnownName sty)
         => ValueParser sch ('TSchematic sty) where
  valueParser vmap _ v = FSchematic <$> parseObjectOrEnum' vmap (T.pack $ nameVal (Proxy @sty)) v
instance ValueParser sch ('TPrimitive A.Value) where
  valueParser vmap _ x = FPrimitive <$> toAesonValue vmap x
instance ValueParser sch ('TPrimitive A.Object) where
  valueParser vm _ (GQL.Object xs) = FPrimitive . HM.fromList <$> traverse (toKeyValuePairs vm) xs
  valueParser _ fname _            = throwError $ "field '" <> fname <> "' was not of right type"

toKeyValuePairs :: MonadError T.Text m => VariableMap -> GQL.ObjectField GQL.Value -> m (T.Text, A.Value)
toKeyValuePairs vmap (GQL.ObjectField key (GQL.Node v _) _) = (key,) <$> toAesonValue vmap v

toAesonValue :: MonadError T.Text m => VariableMap -> GQL.Value -> m A.Value
toAesonValue vm (GQL.Variable v) =
  case HM.lookup v vm of
    Nothing -> throwError $ "variable '" <> v <> "' was not found"
    Just xs -> toAesonValue vm xs
toAesonValue _  (GQL.Int n)      = pure . A.Number $ fromIntegral n
toAesonValue _  (GQL.Float d)    = pure . A.Number $ fromFloatDigits d
toAesonValue _  (GQL.String s)   = pure $ A.String s
toAesonValue _  (GQL.Boolean b)  = pure $ A.Bool b
toAesonValue _   GQL.Null        = pure A.Null
toAesonValue _  (GQL.Enum e)     = pure $ A.String e
toAesonValue vm (GQL.List xs)    = A.toJSON <$> traverse (toAesonValue vm . GQL.node) xs
toAesonValue vm (GQL.Object xs)  = A.Object . HM.fromList <$> traverse (toKeyValuePairs vm) xs

class ParseDifferentReturn (p :: Package') (r :: Return Symbol (TypeRef Symbol)) where
  parseDiffReturn :: MonadError T.Text f
                  => VariableMap
                  -> FragmentMap
                  -> T.Text
                  -> [GQL.Selection]
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
              -> [GQL.Selection]
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
instance ( p ~ 'Package pname ss, ParseQuery p s, KnownName s )
         => ParseReturn p ('ObjectRef s) where
  parseReturn vmap frmap _ s
    = RetObject <$> parseQuery (Proxy @p) (Proxy @s) vmap frmap s

class ParseSchema (s :: Schema') (t :: TypeDef Symbol Symbol) where
  parseSchema :: MonadError T.Text f
              => VariableMap
              -> FragmentMap
              -> T.Text
              -> [GQL.Selection]
              -> f (SchemaQuery s t)
instance ParseSchema sch ('DEnum name choices) where
  parseSchema _ _ _ []
    = pure QueryEnum
  parseSchema _ _ fname _
    = throwError $ "field '" <> fname <> "' should not have a selection of subfields"
instance (KnownName name, ParseField sch fields)
         => ParseSchema sch ('DRecord name fields) where
  parseSchema vmap frmap _ s
    = QueryRecord <$> parseSchemaQuery (Proxy @sch) (Proxy @('DRecord name fields)) vmap frmap s

parseSchemaQuery ::
  forall (sch :: Schema') t (rname :: Symbol) fields f.
  ( MonadError T.Text f
  , t ~  'DRecord rname fields
  , KnownName rname
  , ParseField sch fields ) =>
  Proxy sch ->
  Proxy t ->
  VariableMap -> FragmentMap -> [GQL.Selection] ->
  f [OneFieldQuery sch fields]
parseSchemaQuery _ _ _ _ [] = pure []
parseSchemaQuery pp ps vmap frmap (GQL.FieldSelection fld : ss)
  = (++) <$> (maybeToList <$> fieldToMethod fld)
         <*> parseSchemaQuery pp ps vmap frmap ss
  where
    fieldToMethod :: GQL.Field -> f (Maybe (OneFieldQuery sch fields))
    fieldToMethod (GQL.Field alias name args dirs sels _)
      | any (shouldSkip vmap) dirs
      = pure Nothing
      | name == "__typename"
      = case (args, sels) of
          ([], []) -> pure $ Just $ TypeNameFieldQuery alias
          _        -> throwError "__typename does not admit arguments nor selection of subfields"
      | _:_ <- args
      = throwError "this field does not support arguments"
      | otherwise
      = Just . OneFieldQuery alias
         <$> selectField (T.pack $ nameVal (Proxy @rname)) vmap frmap name sels
parseSchemaQuery pp ps vmap frmap (GQL.FragmentSpreadSelection (GQL.FragmentSpread nm dirs _) : ss)
  | Just fr <- HM.lookup nm frmap
  = if not (any (shouldSkip vmap) dirs) && not (any (shouldSkip vmap) $ fdDirectives fr)
       then (++) <$> parseSchemaQuery pp ps vmap frmap (fdSelectionSet fr)
                 <*> parseSchemaQuery pp ps vmap frmap ss
       else parseSchemaQuery pp ps vmap frmap ss
  | otherwise  -- the fragment definition was not found
  = throwError $ "fragment '" <> nm <> "' was not found"
parseSchemaQuery _ _ _ _ (_ : _)  -- Inline fragments are not yet supported
  = throwError "inline fragments are not (yet) supported"

class ParseField (sch :: Schema') (fs :: [FieldDef Symbol Symbol]) where
  selectField ::
    MonadError T.Text f =>
    T.Text ->
    VariableMap ->
    FragmentMap ->
    GQL.Name ->
    [GQL.Selection] ->
    f (NS (ChosenFieldQuery sch) fs)

instance ParseField sch '[] where
  selectField tyName _ _ wanted _
    = throwError $ "field '" <> wanted <> "' was not found on type '" <> tyName <> "'"
instance
  (KnownName fname, ParseField sch fs, ParseSchemaReturn sch r) =>
  ParseField sch ('FieldDef fname r ': fs)
  where
  selectField tyName vmap frmap wanted sels
    | wanted == mname
    = Z <$> (ChosenFieldQuery <$> parseSchemaReturn vmap frmap wanted sels)
    | otherwise
    = S <$> selectField tyName vmap frmap wanted sels
    where
      mname = T.pack $ nameVal (Proxy @fname)

class ParseSchemaReturn (sch :: Schema') (r :: FieldType Symbol) where
  parseSchemaReturn :: MonadError T.Text f
                    => VariableMap
                    -> FragmentMap
                    -> T.Text
                    -> [GQL.Selection]
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

-- some useful field accessors

fdName :: GQL.FragmentDefinition -> GQL.Name
fdName (GQL.FragmentDefinition nm _ _ _ _) = nm

fdDirectives :: GQL.FragmentDefinition -> [GQL.Directive]
fdDirectives (GQL.FragmentDefinition _ _ ds _ _) = ds

fdSelectionSet :: GQL.FragmentDefinition -> [GQL.Selection]
fdSelectionSet (GQL.FragmentDefinition _ _ _ ss _)
  = F.toList ss

argName :: GQL.Argument -> GQL.Name
argName (GQL.Argument nm _ _) = nm

fName :: GQL.Field -> GQL.Name
fName (GQL.Field _ nm _ _ _ _) = nm
