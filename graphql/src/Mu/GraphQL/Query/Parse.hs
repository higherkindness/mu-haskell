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

module Mu.GraphQL.Query.Parse where

import           Control.Applicative
import qualified Data.HashMap.Strict           as HM
import           Data.Int                      (Int32)
import           Data.Kind
import           Data.List                     (find)
import           Data.Maybe
import           Data.Proxy
import           Data.SOP.NS
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Query.Definition
import           Mu.Rpc
import           Mu.Schema

type VariableMapC = HM.HashMap T.Text GQL.ValueConst
type VariableMap = HM.HashMap T.Text GQL.Value
type FragmentMap = HM.HashMap T.Text GQL.FragmentDefinition

parseDoc ::
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    ParseMethod p mmethods
  ) =>
  Maybe T.Text -> VariableMapC ->
  GQL.ExecutableDocument ->
  f (Document p qr mut)
-- If there's no operation name, there must be only one query
parseDoc Nothing _ (GQL.ExecutableDocument defns)
  = case GQL.partitionExDefs defns of
      ([unnamed], [], frs)
        -> QueryDoc <$> parseQuery Proxy Proxy HM.empty (fragmentsToMap frs) unnamed
      ([], [named], frs)
        -> parseTypedDoc HM.empty (fragmentsToMap frs) named
      _ -> empty
-- If there's an operation name, look in the named queries
parseDoc (Just operationName) vmap (GQL.ExecutableDocument defns)
  = case GQL.partitionExDefs defns of
      (_, named, frs) -> maybe empty (parseTypedDoc vmap (fragmentsToMap frs)) (find isThis named)
    where isThis (GQL._todName -> Just nm)
            = GQL.unName nm == operationName
          isThis _ = False

fragmentsToMap :: [GQL.FragmentDefinition] -> FragmentMap
fragmentsToMap = HM.fromList . map fragmentToThingy
  where fragmentToThingy :: GQL.FragmentDefinition -> (T.Text, GQL.FragmentDefinition)
        fragmentToThingy f = (GQL.unName $ GQL._fdName f, f)

parseTypedDoc ::
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss qr ~ 'Service qr qanns qmethods,
    ParseMethod p qmethods,
    LookupService ss mut ~ 'Service mut manns mmethods,
    ParseMethod p mmethods
  ) =>
  VariableMapC -> FragmentMap ->
  GQL.TypedOperationDefinition ->
  f (Document p qr mut)
parseTypedDoc vmap frmap tod
  = let defVmap = parseVariableMap (GQL._todVariableDefinitions tod)
        finalVmap = constToValue <$> HM.union vmap defVmap  -- first one takes precedence
    in case GQL._todType tod of
         GQL.OperationTypeQuery
           -> QueryDoc <$> parseQuery Proxy Proxy finalVmap frmap (GQL._todSelectionSet tod)
         GQL.OperationTypeMutation
           -> MutationDoc <$> parseQuery Proxy Proxy finalVmap frmap (GQL._todSelectionSet tod)
         _ -> empty

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
  ( Alternative f, p ~ 'Package pname ss,
    LookupService ss s ~ 'Service s sanns methods,
    ParseMethod p methods
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
         <$> selectMethod vmap frmap name args sels
parseQuery pp ps vmap frmap (GQL.SelectionFragmentSpread (GQL.FragmentSpread nm dirs) : ss)
  | Just fr <- HM.lookup (GQL.unName nm) frmap
  = if not (any (shouldSkip vmap) dirs) && not (any (shouldSkip vmap) $ GQL._fdDirectives fr)
       then (++) <$> parseQuery pp ps vmap frmap (GQL._fdSelectionSet fr)
                 <*> parseQuery pp ps vmap frmap ss
       else parseQuery pp ps vmap frmap ss
  | otherwise  -- the fragment definition was not found
  = empty
parseQuery _ _ _ _ (_ : _)  -- Inline fragments are not yet supported
  = empty

shouldSkip :: VariableMap -> GQL.Directive -> Bool
shouldSkip vmap (GQL.Directive (GQL.unName -> nm) [GQL.Argument (GQL.unName -> ifn) v])
  | nm == "skip", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap v of
      Just (FPrimitive b) -> b
      Nothing             -> False
  | nm == "include", ifn == "if"
  = case valueParser' @'[] @('TPrimitive Bool) vmap v of
      Just (FPrimitive b) -> not b
      Nothing             -> False
shouldSkip _ _ = False

class ParseMethod (p :: Package') (ms :: [Method']) where
  selectMethod ::
    Alternative f =>
    VariableMap ->
    FragmentMap ->
    GQL.Name ->
    [GQL.Argument] ->
    GQL.SelectionSet ->
    f (NS (ChosenMethodQuery p) ms)

instance ParseMethod p '[] where
  selectMethod _ _ _ _ _ = empty
instance
  (KnownSymbol mname, ParseMethod p ms, ParseArgs p args, ParseReturn p r) =>
  ParseMethod p ('Method mname manns args ('RetSingle r) ': ms)
  where
  selectMethod vmap frmap w@(GQL.unName -> wanted) args sels
    | wanted == mname
    = Z <$> (ChosenMethodQuery <$> parseArgs vmap args
                               <*> parseReturn vmap frmap sels)
    | otherwise
    = S <$> selectMethod vmap frmap w args sels
    where
      mname = T.pack $ nameVal (Proxy @mname)

class ParseArgs (p :: Package') (args :: [Argument']) where
  parseArgs :: Alternative f
            => VariableMap
            -> [GQL.Argument]
            -> f (NP (ArgumentValue p) args)

instance ParseArgs p '[] where
  parseArgs _ _ = pure Nil
instance (KnownName aname, ParseArg p a, ParseArgs p as, FindDefaultArgValue aanns)
         => ParseArgs p ('ArgSingle ('Just aname) aanns a ': as) where
  parseArgs vmap args
    = case find ((== nameVal (Proxy @aname)) . T.unpack . GQL.unName . GQL._aName) args of
        Just (GQL.Argument _ x)
          -> (:*) <$> (ArgumentValue <$> parseArg' vmap x)
                  <*> parseArgs vmap args
        Nothing -> case findDefaultArgValue (Proxy @aanns) of
                     Just x  -> (:*) <$> (ArgumentValue <$> parseArg' vmap (constToValue x))
                                     <*> parseArgs vmap args
                     Nothing -> empty

class FindDefaultArgValue (vs :: [Type]) where
  findDefaultArgValue :: Alternative f
                      => Proxy vs
                      -> f GQL.ValueConst
instance FindDefaultArgValue '[] where
  findDefaultArgValue _ = empty
instance {-# OVERLAPPABLE #-} FindDefaultArgValue xs
         => FindDefaultArgValue (x ': xs) where
  findDefaultArgValue _ = findDefaultArgValue (Proxy @xs)
instance {-# OVERLAPS #-} ReflectValueConst v
         => FindDefaultArgValue (DefaultValue v ': xs) where
  findDefaultArgValue _ = pure $ reflectValueConst (Proxy @v)

parseArg' :: (ParseArg p a, Alternative f)
          => VariableMap
          -> GQL.Value
          -> f (ArgumentValue' p a)
parseArg' vmap (GQL.VVariable x)
  = case HM.lookup (GQL.unName (GQL.unVariable x)) vmap of
      Nothing -> empty
      Just v  -> parseArg vmap v
parseArg' vmap v = parseArg vmap v

class ParseArg (p :: Package') (a :: TypeRef Symbol) where
  parseArg :: Alternative f
           => VariableMap
           -> GQL.Value
           -> f (ArgumentValue' p a)

instance (ParseArg p r) => ParseArg p ('ListRef r) where
  parseArg vmap (GQL.VList (GQL.ListValueG xs)) = ArgList <$> traverse (parseArg' vmap) xs
  parseArg _ _                                  = empty
instance ParseArg p ('PrimitiveRef Bool) where
  parseArg _ (GQL.VBoolean b) = pure (ArgPrimitive b)
  parseArg _ _                = empty
instance ParseArg p ('PrimitiveRef Int32) where
  parseArg _ (GQL.VInt b) = pure (ArgPrimitive b)
  parseArg _ _            = empty
instance ParseArg p ('PrimitiveRef Integer) where
  parseArg _ (GQL.VInt b) = pure $ ArgPrimitive $ fromIntegral b
  parseArg _ _            = empty
instance ParseArg p ('PrimitiveRef Double) where
  parseArg _ (GQL.VFloat b) = pure (ArgPrimitive b)
  parseArg _ _              = empty
instance ParseArg p ('PrimitiveRef T.Text) where
  parseArg _ (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive b
  parseArg _ _                                 = empty
instance ParseArg p ('PrimitiveRef String) where
  parseArg _ (GQL.VString (GQL.StringValue b)) = pure $ ArgPrimitive $ T.unpack b
  parseArg _ _                                 = empty
instance ParseArg p ('PrimitiveRef ()) where
  parseArg _ GQL.VNull = pure $ ArgPrimitive ()
  parseArg _ _         = empty
instance (ObjectOrEnumParser sch (sch :/: sty))
         => ParseArg p ('SchemaRef sch sty) where
  parseArg vmap v = ArgSchema <$> parseObjectOrEnum' vmap v

parseObjectOrEnum' :: (ObjectOrEnumParser sch t, Alternative f)
          => VariableMap
          -> GQL.Value
          -> f (Term sch t)
parseObjectOrEnum' vmap (GQL.VVariable x)
  = case HM.lookup (GQL.unName (GQL.unVariable x)) vmap of
      Nothing -> empty
      Just v  -> parseObjectOrEnum vmap v
parseObjectOrEnum' vmap v = parseObjectOrEnum vmap v

class ObjectOrEnumParser (sch :: Schema') (t :: TypeDef Symbol Symbol) where
  parseObjectOrEnum :: Alternative f
                    => VariableMap
                    -> GQL.Value
                    -> f (Term sch t)

instance (ObjectParser sch args)
         => ObjectOrEnumParser sch ('DRecord name args) where
  parseObjectOrEnum vmap (GQL.VObject (GQL.ObjectValueG vs)) = TRecord <$> objectParser vmap vs
  parseObjectOrEnum _    _                                   = empty
instance (EnumParser choices)
         => ObjectOrEnumParser sch ('DEnum name choices) where
  parseObjectOrEnum _ (GQL.VEnum (GQL.EnumValue nm)) = TEnum <$> enumParser nm
  parseObjectOrEnum _ _                              = empty

class ObjectParser (sch :: Schema') (args :: [FieldDef Symbol Symbol]) where
  objectParser :: Alternative f
               => VariableMap
               -> [GQL.ObjectFieldG GQL.Value]
               -> f (NP (Field sch) args)

instance ObjectParser sch '[] where
  objectParser _ _ = pure Nil
instance
  (ObjectParser sch args, ValueParser sch v, KnownName nm) =>
  ObjectParser sch ('FieldDef nm v ': args)
  where
  objectParser vmap args
    = case find ((== nameVal (Proxy @nm)) . T.unpack . GQL.unName . GQL._ofName) args of
        Just (GQL.ObjectFieldG _ v)
          -> (:*) <$> (Field <$> valueParser' vmap v) <*> objectParser vmap args
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

valueParser' :: (ValueParser sch v, Alternative f)
             => VariableMap
             -> GQL.Value
             -> f (FieldValue sch v)
valueParser' vmap (GQL.VVariable x)
  = case HM.lookup (GQL.unName (GQL.unVariable x)) vmap of
      Nothing -> empty
      Just v  -> valueParser vmap v
valueParser' vmap v = valueParser vmap v

class ValueParser (sch :: Schema') (v :: FieldType Symbol) where
  valueParser :: Alternative f
              => VariableMap
              -> GQL.Value
              -> f (FieldValue sch v)

instance ValueParser sch 'TNull where
  valueParser _ GQL.VNull = pure FNull
  valueParser _ _         = empty
instance ValueParser sch ('TPrimitive Bool) where
  valueParser _ (GQL.VBoolean b) = pure (FPrimitive b)
  valueParser _ _                = empty
instance ValueParser sch ('TPrimitive Int32) where
  valueParser _ (GQL.VInt b) = pure (FPrimitive b)
  valueParser _ _            = empty
instance ValueParser sch ('TPrimitive Integer) where
  valueParser _ (GQL.VInt b) = pure $ FPrimitive $ fromIntegral b
  valueParser _ _            = empty
instance ValueParser sch ('TPrimitive Double) where
  valueParser _ (GQL.VFloat b) = pure (FPrimitive b)
  valueParser _ _              = empty
instance ValueParser sch ('TPrimitive T.Text) where
  valueParser _ (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive b
  valueParser _ _                                 = empty
instance ValueParser sch ('TPrimitive String) where
  valueParser _ (GQL.VString (GQL.StringValue b)) = pure $ FPrimitive $ T.unpack b
  valueParser _ _                                 = empty
instance (ValueParser sch r) => ValueParser sch ('TList r) where
  valueParser vmap (GQL.VList (GQL.ListValueG xs)) = FList <$> traverse (valueParser' vmap) xs
  valueParser _    _                               = empty
instance (ObjectOrEnumParser sch (sch :/: sty))
         => ValueParser sch ('TSchematic sty) where
  valueParser vmap v = FSchematic <$> parseObjectOrEnum' vmap v

class ParseReturn (p :: Package') (r :: TypeRef Symbol) where
  parseReturn :: Alternative f
              => VariableMap
              -> FragmentMap
              -> GQL.SelectionSet
              -> f (ReturnQuery p r)

instance ParseReturn p ('PrimitiveRef t) where
  parseReturn _ _ [] = pure RetPrimitive
  parseReturn _ _ _  = empty
instance ParseReturn p ('SchemaRef sch sty) where
  parseReturn _ _ _ = pure RetSchema
instance ParseReturn p r
         => ParseReturn p ('ListRef r) where
  parseReturn vmap frmap s = RetList <$> parseReturn vmap frmap s
instance ParseReturn p r
         => ParseReturn p ('OptionalRef r) where
  parseReturn vmap frmap s = RetOptional <$> parseReturn vmap frmap s
instance ( p ~ 'Package pname ss,
           LookupService ss s ~ 'Service s sanns methods,
           ParseMethod p methods
         ) => ParseReturn p ('ObjectRef s) where
  parseReturn vmap frmap s = RetObject <$> parseQuery (Proxy @p) (Proxy @s) vmap frmap s
