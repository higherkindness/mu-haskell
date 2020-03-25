{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
module Mu.GraphQL.Query.Introspection where

import           Control.Monad.Writer
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as S
import           Data.Int             (Int32)
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Proxy
import qualified Data.Text            as T
import           GHC.TypeLits
import           Mu.Rpc
import qualified Mu.Schema            as Mu

type TypeMap = HM.HashMap T.Text Type

data Schema
  = Schema { queryType        :: Maybe T.Text
           , mutationType     :: Maybe T.Text
           , subscriptionType :: Maybe T.Text
           , types            :: TypeMap }
  deriving Show

data Type
  = Type { kind       :: TypeKind
         , typeName   :: Maybe T.Text
         , fields     :: [Field]
         , enumValues :: [EnumValue]
         , ofType     :: Maybe Type }
  | TypeRef { to      :: T.Text }
  deriving Show

data Field
  = Field { fieldName :: T.Text
          , args      :: [Input]
          , fieldType :: Type }
  deriving Show

data Input
  = Input { inputName         :: T.Text
          , inputDefaultValue :: Maybe T.Text
          , inputType         :: Type }
  deriving Show

newtype EnumValue
  = EnumValue { enumValueName :: T.Text }
  deriving Show

data TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL
  deriving Show

tSimple :: T.Text -> Type
tSimple t = Type SCALAR (Just t) [] [] Nothing

tList :: Type -> Type
tList = Type LIST Nothing [] [] . Just

tNonNull :: Type -> Type
tNonNull = Type NON_NULL Nothing [] [] . Just

unwrapNonNull :: Type -> Maybe Type
unwrapNonNull (Type NON_NULL _ _ _ x) = x
unwrapNonNull _                       = Nothing

-- BUILD INTROSPECTION DATA
-- ========================

class Introspect (p :: Package')
                 (qr :: Maybe Symbol)
                 (mut :: Maybe Symbol)
                 (sub :: Maybe Symbol) where
  introspect
    :: Proxy p -> Proxy qr -> Proxy mut -> Proxy sub -> Schema

instance ( IntrospectServices ss sub
         , KnownMaybeSymbol qr
         , KnownMaybeSymbol mut
         , KnownMaybeSymbol sub)
         => Introspect ('Package nm ss) qr mut sub where
  introspect _ _ _ _
    = let (_, ts) = runWriter $
           introspectServices (Proxy @ss) (Proxy @sub) >>
           tell (HM.fromList (
             (\i -> (i, tSimple i)) <$> ["Null", "Int", "Float", "String", "Boolean", "ID"]))
          -- return only reachable types
          qrS  = maybeSymbolVal (Proxy @qr)
          mutS = maybeSymbolVal (Proxy @mut)
          subS = maybeSymbolVal (Proxy @sub)
          initials = S.fromList $ catMaybes [qrS, mutS, subS]
          reach = reachableFrom ts initials
          --
          finalTs = HM.filterWithKey (\k _ -> k `S.member` reach) ts
      in Schema qrS mutS subS finalTs

reachableFrom :: TypeMap -> S.HashSet T.Text -> S.HashSet T.Text
reachableFrom mp tys
  = let tys' = S.toList tys
        fromThis = map (S.fromList . reachableFromOne) tys'
        allReachable = S.unions fromThis
    in if tys == allReachable
          then tys
          else reachableFrom mp allReachable
  where
    reachableFromOne :: T.Text -> [T.Text]
    reachableFromOne t
      = case HM.lookup t mp of
          Just ty@Type {}
            -> t : concatMap reachableFromField (fields ty)
          _ -> error "this should never happen"

    reachableFromField :: Field -> [T.Text]
    reachableFromField f
      = reachableFromType (fieldType f) ++ concatMap reachableFromInput (args f)

    reachableFromInput :: Input -> [T.Text]
    reachableFromInput i = reachableFromType (inputType i)

    reachableFromType :: Type -> [T.Text]
    reachableFromType (TypeRef t) = [t]
    reachableFromType t@Type {}
      = case ofType t of
          Just t' -> reachableFromType t'
          Nothing -> case typeName t of
            Just tn -> [tn]
            Nothing -> []

class KnownMaybeSymbol (s :: Maybe Symbol) where
  maybeSymbolVal :: Proxy s -> Maybe T.Text
instance KnownSymbol s => KnownMaybeSymbol ('Just s) where
  maybeSymbolVal _ = Just $ T.pack $ symbolVal (Proxy @s)
instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal _ = Nothing

type family IsSub (sname :: Symbol) (sub :: Maybe Symbol) :: Bool where
  IsSub sname 'Nothing      = 'False
  IsSub sname ('Just sname) = 'True
  IsSub sname ('Just other) = 'False

class IntrospectServices (ss :: [Service']) (sub :: Maybe Symbol) where
  introspectServices
    :: Proxy ss -> Proxy sub -> Writer TypeMap ()
instance IntrospectServices '[] sub where
  introspectServices _ _ = pure ()
instance ( KnownSymbol sname
         , IntrospectFields smethods (IsSub sname sub)
         , IntrospectServices ss sub )
         => IntrospectServices ('Service sname sanns smethods ': ss) sub where
  introspectServices _ psub
    = do let name = T.pack $ symbolVal (Proxy @sname)
         fs <- introspectFields (Proxy @smethods) (Proxy @(IsSub sname sub))
         let t = Type OBJECT (Just name) fs [] Nothing
         -- add this one to the mix
         tell (HM.singleton name t)
         -- continue with the rest
         introspectServices (Proxy @ss) psub

class IntrospectFields (fs :: [Method']) (isSub :: Bool) where
  introspectFields
    :: Proxy fs -> Proxy isSub -> Writer TypeMap [Field]
instance IntrospectFields '[] isSub where
  introspectFields _ _ = pure []
instance ( KnownSymbol mname
         , IntrospectInputs margs
         , IntrospectReturn mret isSub
         , IntrospectFields fs isSub)
         => IntrospectFields ('Method mname manns margs mret ': fs) isSub where
  introspectFields _ pIsSub
    = do let name = T.pack $ symbolVal (Proxy @mname)
         inputs <- introspectInputs (Proxy @margs)
         ret <- introspectReturn (Proxy @mret) pIsSub
         let this = Field name inputs ret
         (this :) <$> introspectFields (Proxy @fs) pIsSub

class IntrospectInputs (args :: [Argument']) where
  introspectInputs
    :: Proxy args -> Writer TypeMap [Input]
instance IntrospectInputs '[] where
  introspectInputs _ = pure []
instance ( KnownMaybeSymbol nm
         , IntrospectTypeRef r
         , IntrospectInputs args )
         => IntrospectInputs ('ArgSingle nm anns r ': args) where
  introspectInputs _
    = do let nm = maybeSymbolVal (Proxy @nm)
         t <- introspectTypeRef (Proxy @r) False
         -- TODO Find default value
         let this = Input (fromMaybe "arg" nm) Nothing t
         (this :) <$> introspectInputs (Proxy @args)
instance ( KnownMaybeSymbol nm
         , IntrospectTypeRef r
         , IntrospectInputs args )
         => IntrospectInputs ('ArgStream nm anns r ': args) where
  introspectInputs _
    = do let nm = maybeSymbolVal (Proxy @nm)
         t <- tList <$> introspectTypeRef (Proxy @r) False
         -- TODO Find default value
         let this = Input (fromMaybe "arg" nm) Nothing t
         (this :) <$> introspectInputs (Proxy @args)

class IntrospectReturn (r :: Return Symbol) (isSub :: Bool) where
  introspectReturn
    :: Proxy r -> Proxy isSub -> Writer TypeMap Type

instance IntrospectReturn 'RetNothing isSub where
  introspectReturn _ _ = pure $ tSimple "Null"
instance IntrospectTypeRef t
         => IntrospectReturn ('RetSingle t) isSub where
  introspectReturn _ _ = introspectTypeRef (Proxy @t) True
instance IntrospectTypeRef t
         => IntrospectReturn ('RetStream t) 'False where
  introspectReturn _ _ = tList <$> introspectTypeRef (Proxy @t) True
instance IntrospectTypeRef t
         => IntrospectReturn ('RetStream t) 'True where
  introspectReturn _ _ = introspectTypeRef (Proxy @t) True

class IntrospectTypeRef (tr :: TypeRef Symbol) where
  introspectTypeRef
    :: Proxy tr -> Bool -> Writer TypeMap Type

instance IntrospectTypeRef ('PrimitiveRef Bool) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "Boolean"
instance IntrospectTypeRef ('PrimitiveRef Int32) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "Int"
instance IntrospectTypeRef ('PrimitiveRef Integer) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "Int"
instance IntrospectTypeRef ('PrimitiveRef Double) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "Float"
instance IntrospectTypeRef ('PrimitiveRef String) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "String"
instance IntrospectTypeRef ('PrimitiveRef T.Text) where
  introspectTypeRef _ _ = pure $ tNonNull $ tSimple "String"

instance (IntrospectTypeRef r)
         => IntrospectTypeRef ('ListRef r) where
  introspectTypeRef _ isRet = tList <$> introspectTypeRef (Proxy @r) isRet
instance (IntrospectTypeRef r)
         => IntrospectTypeRef ('OptionalRef r) where
  introspectTypeRef _ isRet = do
    r <- introspectTypeRef (Proxy @r) isRet
    pure $ fromMaybe r (unwrapNonNull r)

instance (KnownSymbol o)
         => IntrospectTypeRef ('ObjectRef o) where
  introspectTypeRef _ _
    = pure $ TypeRef $ T.pack $ symbolVal (Proxy @o)

instance (IntrospectSchema sch, KnownSymbol t)
         => IntrospectTypeRef ('SchemaRef sch t) where
  introspectTypeRef _ isRet
    = do let (k, suffix) = if isRet then (OBJECT, "R") else (INPUT_OBJECT, "")
         introspectSchema k suffix (Proxy @sch)
         pure $ TypeRef $ T.pack (symbolVal (Proxy @t)) <> suffix

class IntrospectSchema (ts :: [Mu.TypeDef Symbol Symbol]) where
  introspectSchema
    :: TypeKind -> T.Text -> Proxy ts -> Writer TypeMap ()
instance IntrospectSchema '[] where
  introspectSchema _ _ _ = pure ()
instance (KnownSymbol name, IntrospectSchemaFields fields, IntrospectSchema ts)
         => IntrospectSchema ('Mu.DRecord name fields ': ts) where
  introspectSchema k suffix _
    = do let name = T.pack (symbolVal (Proxy @name)) <> suffix
             fs   = introspectSchemaFields suffix (Proxy @fields)
             t    = Type k (Just name) fs [] Nothing
         -- add this one to the mix
         tell (HM.singleton name t)
         -- continue with the rest
         introspectSchema k suffix (Proxy @ts)
instance (KnownSymbol name, IntrospectSchemaEnum choices, IntrospectSchema ts)
         => IntrospectSchema ('Mu.DEnum name choices ': ts) where
  introspectSchema k suffix _
    = do let name = T.pack (symbolVal (Proxy @name)) <> suffix
             cs   = introspectSchemaEnum (Proxy @choices)
             t    = Type ENUM (Just name) [] cs Nothing
         -- add this one to the mix
         tell (HM.singleton name t)
         -- continue with the rest
         introspectSchema k suffix (Proxy @ts)

class IntrospectSchemaFields (fs :: [Mu.FieldDef Symbol Symbol]) where
  introspectSchemaFields
    :: T.Text -> Proxy fs -> [Field]
instance IntrospectSchemaFields '[] where
  introspectSchemaFields _ _ = []
instance (KnownSymbol fname,IntrospectSchemaFieldType r, IntrospectSchemaFields fs)
         => IntrospectSchemaFields ('Mu.FieldDef fname r ': fs) where
  introspectSchemaFields suffix _
    = let name = T.pack $ symbolVal (Proxy @fname)
          ret  = introspectSchemaFieldType suffix (Proxy @r)
          this = Field name [] ret
      in this : introspectSchemaFields suffix (Proxy @fs)

class IntrospectSchemaFieldType (t :: Mu.FieldType Symbol) where
  introspectSchemaFieldType
    :: T.Text -> Proxy t -> Type

instance IntrospectSchemaFieldType ('Mu.TPrimitive Bool) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "Boolean"
instance IntrospectSchemaFieldType ('Mu.TPrimitive Int32) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "Int"
instance IntrospectSchemaFieldType ('Mu.TPrimitive Integer) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "Int"
instance IntrospectSchemaFieldType ('Mu.TPrimitive Double) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "Float"
instance IntrospectSchemaFieldType ('Mu.TPrimitive String) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "String"
instance IntrospectSchemaFieldType ('Mu.TPrimitive T.Text) where
  introspectSchemaFieldType _ _ = tNonNull $ tSimple "String"

instance (IntrospectSchemaFieldType r)
         => IntrospectSchemaFieldType ('Mu.TList r) where
  introspectSchemaFieldType suffix _
    = tList $ introspectSchemaFieldType suffix (Proxy @r)
instance (IntrospectSchemaFieldType r)
         => IntrospectSchemaFieldType ('Mu.TOption r) where
  introspectSchemaFieldType suffix _
    = let r = introspectSchemaFieldType suffix (Proxy @r)
      in fromMaybe r (unwrapNonNull r)

instance (KnownSymbol nm)
         => IntrospectSchemaFieldType ('Mu.TSchematic nm) where
  introspectSchemaFieldType suffix _
    = TypeRef $ T.pack (symbolVal (Proxy @nm)) <> suffix

class IntrospectSchemaEnum (c :: [Mu.ChoiceDef Symbol]) where
  introspectSchemaEnum :: Proxy c -> [EnumValue]
instance IntrospectSchemaEnum '[] where
  introspectSchemaEnum _ = []
instance (KnownSymbol nm, IntrospectSchemaEnum cs)
         => IntrospectSchemaEnum ('Mu.ChoiceDef nm ': cs) where
  introspectSchemaEnum _
    = let this = EnumValue $ T.pack $ symbolVal (Proxy @nm)
      in this : introspectSchemaEnum (Proxy @cs)
