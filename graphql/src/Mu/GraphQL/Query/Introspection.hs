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
import           Data.Int             (Int32)
import           Data.Maybe           (fromMaybe)
import           Data.Proxy
import           Data.Text            as T
import           GHC.TypeLits
import           Mu.Rpc
import qualified Mu.Schema            as Mu


type TypeMap = HM.HashMap T.Text Type

data Schema
  = Schema { queryType        :: Maybe T.Text
           , mutationType     :: Maybe T.Text
           , subscriptionType :: Maybe T.Text
           , types            :: TypeMap }

data Type
  = Type { kind       :: TypeKind
         , typeName   :: Maybe T.Text
         , fields     :: [Field]
         , enumValues :: [EnumValue]
         , ofType     :: Maybe Type }
  | TypeRef { to      :: T.Text }

data Field
  = Field { fieldName :: T.Text
          , args      :: [Input]
          , fieldType :: Type }

data Input
  = Input { inputName         :: T.Text
          , inputDefaultValue :: Maybe T.Text
          , inputType         :: Type }

newtype EnumValue
  = EnumValue { enumValueName :: T.Text }

data TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM
  | INPUT_OBJECT
  | LIST
  | NON_NULL

tSimple :: T.Text -> Type
tSimple t = Type SCALAR (Just t) [] [] Nothing

tList :: Type -> Type
tList = Type LIST Nothing [] [] . Just

tNonNull :: Type -> Type
tNonNull = Type NON_NULL Nothing [] [] . Just

unwrapNonNull :: Type -> Maybe Type
unwrapNonNull (Type NON_NULL _ _ _ x) = x
unwrapNonNull _                       = Nothing

class Introspect (p :: Package')
                 (qr :: Maybe Symbol)
                 (mut :: Maybe Symbol)
                 (sub :: Maybe Symbol) where
  introspect
    :: Proxy p -> Proxy qr -> Proxy mut -> Proxy sub -> Schema

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
         t <- introspectTypeRef (Proxy @r)
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
  introspectReturn _ _ = introspectTypeRef (Proxy @t)
instance IntrospectTypeRef t
         => IntrospectReturn ('RetStream t) 'False where
  introspectReturn _ _ = tList <$> introspectTypeRef (Proxy @t)
instance IntrospectTypeRef t
         => IntrospectReturn ('RetStream t) 'True where
  introspectReturn _ _ = introspectTypeRef (Proxy @t)

class IntrospectTypeRef (tr :: TypeRef Symbol) where
  introspectTypeRef
    :: Proxy tr -> Writer TypeMap Type

instance IntrospectTypeRef ('PrimitiveRef Bool) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "Boolean"
instance IntrospectTypeRef ('PrimitiveRef Int32) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "Int"
instance IntrospectTypeRef ('PrimitiveRef Integer) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "Int"
instance IntrospectTypeRef ('PrimitiveRef Double) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "Float"
instance IntrospectTypeRef ('PrimitiveRef String) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "String"
instance IntrospectTypeRef ('PrimitiveRef Text) where
  introspectTypeRef _ = pure $ tNonNull $ tSimple "Text"

instance (IntrospectTypeRef r)
         => IntrospectTypeRef ('ListRef r) where
  introspectTypeRef _ = tList <$> introspectTypeRef (Proxy @r)
instance (IntrospectTypeRef r)
         => IntrospectTypeRef ('OptionalRef r) where
  introspectTypeRef _ = do
    r <- introspectTypeRef (Proxy @r)
    pure $ fromMaybe r (unwrapNonNull r)

instance (KnownSymbol o)
         => IntrospectTypeRef ('ObjectRef o) where
  introspectTypeRef _ = pure $ TypeRef $ T.pack $ symbolVal (Proxy @o)
