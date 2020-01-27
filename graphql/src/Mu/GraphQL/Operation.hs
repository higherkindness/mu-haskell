{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language GADTs                 #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
module Mu.GraphQL.Operation where

import           Data.Functor.Identity
import           Data.Kind
import           Data.SOP.NP
import           Mu.Schema

data Operation opName mtName inSchName oSchName
  = Operation (Maybe opName) [Method mtName inSchName oSchName]

data Method mtName inSchName oSchName
  = Method mtName [inSchName] oSchName

newtype TTerm w sch ty
  = TTerm { unTTerm :: Term w sch (sch :/: ty) }

data OpResolver m (inSch :: Schema inSchName inSchFn) (oSch :: Schema oSchName  oSchFn)
                (op :: Operation opName mtName inSchName oSchName) where
  OR :: NP (MethodResolver m inSch oSch) ms
     -> OpResolver m inSch oSch ('Operation oName ms)

data MethodResolver m (inSch :: Schema inSchName inSchFn) (oSch :: Schema oSchName  oSchFn)
                    (mt :: Method mtName inSchName oSchName) where
  MR :: (NP (TTerm Identity inSch) args -> m (TTerm Identity oSch result))
     -> MethodResolver m inSch oSch ('Method mtName args result)

-- COMPOSABLE OPERATIONS OVER DOMAIN TYPES
-- =======================================

data OpResolverD m (inSch :: Schema inSchName inSchFn) (oSch :: Schema oSchName oSchFn)
                 (op :: Operation opName mtName inSchName oSchName) where
  OR_ :: NP (MethodResolverD m inSch oSch) ms
      -> OpResolverD m inSch oSch ('Operation oName ms)

data MethodResolverD m (inSch :: Schema inSchName inSchFn) (oSch :: Schema oSchName oSchFn)
                     (mt :: Method mtName inSchName oSchName) where
  MR_ :: MethodHandlerD m h inSch oSch args result
      => h -> MethodResolverD m inSch oSch ('Method mtName args result)

class MethodHandlerD (m :: Type -> Type) (h :: Type)
                     (inSch :: Schema inSchName inSchFn)
                     (oSch  :: Schema oSchName  oSchFn)
                     (args :: [inSchName]) (result :: oSchName) where
  executeMethodHandler :: h -> NP (TTerm Identity inSch) args -> m (TTerm Identity oSch result)
instance ( FromSchema Identity inSch arg v
         , MethodHandlerD m h inSch oSch args result )
         => MethodHandlerD m (v -> h) inSch oSch (arg ': args) result where
  executeMethodHandler h (x :* xs)
    = let v = fromSchema @_ @_ @Identity @inSch @arg (unTTerm x)
      in executeMethodHandler (h v) xs
instance ( Functor m, ToSchema Identity oSch result r )
         => MethodHandlerD m (m r) inSch oSch '[] result where
  executeMethodHandler h Nil
    = TTerm . toSchema @_ @_ @Identity @oSch @result <$> h

operationDomain
  :: forall m inS oS op.
     Functor m
  => OpResolverD m inS oS op
  -> OpResolver  m inS oS op
operationDomain (OR_ x) = OR (go x)
  where
    go :: forall ms.
          NP (MethodResolverD m inS oS) ms
       -> NP (MethodResolver  m inS oS) ms
    go Nil           = Nil
    go (MR_ m :* ms) = MR (executeMethodHandler m) :* go ms
