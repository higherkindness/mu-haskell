{-# language DataKinds, PolyKinds,
             GADTs, TypeFamilies,
             ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             ConstraintKinds,
             RankNTypes #-}
module Mu.Server where

import Data.Conduit
import Data.Kind

import Mu.Rpc
import Mu.Schema

data ServerT (s :: Service snm mnm) (m :: Type -> Type) (hs :: [Type]) where
  Server :: HandlersT methods m hs -> ServerT ('Service sname methods) m hs
type ServerIO service = ServerT service IO

infixr 5 :<|>:
data HandlersT (methods :: [Method mnm]) (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT '[] m '[]
  (:<|>:) :: Handles args ret m h => h -> HandlersT ms m hs
          -> HandlersT ('Method name args ret ': ms) m (h ': hs)
type HandlersIO methods = HandlersT methods IO

-- Define a relation for handling
class Handles (args :: [Argument]) (ret :: Return)
              (m :: Type -> Type) (h :: Type)
class HandlesRef (ref :: TypeRef) (t :: Type)

-- Type references
instance HasSchema sch sty t => HandlesRef ('FromSchema sch sty) t
instance HandlesRef ('FromRegistry subject t last) t

-- Arguments
instance (HandlesRef ref t, Handles args ret m h)
         => Handles ('ArgSingle ref ': args) ret m
                    (t -> h)
instance (HandlesRef ref t, Handles args ret m h)
         => Handles ('ArgStream ref ': args) ret m
                    (ConduitT () t IO () -> h)
-- Result with exception
instance Handles '[] 'RetNothing m (m ())
instance (HandlesRef eref e, HandlesRef vref v)
         => Handles '[] ('RetThrows eref vref) m
                    (m (Either e v))
instance (HandlesRef ref v)
         => Handles '[] ('RetSingle ref) m (m v)
instance (HandlesRef ref v)
         => Handles '[] ('RetStream ref) m (ConduitT v Void m () -> m ())