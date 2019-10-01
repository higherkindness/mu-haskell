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

-- Arguments
instance (HasSchema sch sty t, Handles args ret m h)
         => Handles ('ArgSingle '(sch,sty) ': args) ret m
                    (t -> h)
instance (HasSchema sch sty t, Handles args ret m h)
         => Handles ('ArgStream '(sch,sty) ': args) ret m
                    (ConduitT () t m () -> h)
-- Result with exception
instance (HasSchema esch ety e, HasSchema vsch vty v)
         => Handles '[] ('RetThrows '(esch,ety) '(vsch,vty)) m
                    (m (Either e v))
instance (HasSchema vsch vty v)
         => Handles '[] ('RetSingle '(vsch,vty)) m (m v)
instance (HasSchema vsch vty v)
         => Handles '[] ('RetStream '(vsch,vty)) m (ConduitT v Void m ())