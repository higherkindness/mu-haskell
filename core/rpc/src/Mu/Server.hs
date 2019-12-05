{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts          #-}
{-# language FlexibleInstances         #-}
{-# language GADTs                     #-}
{-# language MultiParamTypeClasses     #-}
{-# language PolyKinds                 #-}
{-# language RankNTypes                #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
-- | Protocol-independent declaration of servers.
--
--   A server (represented by 'ServerIO' and in general
--   by 'ServerT') is a sequence of handlers (represented
--   by 'HandlersIO' and 'HandlersT'), one for each
--   operation in the corresponding Mu service declaration.
--
--   In general, you should declare a server as:
--
--   > server :: ServerIO MyService _
--   > server = Server (h1 :<|>: h2 :<|>: ... :<|>: H0)
--
--   where each of @h1@, @h2@, ... handles each method in
--   @MyService@ /in the order they were declared/.
--   The @_@ in the type allows GHC to fill in the boring
--   and long type you would need to write there otherwise.
module Mu.Server (
  -- * Servers and handlers
  MonadServer, ServerT(..), HandlersT(..)
  -- ** Simple servers using only IO
, ServerErrorIO, ServerIO
  -- * Errors which might be raised
, serverError, ServerError(..), ServerErrorCode(..)
  -- ** Useful when you do not want to deal with errors
, alwaysOk
) where

import           Control.Monad.Except
import           Data.Conduit
import           Data.Kind

import           Mu.Rpc
import           Mu.Schema

-- | Constraint for monads that can be used as servers
type MonadServer m = (MonadError ServerError m, MonadIO m)
type ServerErrorIO = ExceptT ServerError IO
type ServerIO srv = ServerT srv ServerErrorIO

serverError :: (MonadError ServerError m)
            => ServerError -> m a
serverError = throwError

alwaysOk :: (MonadIO m)
         => IO a -> m a
alwaysOk = liftIO

data ServerError
  = ServerError ServerErrorCode String

data ServerErrorCode
  = Unknown
  | Unavailable
  | Unimplemented
  | Unauthenticated
  | Internal
  | Invalid
  | NotFound
  deriving (Eq, Show)

data ServerT (s :: Service snm mnm) (m :: Type -> Type) (hs :: [Type]) where
  Server :: HandlersT methods m hs -> ServerT ('Service sname anns methods) m hs

infixr 5 :<|>:
data HandlersT (methods :: [Method mnm]) (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT '[] m '[]
  (:<|>:) :: Handles args ret m h => h -> HandlersT ms m hs
          -> HandlersT ('Method name anns args ret ': ms) m (h ': hs)

-- Define a relation for handling
class Handles (args :: [Argument]) (ret :: Return)
              (m :: Type -> Type) (h :: Type)
class HandlesRef (ref :: TypeRef) (t :: Type)

-- Type references
instance HasSchema sch sty t => HandlesRef ('FromSchema sch sty) t
instance HandlesRef ('FromRegistry subject t last) t

-- Arguments
instance (HandlesRef ref t, Handles args ret m h,
          handler ~ (t -> h))
         => Handles ('ArgSingle ref ': args) ret m handler
instance (MonadError ServerError m, HandlesRef ref t, Handles args ret m h,
          handler ~ (ConduitT () t m () -> h))
         => Handles ('ArgStream ref ': args) ret m handler
-- Result with exception
instance (MonadError ServerError m, handler ~ m ())
         => Handles '[] 'RetNothing m handler
instance (MonadError ServerError m, HandlesRef eref e, HandlesRef vref v, handler ~ m (Either e v))
         => Handles '[] ('RetThrows eref vref) m handler
instance (MonadError ServerError m, HandlesRef ref v, handler ~ m v)
         => Handles '[] ('RetSingle ref) m handler
instance (MonadError ServerError m, HandlesRef ref v, handler ~ (ConduitT v Void m () -> m ()))
         => Handles '[] ('RetStream ref) m handler
