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
type ServerIO w srv = ServerT w srv ServerErrorIO

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

data ServerT (w :: Type -> Type) (s :: Service snm mnm) (m :: Type -> Type) (hs :: [Type]) where
  Server :: HandlersT w methods m hs -> ServerT w ('Service sname anns methods) m hs

infixr 5 :<|>:
data HandlersT (w :: Type -> Type) (methods :: [Method mnm]) (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT w '[] m '[]
  (:<|>:) :: Handles w args ret m h => h -> HandlersT w ms m hs
          -> HandlersT w ('Method name anns args ret ': ms) m (h ': hs)

-- Define a relation for handling
class Handles (w :: Type -> Type) (args :: [Argument]) (ret :: Return)
              (m :: Type -> Type) (h :: Type)
class ToRef (w :: Type -> Type) (ref :: TypeRef) (t :: Type)
class FromRef (w :: Type -> Type) (ref :: TypeRef) (t :: Type)

-- Type references
instance ToSchema w sch sty t => ToRef w ('ViaSchema sch sty) t
instance ToRef w ('ViaRegistry subject t last) t
instance FromSchema w sch sty t => FromRef w ('ViaSchema sch sty) t
instance FromRef w ('ViaRegistry subject t last) t

-- Arguments
instance (FromRef w ref t, Handles w args ret m h,
          handler ~ (t -> h))
         => Handles w ('ArgSingle ref ': args) ret m handler
instance (MonadError ServerError m, FromRef w ref t, Handles w args ret m h,
          handler ~ (ConduitT () t m () -> h))
         => Handles w ('ArgStream ref ': args) ret m handler
-- Result with exception
instance (MonadError ServerError m, handler ~ m ())
         => Handles w '[] 'RetNothing m handler
instance (MonadError ServerError m, ToRef w eref e, ToRef w vref v, handler ~ m (Either e v))
         => Handles w '[] ('RetThrows eref vref) m handler
instance (MonadError ServerError m, ToRef w ref v, handler ~ m v)
         => Handles w '[] ('RetSingle ref) m handler
instance (MonadError ServerError m, ToRef w ref v, handler ~ (ConduitT v Void m () -> m ()))
         => Handles w '[] ('RetStream ref) m handler
