{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
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
  ServerIO, ServerT(..)
, HandlersIO, HandlersT(..)
  -- * Errors which might be raised
, serverError, ServerErrorIO, ServerError(..), ServerErrorCode(..)
  -- ** Useful when you do not want to deal with errors
, alwaysOk
) where

import           Control.Monad.Except
import           Data.Conduit
import           Data.Kind

import           Mu.Rpc
import           Mu.Schema

serverError :: ServerError -> ServerErrorIO a
serverError = throwError

alwaysOk :: IO a -> ServerErrorIO a
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

type ServerErrorIO = ExceptT ServerError IO

data ServerT (s :: Service snm mnm) (m :: Type -> Type) (hs :: [Type]) where
  Server :: HandlersT methods m hs -> ServerT ('Service sname anns methods) m hs
type ServerIO service = ServerT service ServerErrorIO

infixr 5 :<|>:
data HandlersT (methods :: [Method mnm]) (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT '[] m '[]
  (:<|>:) :: Handles args ret m h => h -> HandlersT ms m hs
          -> HandlersT ('Method name anns args ret ': ms) m (h ': hs)
type HandlersIO methods = HandlersT methods ServerErrorIO

-- Define a relation for handling
class Handles (args :: [Argument]) (ret :: Return)
              (m :: Type -> Type) (h :: Type)
class HandlesRef (ref :: TypeRef) (t :: Type)

-- Type references
instance HasSchema sch sty t => HandlesRef ('FromSchema sch sty) t
instance HandlesRef ('FromRegistry subject t last) t

-- Arguments
instance (HandlesRef ref t, Handles args ret m h, handler ~ (t -> h))
         => Handles ('ArgSingle ref ': args) ret m handler
instance (HandlesRef ref t, Handles args ret m h, handler ~ (ConduitT () t m () -> h))
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
