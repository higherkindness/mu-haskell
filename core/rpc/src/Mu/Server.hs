{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts          #-}
{-# language FlexibleInstances         #-}
{-# language GADTs                     #-}
{-# language MultiParamTypeClasses     #-}
{-# language PatternSynonyms           #-}
{-# language PolyKinds                 #-}
{-# language RankNTypes                #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
{-# language ViewPatterns              #-}
{-|
Description : Protocol-independent declaration of servers.

A server (represented by 'ServerT') is a sequence
of handlers (represented by 'HandlersT'), one for each
operation in the corresponding Mu service declaration.

In general, you should declare a server as:

> server :: MonadServer m => ServerT w MyService m _
> server = Server (h1 :<|>: h2 :<|>: ... :<|>: H0)

where each of @h1@, @h2@, ... handles each method in
@MyService@ /in the order they were declared/.
The @_@ in the type allows GHC to fill in the boring
and long type you would need to write there otherwise.

/Implementation note/: exceptions raised in handlers
produce an error to be sent as response to the client.
We recommend you to catch exceptions and return custom
'ServerError's instead.
-}
module Mu.Server (
  -- * Servers and handlers
  MonadServer
, SingleServerT
, ServerT(.., Server), ServicesT(..), HandlersT(.., (:<|>:))
, ServiceChain, noContext
  -- ** Simple servers using only IO
, ServerErrorIO, ServerIO
  -- * Errors which might be raised
, serverError, ServerError(..), ServerErrorCode(..)
  -- ** Useful when you do not want to deal with errors
, alwaysOk
  -- * For internal use
, Handles, FromRef, ToRef
) where

import           Control.Monad.Except
import           Data.Conduit
import           Data.Kind

import           Mu.Rpc
import           Mu.Schema

-- | Constraint for monads that can be used as servers
type MonadServer m = (MonadError ServerError m, MonadIO m)
-- | Simplest monad which satisfies 'MonadServer'.
type ServerErrorIO = ExceptT ServerError IO

-- | Simple 'ServerT' which uses only 'IO' and errors,
--   and whose service has no back-references.
type ServerIO w srv = ServerT w '[] srv ServerErrorIO

-- | Stop the current handler,
--   returning an error to the client.
serverError :: (MonadError ServerError m)
            => ServerError -> m a
serverError = throwError

-- | Wrapper for handlers which do not use errors.
--   Remember that any exception raised in 'IO'
--   is propagated to the client.
alwaysOk :: (MonadIO m)
         => IO a -> m a
alwaysOk = liftIO

-- | To declare that the function doesn't use
--   its context.
noContext :: b -> a -> b
noContext = const

-- | Errors raised in a handler.
data ServerError
  = ServerError ServerErrorCode String

-- | Possible types of errors.
--   Some of these are handled in a special way
--   by different transpoprt layers.
data ServerErrorCode
  = Unknown
  | Unavailable
  | Unimplemented
  | Unauthenticated
  | Internal
  | Invalid
  | NotFound
  deriving (Eq, Show)

-- | Defines a mapping between outcome of
--   a service, and its representation as
--   Haskell type.
type ServiceChain snm = Mappings snm Type

-- | A server for a single service,
--   like most RPC ones.
type SingleServerT w = ServerT w '[]

-- | Definition of a complete server
--   for a set of services, with possible
--   references between them.
data ServerT (w :: Type -> Type)  -- wrapper for data types
             (chn :: ServiceChain snm) (s :: Package snm mnm anm)
             (m :: Type -> Type) (hs :: [[Type]]) where
  Services :: ServicesT w chn s m hs
           -> ServerT w chn ('Package pname s) m hs

pattern Server :: (MappingRight chn sname ~ ())
               => HandlersT w chn () methods m hs
               -> ServerT w chn ('Package pname '[ 'Service sname sanns methods ]) m '[hs]
pattern Server svr = Services (svr :<&>: S0)

infixr 3 :<&>:
-- | Definition of a complete server for a service.
data ServicesT (w :: Type -> Type)
               (chn :: ServiceChain snm) (s :: [Service snm mnm anm])
               (m :: Type -> Type) (hs :: [[Type]]) where
  S0 :: ServicesT w chn '[] m '[]
  (:<&>:) :: HandlersT w chn (MappingRight chn sname) methods m hs
          -> ServicesT w chn rest m hss
          -> ServicesT w chn ('Service sname anns methods ': rest) m (hs ': hss)

infixr 4 :<||>:
-- | 'HandlersT' is a sequence of handlers.
--   Note that the handlers for your service
--   must appear __in the same order__ as they
--   are defined.
--
--   In general you can choose any type you want
--   for your handlers, due to the following restrictions:
--
--   * Haskell types must be convertible to the
--     corresponding schema type. In other words,
--     they must implement 'FromSchema' if they are
--     inputs, and 'ToSchema' if they are outputs.
--   * Normal returns are represented by returning
--     the corresponding Haskell type.
--   * Input streams turn into @Conduit () t m ()@,
--     where @t@ is the Haskell type for that schema type.
--   * Output streams turn into an __additional argument__
--     of type @Conduit t Void m ()@. This stream should
--     be connected to a source to get the elements.
data HandlersT (w :: Type -> Type) (chn :: ServiceChain snm)
               (inh :: *) (methods :: [Method snm mnm anm])
               (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT w chn inh '[] m '[]
  (:<||>:) :: Handles w chn args ret m h
           => (inh -> h) -> HandlersT w chn inh ms m hs
           -> HandlersT w chn inh ('Method name anns args ret ': ms) m (h ': hs)

infixr 4 :<|>:
pattern (:<|>:) :: (Handles w chn args ret m h)
                => h -> HandlersT w chn () ms m hs
                -> HandlersT w chn () ('Method name anns args ret ': ms) m (h ': hs)
pattern x :<|>: xs <- (($ ()) -> x) :<||>: xs where
  x :<|>: xs = noContext x :<||>: xs

-- Define a relation for handling
class Handles (w :: Type -> Type)
              (chn :: ServiceChain snm)
              (args :: [Argument snm anm]) (ret :: Return snm)
              (m :: Type -> Type) (h :: Type)
class ToRef   (w :: Type -> Type) (chn :: ServiceChain snm)
              (ref :: TypeRef snm) (t :: Type)
class FromRef (w :: Type -> Type) (chn :: ServiceChain snm)
              (ref :: TypeRef snm) (t :: Type)

-- Type references
instance t ~ s => ToRef w chn ('PrimitiveRef t) s
instance ToSchema w sch sty t => ToRef w chn ('SchemaRef sch sty) t
instance MappingRight chn ref ~ t => ToRef w chn ('ObjectRef ref) t
instance t ~ s => ToRef w chn ('RegistryRef subject t last) s
instance (ToRef w chn ref t, [t] ~ s) => ToRef w chn ('ListRef ref) s
instance (ToRef w chn ref t, Maybe t ~ s) => ToRef w chn ('OptionalRef ref) s

instance t ~ s => FromRef w chn ('PrimitiveRef t) s
instance FromSchema w sch sty t => FromRef w chn ('SchemaRef sch sty) t
instance MappingRight chn ref ~ t => FromRef w chn ('ObjectRef ref) t
instance t ~ s => FromRef w chn ('RegistryRef subject t last) s
instance (FromRef w chn ref t, [t] ~ s) => FromRef w chn ('ListRef ref) s
instance (FromRef w chn ref t, Maybe t ~ s) => FromRef w chn ('OptionalRef ref) s

-- Arguments
instance (FromRef w chn ref t, Handles w chn args ret m h,
          handler ~ (t -> h))
         => Handles w chn ('ArgSingle aname ref ': args) ret m handler
instance (MonadError ServerError m, FromRef w chn ref t, Handles w chn args ret m h,
          handler ~ (ConduitT () t m () -> h))
         => Handles w chn ('ArgStream aname ref ': args) ret m handler
-- Result with exception
instance (MonadError ServerError m, handler ~ m ())
         => Handles w chn '[] 'RetNothing m handler
instance (MonadError ServerError m, ToRef w chn eref e, ToRef w chn vref v, handler ~ m (Either e v))
         => Handles w chn '[] ('RetThrows eref vref) m handler
instance (MonadError ServerError m, ToRef w chn ref v, handler ~ m v)
         => Handles w chn '[] ('RetSingle ref) m handler
instance (MonadError ServerError m, ToRef w chn ref v, handler ~ (ConduitT v Void m () -> m ()))
         => Handles w chn '[] ('RetStream ref) m handler
