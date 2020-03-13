{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts          #-}
{-# language FlexibleInstances         #-}
{-# language FunctionalDependencies    #-}
{-# language GADTs                     #-}
{-# language MultiParamTypeClasses     #-}
{-# language PatternSynonyms           #-}
{-# language PolyKinds                 #-}
{-# language RankNTypes                #-}
{-# language ScopedTypeVariables       #-}
{-# language TypeApplications          #-}
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

> server :: MonadServer m => ServerT MyService m _
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
  MonadServer, ServiceChain, noContext
  -- ** Definitions by name
, singleService, method, field, NamedList(..)
  -- ** Definitions by position
, SingleServerT, pattern Server
, ServerT(..), ServicesT(..), HandlersT(.., (:<|>:))
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
import           GHC.TypeLits

import           Mu.Rpc
import           Mu.Schema

-- | Constraint for monads that can be used as servers
type MonadServer m = (MonadError ServerError m, MonadIO m)
-- | Simplest monad which satisfies 'MonadServer'.
type ServerErrorIO = ExceptT ServerError IO

-- | Simple 'ServerT' which uses only 'IO' and errors,
--   and whose service has no back-references.
type ServerIO srv = ServerT '[] srv ServerErrorIO

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
type SingleServerT = ServerT '[]

-- | Definition of a complete server
--   for a set of services, with possible
--   references between them.
data ServerT (chn :: ServiceChain snm) (s :: Package snm mnm anm)
             (m :: Type -> Type) (hs :: [[Type]]) where
  Services :: ServicesT chn s m hs
           -> ServerT chn ('Package pname s) m hs

pattern Server :: (MappingRight chn sname ~ ())
               => HandlersT chn () methods m hs
               -> ServerT chn ('Package pname '[ 'Service sname sanns methods ]) m '[hs]
pattern Server svr = Services (svr :<&>: S0)

infixr 3 :<&>:
-- | Definition of a complete server for a service.
data ServicesT (chn :: ServiceChain snm) (s :: [Service snm mnm anm])
               (m :: Type -> Type) (hs :: [[Type]]) where
  S0 :: ServicesT chn '[] m '[]
  (:<&>:) :: HandlersT chn (MappingRight chn sname) methods m hs
          -> ServicesT chn rest m hss
          -> ServicesT chn ('Service sname anns methods ': rest) m (hs ': hss)

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
data HandlersT (chn :: ServiceChain snm)
               (inh :: *) (methods :: [Method snm mnm anm])
               (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT chn inh '[] m '[]
  (:<||>:) :: Handles chn args ret m h
           => (inh -> h) -> HandlersT chn inh ms m hs
           -> HandlersT chn inh ('Method name anns args ret ': ms) m (h ': hs)

infixr 4 :<|>:
pattern (:<|>:) :: (Handles chn args ret m h)
                => h -> HandlersT chn () ms m hs
                -> HandlersT chn () ('Method name anns args ret ': ms) m (h ': hs)
pattern x :<|>: xs <- (($ ()) -> x) :<||>: xs where
  x :<|>: xs = noContext x :<||>: xs

-- Define a relation for handling
class Handles (chn :: ServiceChain snm)
              (args :: [Argument snm anm]) (ret :: Return snm)
              (m :: Type -> Type) (h :: Type)
class ToRef   (chn :: ServiceChain snm)
              (ref :: TypeRef snm) (t :: Type)
class FromRef (chn :: ServiceChain snm)
              (ref :: TypeRef snm) (t :: Type)

-- Type references
instance t ~ s => ToRef chn ('PrimitiveRef t) s
instance ToSchema sch sty t => ToRef chn ('SchemaRef sch sty) t
instance MappingRight chn ref ~ t => ToRef chn ('ObjectRef ref) t
instance t ~ s => ToRef chn ('RegistryRef subject t last) s
instance (ToRef chn ref t, [t] ~ s) => ToRef chn ('ListRef ref) s
instance (ToRef chn ref t, Maybe t ~ s) => ToRef chn ('OptionalRef ref) s

instance t ~ s => FromRef chn ('PrimitiveRef t) s
instance FromSchema sch sty t => FromRef chn ('SchemaRef sch sty) t
instance MappingRight chn ref ~ t => FromRef chn ('ObjectRef ref) t
instance t ~ s => FromRef chn ('RegistryRef subject t last) s
instance (FromRef chn ref t, [t] ~ s) => FromRef chn ('ListRef ref) s
instance (FromRef chn ref t, Maybe t ~ s) => FromRef chn ('OptionalRef ref) s

-- Arguments
instance (FromRef chn ref t, Handles chn args ret m h,
          handler ~ (t -> h))
         => Handles chn ('ArgSingle aname anns ref ': args) ret m handler
instance (MonadError ServerError m, FromRef chn ref t, Handles chn args ret m h,
          handler ~ (ConduitT () t m () -> h))
         => Handles chn ('ArgStream aname anns ref ': args) ret m handler
-- Result with exception
instance (MonadError ServerError m, handler ~ m ())
         => Handles chn '[] 'RetNothing m handler
instance (MonadError ServerError m, ToRef chn eref e, ToRef chn vref v, handler ~ m (Either e v))
         => Handles chn '[] ('RetThrows eref vref) m handler
instance (MonadError ServerError m, ToRef chn ref v, handler ~ m v)
         => Handles chn '[] ('RetSingle ref) m handler
instance (MonadError ServerError m, ToRef chn ref v, handler ~ (ConduitT v Void m () -> m ()))
         => Handles chn '[] ('RetStream ref) m handler

-- SIMPLER WAY TO DECLARE SERVICES

method :: forall n p. p -> Named n (() -> p)
method f = Named (\() -> f)
field :: forall n h. h -> Named n h
field  = Named

singleService
  :: (ToHandlers chn () methods m hs nl, MappingRight chn sname ~ ())
  => NamedList nl -> ServerT chn ('Package pname '[ 'Service sname sanns methods]) m '[hs]
singleService nl = Server (toHandlers nl)

data Named n h where
  Named :: forall n h. h -> Named n h

infixr 4 :|:
data NamedList hs where
  N0    :: NamedList '[]
  (:|:) :: Named n h -> NamedList hs
        -> NamedList ('(n, h) ': hs)

class ToHandlers chn inh ms m hs nl | chn inh ms m nl -> hs where
  toHandlers :: NamedList nl
             -> HandlersT chn inh ms m hs

instance ToHandlers chn inh '[] m '[] nl where
  toHandlers _ = H0
instance (FindHandler name inh h nl, Handles chn args ret m h, ToHandlers chn inh ms m hs nl)
         => ToHandlers chn inh ('Method name anns args ret ': ms) m (h ': hs) nl where
  toHandlers nl = findHandler (Proxy @name) nl :<||>: toHandlers nl

class FindHandler name inh h nl | name nl -> inh h where
  findHandler :: Proxy name -> NamedList nl -> inh -> h
{-
instance TypeError ('Text "cannot find handler for " ':<>: 'ShowType name)
         => FindHandler name inh h '[] where
  findHandler = error "this should never be called"
-}
instance {-# OVERLAPS #-} (inh ~ inh', h ~ h')
         => FindHandler name inh h ( '(name, inh' -> h') ': rest ) where
  findHandler _ (Named f :|: _) = f
instance {-# OVERLAPPABLE #-} FindHandler name inh h rest
         => FindHandler name inh h (thing ': rest) where
  findHandler p (_ :|: rest) = findHandler p rest
