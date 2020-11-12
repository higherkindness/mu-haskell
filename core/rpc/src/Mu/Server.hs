{-# language CPP                       #-}
{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts          #-}
{-# language FlexibleInstances         #-}
{-# language FunctionalDependencies    #-}
{-# language GADTs                     #-}
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

In general, you can declare a server by naming
each of the methods with their handlers:

> server :: MonadServer m => ServerT MyService m _
> server = singleService ( method @"m1" h1
>                        , method @"m2" h2
>                        , ... )

or by position:

> server :: MonadServer m => ServerT MyService m _
> server = Server (h1 :<|>: h2 :<|>: ... :<|>: H0)

where each of @h1@, @h2@, ... handles each method in
@MyService@ /in the order they were declared/.

In both cases, the @_@ in the type allows GHC to fill
in the boring and long type you would need to write
there otherwise.

/Implementation note/: exceptions raised in handlers
produce an error to be sent as response to the client.
We recommend you to catch exceptions and return custom
'ServerError's instead.
-}
module Mu.Server (
  -- * Servers and handlers
  MonadServer, ServiceChain, noContext
, wrapServer
  -- ** Definitions by name
, singleService
, method, methodWithInfo
, resolver, object
, field, fieldWithInfo
, NamedList(..)
  -- ** Definitions by position
, SingleServerT, pattern Server
, ServerT(..), ServicesT(..), HandlersT(.., (:<||>:), (:<|>:))
  -- ** Simple servers using only IO
, ServerErrorIO, ServerIO
  -- * Errors which might be raised
, serverError, ServerError(..), ServerErrorCode(..)
  -- ** Useful when you do not want to deal with errors
, alwaysOk
  -- * For internal use
, Handles, FromRef, ToRef
) where

import           Control.Exception    (Exception)
import           Control.Monad.Except
import           Data.Conduit
import           Data.Kind
import           GHC.TypeLits

import           Mu.Rpc
import           Mu.Schema

#if __GLASGOW_HASKELL__ < 880
import Unsafe.Coerce (unsafeCoerce)
#endif

-- | Constraint for monads that can be used as servers
type MonadServer m = (MonadError ServerError m, MonadIO m)
-- | Simplest monad which satisfies 'MonadServer'.
type ServerErrorIO = ExceptT ServerError IO

-- | Simple 'ServerT' which uses only 'IO' and errors,
--   and whose service has no back-references.
type ServerIO info srv = ServerT '[] info srv ServerErrorIO

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
noContext :: b -> a1 -> a2 -> b
noContext x _ _ = x

-- | Errors raised in a handler.
data ServerError
  = ServerError ServerErrorCode String
  deriving Show

instance Exception ServerError

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
data ServerT (chn :: ServiceChain snm) (info :: Type)
             (s :: Package snm mnm anm (TypeRef snm))
             (m :: Type -> Type) (hs :: [[Type]]) where
  Services :: ServicesT chn info s m hs
           -> ServerT chn info ('Package pname s) m hs

pattern Server :: (MappingRight chn sname ~ ())
               => HandlersT chn info () methods m hs
               -> ServerT chn info ('Package pname '[ 'Service sname methods ]) m '[hs]
pattern Server svr = Services (svr :<&>: S0)

infixr 3 :<&>:
-- | Definition of a complete server for a service.
data ServicesT (chn :: ServiceChain snm) (info :: Type)
               (s :: [Service snm mnm anm (TypeRef snm)])
               (m :: Type -> Type) (hs :: [[Type]]) where
  S0 :: ServicesT chn info '[] m '[]
  (:<&>:) :: HandlersT chn info (MappingRight chn sname) methods m hs
          -> ServicesT chn info rest m hss
          -> ServicesT chn info ('Service sname methods ': rest) m (hs ': hss)

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
data HandlersT (chn :: ServiceChain snm) (info :: Type)
               (inh :: *) (methods :: [Method snm mnm anm (TypeRef snm)])
               (m :: Type -> Type) (hs :: [Type]) where
  H0 :: HandlersT chn info inh '[] m '[]
  Hmore :: Handles chn args ret m h
        => Proxy args -> Proxy ret
        -> (RpcInfo info -> inh -> h)
        -> HandlersT chn info inh ms m hs
        -> HandlersT chn info inh ('Method name args ret ': ms) m (h ': hs)

infixr 4 :<||>:
pattern (:<||>:) :: Handles chn args ret m h
                 => (RpcInfo info -> inh -> h) -> HandlersT chn info inh ms m hs
                 -> HandlersT chn info inh ('Method name args ret ': ms) m (h ': hs)
pattern x :<||>: xs <- Hmore _ _ x xs where
  x :<||>: xs = Hmore Proxy Proxy x xs

infixr 4 :<|>:
pattern (:<|>:) :: (Handles chn args ret m h)
                => h -> HandlersT chn info () ms m hs
                -> HandlersT chn info () ('Method name args ret ': ms) m (h ': hs)
pattern x :<|>: xs <- (($ ()) . ($ NoRpcInfo) -> x) :<||>: xs where
  x :<|>: xs = noContext x :<||>: xs

-- | Defines a relation for handling.
class Handles (chn :: ServiceChain snm)
              (args :: [Argument snm anm (TypeRef snm)])
              (ret :: Return snm (TypeRef snm))
              (m :: Type -> Type) (h :: Type) where
  wrapHandler :: Proxy '(chn, m) -> Proxy args -> Proxy ret
              -> (forall a. m a -> m a) -> h -> h
-- | Defines whether a given type @t@
--   can be turned into the 'TypeRef' @ref@.
class ToRef   (chn :: ServiceChain snm)
              (ref :: TypeRef snm) (t :: Type)
-- | Defines whether a given type @t@
--   can be obtained from the 'TypeRef' @ref@.
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
instance forall chn ref args ret m handler h t aname.
         ( FromRef chn ref t, Handles chn args ret m h
         , handler ~ (t -> h) )
         => Handles chn ('ArgSingle aname ref ': args) ret m handler where
  wrapHandler pchn _ pr f h = wrapHandler pchn (Proxy @args) pr f . h
instance (MonadError ServerError m, FromRef chn ref t, Handles chn args ret m h,
          handler ~ (ConduitT () t m () -> h))
         => Handles chn ('ArgStream aname ref ': args) ret m handler where
  wrapHandler pchn _ pr f h = wrapHandler pchn (Proxy @args) pr f . h
-- Result with exception
instance (MonadError ServerError m, handler ~ m ())
         => Handles chn '[] 'RetNothing m handler where
  wrapHandler _ _ _ f h = f h
instance ( MonadError ServerError m, ToRef chn eref e, ToRef chn vref v
         , handler ~ m (Either e v) )
         => Handles chn '[] ('RetThrows eref vref) m handler where
  wrapHandler _ _ _ f h = f h
instance (MonadError ServerError m, ToRef chn ref v, handler ~ m v)
         => Handles chn '[] ('RetSingle ref) m handler where
  wrapHandler _ _ _ f h = f h
instance ( MonadError ServerError m, ToRef chn ref v
         , handler ~ (ConduitT v Void m () -> m ()) )
         => Handles chn '[] ('RetStream ref) m handler where
  wrapHandler _ _ _ f h = f . h

-- SIMPLER WAY TO DECLARE SERVICES

-- | Declares the handler for a method in the service.
--   Intended to be used with @TypeApplications@:
--
--   > method @"myMethod" myHandler
method :: forall n a p. p -> Named n (a -> () -> p)
method f = Named (\_ _ -> f)

-- | Declares the handler for a method in the service,
--   which is passed additional information about the call.
--   Intended to be used with @TypeApplications@:
--
--   > methodWithInfo @"myMethod" myHandler
methodWithInfo :: forall n p info. (RpcInfo info -> p) -> Named n (RpcInfo info -> () -> p)
methodWithInfo f = Named (\x () -> f x)

-- | Declares the handler for a field in an object.
--   Intended to be used with @TypeApplications@:
--
--   > field @"myField" myHandler
field :: forall n h info. h -> Named n (RpcInfo info -> h)
field f = Named (const f)

-- | Declares the handler for a field in an object,
--   which is passed additional information about the call.
--   Intended to be used with @TypeApplications@:
--
--   > fieldWithInfo @"myField" myHandler
fieldWithInfo :: forall n h info. (RpcInfo info -> h) -> Named n (RpcInfo info -> h)
fieldWithInfo  = Named

-- | Defines a server for a package with a single service.
--   Intended to be used with a tuple of 'method's:
--
--   > singleService (method @"m1" h1, method @"m2" h2)
singleService
  :: ( ToNamedList p nl
     , ToHandlers chn info () methods m hs nl
     , MappingRight chn sname ~ () )
  => p -> ServerT chn info ('Package pname '[ 'Service sname methods ]) m '[hs]
singleService nl = Server $ toHandlers $ toNamedList nl

-- | Defines the implementation of a single GraphQL object,
--   which translates as a single Mu service.
--   Intended to be used with @TypeApplications@
--   and a tuple of 'field's:
--
--   > object @"myObject" (field @"f1" h1, fielf @"f2" h2)
--
--   Note: for the root objects in GraphQL (query, mutation, subscription)
--   use 'method' instead of 'object'.
object
  :: forall sname p nl chn info ms m hs.
     ( ToNamedList p nl
     , ToHandlers chn info (MappingRight chn sname) ms m hs nl )
  => p -> Named sname (HandlersT chn info (MappingRight chn sname) ms m hs)
object nl = Named $ toHandlers $ toNamedList nl

-- | Combines the implementation of several GraphQL objects,
--   which means a whole Mu service for a GraphQL server.
--   Intented to be used with a tuple of 'objects':
--
--   > resolver (object @"o1" ..., object @"o2" ...)
resolver
  :: (ToNamedList p nl, ToServices chn info ss m hs nl)
  => p -> ServerT chn info ('Package pname ss) m hs
resolver nl = Services $ toServices $ toNamedList nl

-- | A value tagged with a type-level name.
data Named n h where
  Named :: forall n h. h -> Named n h

infixr 4 :|:
-- | Heterogeneous list in which each element
--   is tagged with a type-level name.
data NamedList (hs :: [(Symbol, *)]) where
  N0    :: NamedList '[]
  (:|:) :: Named n h -> NamedList hs
        -> NamedList ('(n, h) ': hs)

-- | Used to turn tuples into 'NamedList's.
class ToNamedList p nl | p -> nl where
  toNamedList :: p -> NamedList nl

instance ToNamedList (NamedList nl) nl where
  toNamedList = id
instance ToNamedList () '[] where
  toNamedList _ = N0
instance ToNamedList (Named n h) '[ '(n, h) ] where
  toNamedList n = n :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2)
                     '[ '(n1, h1), '(n2, h2) ] where
  toNamedList (n1, n2) = n1 :|: n2 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3) ] where
  toNamedList (n1, n2, n3) = n1 :|: n2 :|: n3 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4) ] where
  toNamedList (n1, n2, n3, n4) = n1 :|: n2 :|: n3 :|: n4 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5) ] where
  toNamedList (n1, n2, n3, n4, n5) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6) ] where
  toNamedList (n1, n2, n3, n4, n5, n6) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7, Named n8 h8)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7), '(n8, h8) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7, n8) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: n8 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7, Named n8 h8, Named n9 h9)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7), '(n8, h8), '(n9, h9) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7, n8, n9) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: n8 :|: n9 :|: N0

class ToHandlers chn info inh ms m hs nl | chn inh ms m nl -> hs where
  toHandlers :: NamedList nl
             -> HandlersT chn info inh ms m hs

instance ToHandlers chn info inh '[] m '[] nl where
  toHandlers _ = H0
instance ( FindHandler name info inh h nl
         , Handles chn args ret m h
         , ToHandlers chn info inh ms m hs nl )
         => ToHandlers chn info inh ('Method name args ret ': ms) m (h ': hs) nl where
  toHandlers nl = findHandler (Proxy @name) nl :<||>: toHandlers nl

class FindHandler name info inh h nl | name nl -> inh h where
  findHandler :: Proxy name -> NamedList nl -> RpcInfo info -> inh -> h
instance (inh ~ h, h ~ TypeError ('Text "cannot find handler for " ':<>: 'ShowType name))
         => FindHandler name info inh h '[] where
  findHandler = error "this should never be called"
instance {-# OVERLAPS #-} (RpcInfo info ~ rpc', inh ~ inh', h ~ h')
         => FindHandler name info inh h ( '(name, rpc' -> inh' -> h') ': rest ) where
  findHandler _ (Named f :|: _) = f
instance {-# OVERLAPPABLE #-} FindHandler name info inh h rest
         => FindHandler name info inh h (thing ': rest) where
  findHandler p (_ :|: rest) = findHandler p rest

class ToServices chn info ss m hs nl | chn ss m nl -> hs where
  toServices :: NamedList nl
             -> ServicesT chn info ss m hs

instance ToServices chn info '[] m '[] nl where
  toServices _ = S0
instance ( FindService name (HandlersT chn info (MappingRight chn name) methods m h) nl
         , ToServices chn info ss m hs nl)
         => ToServices chn info ('Service name methods ': ss) m (h ': hs) nl where
  toServices nl = findService (Proxy @name) nl :<&>: toServices nl

class FindService name h nl | name nl -> h where
  findService :: Proxy name -> NamedList nl -> h
instance (h ~ TypeError ('Text "cannot find handler for " ':<>: 'ShowType name))
         => FindService name h '[] where
  findService = error "this should never be called"
instance {-# OVERLAPS #-} (h ~ h')
         => FindService name h ( '(name, h') ': rest ) where
  findService _ (Named f :|: _) = f
instance {-# OVERLAPPABLE #-} FindService name h rest
         => FindService name h (thing ': rest) where
  findService p (_ :|: rest) = findService p rest

-- WRAPPING MECHANISM

wrapServer
  :: forall chn info p m topHs.
     (forall a. RpcInfo info -> m a -> m a)
  -> ServerT chn info p m topHs -> ServerT chn info p m topHs
wrapServer f (Services ss) = Services (wrapServices ss)
  where
    wrapServices :: forall ss hs.
                    ServicesT chn info ss m hs
                 -> ServicesT chn info ss m hs
    wrapServices S0 = S0
    wrapServices (h :<&>: rest)
#if __GLASGOW_HASKELL__ >= 880
      = wrapHandlers h :<&>: wrapServices rest
#else
      = unsafeCoerce (wrapHandlers (unsafeCoerce h)) :<&>: unsafeCoerce (wrapServices rest)
#endif

    wrapHandlers :: forall inh ms innerHs.
                    HandlersT chn info inh ms m innerHs
                 -> HandlersT chn info inh ms m innerHs
    wrapHandlers H0 = H0
    wrapHandlers (Hmore pargs pret h rest)
      = Hmore pargs pret
              (\rpc inh -> wrapHandler (Proxy @'(chn, m)) pargs pret (f rpc) (h rpc inh))
              (wrapHandlers rest)
