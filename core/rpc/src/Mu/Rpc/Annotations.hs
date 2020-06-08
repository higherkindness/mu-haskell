{-# language DataKinds            #-}
{-# language GADTs                #-}
{-# language PolyKinds            #-}
{-# language TypeFamilies         #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}
{-|
Description : Protocol-defined annotations.

Libraries can define custom annotations to
indicate additional information not found
in the 'Package' itself. For example, GraphQL
has optional default values for arguments.
-}
module Mu.Rpc.Annotations (
  RpcAnnotation(..)
, AnnotatedPackage
, GetPackageAnnotation
, GetServiceAnnotation
, GetMethodAnnotation
, GetArgAnnotation
, GetArgAnnotationMay
) where

import           GHC.TypeLits

import           Mu.Rpc

-- | Annotations proper.
data RpcAnnotation domain serviceName methodName argName where
  -- | Annotation over the whole package.
  AnnPackage :: domain
             -> RpcAnnotation domain serviceName methodName argName
  -- | Annotation over a service.
  AnnService :: serviceName -> domain
             -> RpcAnnotation domain serviceName methodName argName
  -- | Annotation over a method.
  AnnMethod  :: serviceName -> methodName -> domain
             -> RpcAnnotation domain serviceName methodName argName
  -- | Annotation over an argument.
  AnnArg     :: serviceName -> methodName -> argName -> domain
             -> RpcAnnotation domain serviceName methodName argName

-- | This type family links each schema to
--   its corresponding annotations from one domain.
type family AnnotatedPackage domain (sch :: Package serviceName methodName argName tyRef)
  :: [RpcAnnotation domain serviceName methodName argName]

-- | Find the annotation over the package in the given set.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetPackageAnnotation (anns :: [RpcAnnotation domain s m a]) :: domain where
  GetPackageAnnotation '[]
    = TypeError ('Text "cannot find schema annotation")
  GetPackageAnnotation ('AnnPackage d ': rs) = d
  GetPackageAnnotation (r             ': rs) = GetPackageAnnotation rs

-- | Find the annotation over the given service in the given set.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetServiceAnnotation (anns :: [RpcAnnotation domain s m a]) (snm :: s) :: domain where
  GetServiceAnnotation '[] snm
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType snm)
  GetServiceAnnotation ('AnnService snm d ': rs) snm = d
  GetServiceAnnotation (r ': rs) snm = GetServiceAnnotation rs snm

-- | Find the annotation over the given method in the given service.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetMethodAnnotation (anns :: [RpcAnnotation domain s m a]) (snm :: s) (mnm :: m) :: domain where
  GetMethodAnnotation '[] snm mnm
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType snm ':<>: 'Text "/" ':<>: 'ShowType mnm)
  GetMethodAnnotation ('AnnMethod snm mnm d ': rs) snm mnm = d
  GetMethodAnnotation (r ': rs) snm mnm = GetMethodAnnotation rs snm mnm

-- | Find the annotation over the given argument in te given method in the given service.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetArgAnnotation (anns :: [RpcAnnotation domain s m a]) (snm :: s) (mnm :: m) (anm :: a) :: domain where
  GetArgAnnotation '[] snm mnm anm
    = TypeError ('Text "cannot find annotation for " ':<>: 'ShowType snm ':<>: 'Text "/" ':<>: 'ShowType mnm ':<>: 'Text "/" ':<>: 'ShowType anm)
  GetArgAnnotation ('AnnArg snm mnm anm d ': rs) snm mnm anm = d
  GetArgAnnotation (r ': rs) snm mnm anm = GetArgAnnotation rs snm mnm anm

-- | Find the annotation over the given argument in te given method in the given service.
--   If the annotation cannot be found, raise a 'TypeError'.
type family GetArgAnnotationMay (anns :: [RpcAnnotation domain s m a]) (snm :: s) (mnm :: m) (anm :: a) :: Maybe domain where
  GetArgAnnotationMay '[] snm mnm anm = 'Nothing
  GetArgAnnotationMay ('AnnArg snm mnm anm d ': rs) snm mnm anm = 'Just d
  GetArgAnnotationMay (r ': rs) snm mnm anm = GetArgAnnotationMay rs snm mnm anm
