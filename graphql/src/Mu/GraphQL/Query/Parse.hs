{-# language DataKinds            #-}
{-# language FlexibleInstances    #-}
{-# language GADTs                #-}
{-# language PolyKinds            #-}
{-# language ScopedTypeVariables  #-}
{-# language TypeApplications     #-}
{-# language TypeOperators        #-}
{-# language UndecidableInstances #-}

module Mu.GraphQL.Query.Parse where

import           Data.Proxy
import           Data.SOP.NS
import qualified Data.Text                     as T
import           GHC.TypeLits
import qualified Language.GraphQL.Draft.Syntax as GQL
import           Mu.GraphQL.Query.Definition
import           Mu.Rpc
import           Mu.Schema

-- data Package serviceName methodName
--   = Package (Maybe serviceName)
--             [Service serviceName methodName]

-- data Service serviceName methodName
--   = Service serviceName
--             [ServiceAnnotation]
--             [Method serviceName methodName]

-- type family LookupService (ss :: [Service snm mnm]) (s :: snm) :: Service snm mnm where
--   LookupService '[] s = TypeError ('Text "could not find method " ':<>: 'ShowType s)
--   LookupService ('Service s anns ms ': ss) s = 'Service s anns ms
--   LookupService (other              ': ss) s = LookupService ss s

-- type ServiceQuery (p :: Package snm mnm) (s :: Service snm mnm)
--   = [OneMethodQuery p s]

-- type SelectionSet = [Selection]

-- data Selection
--   = SelectionField !Field
--   | SelectionFragmentSpread !FragmentSpread
--   | SelectionInlineFragment !InlineFragment
--   deriving (Ord, Show, Eq, Lift, Generic)

-- type ServiceQuery (p :: Package snm mnm) (s :: Service snm mnm)
--   = [OneMethodQuery p s]

-- data OneMethodQuery (p :: Package snm mnm) (s :: Service snm mnm) where
--   OneMethodQuery
--     :: Maybe Text
--     -> NS (ChosenMethodQuery p) ms
--     -> OneMethodQuery p ('Service nm anns ms)

-- data ChosenMethodQuery (p :: Package snm mnm) (m :: Method snm mnm) where
--   ChosenMethodQuery
--     :: NP (ArgumentValue p) args
--     -> ReturnQuery p r
--     -> ChosenMethodQuery p ('Method mname anns args ('RetSingle r))

-- TODO: turn Hasura's ExecutableDefinition into a service query
-- hint#1: start with the following function, and then move up
--         (OperationDefinition > ExecutableDefinition > ExecutableDocument)
parseQuery ::
  forall (p :: Package') (s :: Symbol) pname ss sname sanns methods.
  ( p ~ 'Package pname ss,
    LookupService ss s ~ 'Service sname sanns methods
  ) =>
  Proxy p ->
  Proxy s ->
  GQL.SelectionSet ->
  Maybe (ServiceQuery p (LookupService ss s))
parseQuery p s = traverse toOneMethod
  where
    toOneMethod ::
      GQL.Selection ->
      Maybe
        ( OneMethodQuery
            ('Package pname ss)
            ('Service sname sanns methods)
        )
    toOneMethod (GQL.SelectionField flds)          = undefined
    toOneMethod (GQL.SelectionFragmentSpread frgs) = undefined
    toOneMethod (GQL.SelectionInlineFragment inls) = undefined

-- data Field
--   = Field
--   { _fAlias        :: !(Maybe Alias)
--   , _fName         :: !Name
--   , _fArguments    :: ![Argument]
--   , _fDirectives   :: ![Directive]
--   , _fSelectionSet :: !SelectionSet
--   } deriving (Ord, Show, Eq, Lift, Generic)

-- data FragmentSpread
--   = FragmentSpread
--   { _fsName       :: !Name
--   , _fsDirectives :: ![Directive]
--   } deriving (Ord, Show, Eq, Lift, Generic)

-- data InlineFragment
--   = InlineFragment
--   { _ifTypeCondition :: !(Maybe TypeCondition)
--   , _ifDirectives    :: ![Directive]
--   , _ifSelectionSet  :: !SelectionSet
--   } deriving (Ord, Show, Eq, Lift, Generic)

---

-- data Directive
--   = Directive
--   { _dName      :: !Name
--   , _dArguments :: ![Argument]
--   } deriving (Ord, Show, Eq, Lift, Generic)

-- data Argument
--   = Argument
--   { _aName  :: !Name
--   , _aValue :: !Value
--   } deriving (Ord, Show, Eq, Lift, Generic)

class ParseMethod (ms :: [Method Symbol Symbol]) where
  selectMethod :: T.Text -> NS Proxy ms

instance ParseMethod '[] where
  selectMethod = error "this should not be run"

instance
  (KnownSymbol mname, ParseMethod ms) =>
  ParseMethod ('Method mname manns args ret ': ms)
  where
  selectMethod wanted
    | wanted == mname = Z Proxy
    | otherwise = S (selectMethod wanted)
    where
      mname = T.pack $ nameVal (Proxy @mname)
