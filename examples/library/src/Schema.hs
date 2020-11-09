{-# language CPP                        #-}
{-# language DataKinds                  #-}
{-# language DeriveAnyClass             #-}
{-# language DeriveGeneric              #-}
{-# language DerivingStrategies         #-}
{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language OverloadedStrings          #-}
{-# language PartialTypeSignatures      #-}
{-# language PolyKinds                  #-}
{-# language QuasiQuotes                #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TupleSections              #-}
{-# language TypeApplications           #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}
module Schema where

import           Data.Int                (Int64)
import qualified Data.Text               as T
import           Database.Persist.Sqlite (BackendKey (SqlBackendKey), toSqlKey)
import           Database.Persist.TH     (mkMigrate, mkPersist, persistLowerCase, share,
                                          sqlSettings)
import           GHC.Generics            (Generic)
import           Mu.GraphQL.Quasi        (graphql)
import           Mu.Schema               (FromSchema)

#if __GHCIDE__
graphql "Library" "examples/library/library.graphql"
#else
graphql "Library" "library.graphql"
#endif

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author json
  name T.Text
  UniqueName name
  deriving Show Generic
Book json
  title T.Text
  author AuthorId
  UniqueTitlePerAuthor title author
  deriving Show Generic
|]

toAuthorId :: Int64 -> AuthorId
toAuthorId = toSqlKey

newtype NewAuthor
  = NewAuthor { name :: T.Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "NewAuthor")

data NewBook
  = NewBook { title :: T.Text, authorId :: Integer }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema LibrarySchema "NewBook")
