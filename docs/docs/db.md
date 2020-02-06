---
layout: docs
title: Databases
permalink: db/
---

# Databases

In this section of the docs, to have a clearer understanding of how one would use `mu-haskell` to talk to a database, we are going to have a walk through the example of [`with-persistent`](https://github.com/higherkindness/mu-haskell/tree/master/examples/with-persistent).

## First steps

We are going to start with our source of truth: the proto file.

```protobuf
syntax = "proto3";

import "google/protobuf/empty.proto";

package withpersistent;

message PersonRequest { int64 identifier = 1; }
message Person { PersonRequest pid = 1; string name = 2; int32 age = 3; }

service PersistentService {
  rpc getPerson (PersonRequest) returns (Person);
  rpc newPerson (Person) returns (PersonRequest);
  rpc allPeople (google.protobuf.Empty) returns (stream Person);
}
```

Maybe this example looks a bit contrived but bear with me, it covers a common use case when working with protobuf: that one of the messages has another message as its identifying key.

## Defining our Schema

You are going to need to enable the following extensions:

```haskell
{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DerivingVia                #-}
{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language OverloadedStrings          #-}
{-# language PolyKinds                  #-}
{-# language QuasiQuotes                #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}
```

As we've seen in the rest of the docs, we define our own data types to mirror our protobuf schema:

```haskell
grpc "PersistentSchema" id "with-persistent.proto"

newtype MPersonRequest = MPersonRequest
  { identifier :: Maybe Int64
  } deriving (Eq, Show, Ord, Generic)

instance ToSchema   Maybe PersistentSchema "PersonRequest" MPersonRequest
instance FromSchema Maybe PersistentSchema "PersonRequest" MPersonRequest

data MPerson = MPerson
  { pid  :: Maybe MPersonRequest
  , name :: Maybe T.Text
  , age  :: Maybe Int32
  } deriving (Eq, Ord, Show, Generic)

instance ToSchema   Maybe PersistentSchema "Person" MPerson
instance FromSchema Maybe PersistentSchema "Person" MPerson
```

Remember that all the magic starts with that first `grpc` line! ✨

You might have noticed that this time, we are not using `DeriveAnyClass`, so we need to write the instances for `ToSchema` and `FromSchema` on a separate line from our deriving clause, and let GHC fill them for us. This decision was made due to a current [bug in Persistent](https://github.com/yesodweb/persistent/issues/578), but hopefully it will be fixed in future versions. 🙂

## Integration with `persistent`

This is the bit that changes the most. Since we are interested in storing in our database only the `Person` entities, we are going to declare only that `Entity` using TemplateHaskell and `persistent-template`.

For our specific example we are going to integrate with `persistent-sqlite`, but feel free to use whatever database you prefer! 😉

```haskell
import           Data.Int
import qualified Data.Text               as T
import           Database.Persist.Sqlite
import           Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Person json
  name T.Text
  age  Int32
  deriving Show Generic
|]
```

Notice how we are deriving `Generic` also with Persistent's QuasiQuotes.

## Fixing the Id access issue

If you have worked with `persistent` before, you'll know that it generates it's own Ids, and this is very convenient. In our example, we'll get for free a `PersonId` field which is what we want to get with our `PersonRequest`.

But, how to derive the correct instance of `ToSchema` that `Mu` needs to work it's magic? How can we explicitly define this mapping?

We have created some utilities to help you integrate with Persistent in our [`mu-persistent` package](https://github.com/higherkindness/mu-haskell/tree/master/adapter/persistent). One of such is `WithEntityNestedId`, you can use it along with `DerivingVia` to fit our needs:

```haskell
type PersonFieldMapping
  = '[ "personAge" ':-> "age", "personName" ':-> "name" ]

deriving via (WithEntityNestedId "Person" PersonFieldMapping (Entity Person))
  instance ToSchema Maybe PersistentSchema "Person" (Entity Person)
```

Have in mind that we still need to define our own custom field mapping, in this case `PersonFieldMapping` so that the deriving via does its job properly.

## Running a pool of database connections

Now let's focus on the Server!

All you need to do is open the database once, and share the connection across all your services:

```haskell
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}

module Server where

import           Control.Monad.Logger
import           Mu.GRpc.Server
import           Mu.Server

main :: IO ()
main =
  runStderrLoggingT $
    withSqliteConn @(LoggingT IO) "example.db" $ \conn ->
      liftIO $ runGRpcApp msgProtoBuf 8080 (server conn)
```

We have decided in this example to use `LoggingT` from `monad-logger` and `runStderrLoggingT` to get some basic database logs to the console for free, but this is not a must!

## This actually does not work

Maybe you might have noticed that this example is not going to work yet. Unless you created `example.db` yourself, we need to define a "migration". Migrations are not actually *required* by Persistent, they are just a simple way to get an Sqlite database up and running.

We need a small tweak in our `Schema.hs`:

```diff
- mkPersist sqlSettings [persistLowerCase|
+ share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person json
  ...
```

And another one on our `Server.hs`:

```diff
main :: IO ()
main =
  runStderrLoggingT $
-   withSqliteConn @(LoggingT IO) "example.db" $ \conn ->
+   withSqliteConn @(LoggingT IO) "example.db" $ \conn -> do
+     runDb conn $ runMigration migrateAll
      liftIO $ runGRpcApp msgProtoBuf 8080 (server conn)
```

More on that strange `runDb` method in the next section! 😇

## Sample usage with a service

All the pieces are now in place, let's check the implementation of the `allPeople` service:

```haskell
allPeople
  :: SqlBackend
  -> ConduitT (Entity Person) Void ServerErrorIO ()
  -> ServerErrorIO ()
allPeople conn sink = runDb conn $
  runConduit $ selectSource [] [] .| liftServerConduit sink
```

As you can see, all the services need to be passed the `SqlBackend` connection as an argument.

Two interesting things we want to highlight here: we have provided a small helper called `runDb`, its implementation is quite simple and it exists due to **developer ergonomics**. We are basically saving you from writing lots of `liftIO $ flip runSqlPersistM`. 😉

The second one will be discussed in the next section.

## On streams and `Conduit`

Since we are going to work with streams, it is wonderful that `persistent` also provides methods to work with `Conduit` like, for example, `selectSource`. However...

```diff
- ConduitM () (Entity Person) m ()               -- the Monad in which persistent operates
+ ConduitT (Entity Person) Void ServerErrorIO () -- the Monad we know we want instead... 🤔
```

Well, have no fear my friend because we created yet another utility called `liftServerConduit`, born specifically to address this problem. Its type signature is:

```haskell
liftServerConduit
  :: MonadIO m
  => ConduitT a b ServerErrorIO r -> ConduitT a b m r
```

What is this type signature telling us? That is, we can turn any of the Conduits given as input, which work on the `ServerErrorIO` Monad from `mu-rpc`, into a Conduit working on another `IO`-like Monad. This is the case, in particular, of the Monad in which Persistent runs.


And that concludes our round-trip!

If you think that something is not clear or could be further improved, feel free to [open an Issue or Pull Request!](https://github.com/higherkindness/mu-haskell/issues) 😊
