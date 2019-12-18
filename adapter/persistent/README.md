# mu-persistent

This are some utilities to integrate easily with `persistent` while using Mu.

## Usage

Say you have for example, the following `Entity`:

```haskell
mkPersist sqlSettings [persistLowerCase|
Person
  name T.Text
  age  Int32
  deriving Show Generic
|]
```

But in your `proto3`, the `Person` message is defined as:

```protobuf
message PersonRequest {
  int64 identifier = 1;
}

message Person {
  PersonRequest pid = 1;
  string name = 2;
  int32 age = 3;
}
```

How can you derive the correct `ToSchema` instances that `Mu` needs to work with that nested `Id` that belongs to another message? 🤔

You can use `WithEntityNestedId`, along with a custom field mapping and `DerivingVia` to do all the work for you!

```haskell
{-# language DerivingVia #-}

type PersonFieldMapping
  = '[ "personAge" ':-> "age", "personName" ':-> "name" ]

deriving via (WithEntityNestedId "Person" PersonFieldMapping (Entity Person))
  instance ToSchema Maybe PersistentSchema "Person" (Entity Person)
```

For a more complete example of usage, please check [the example with `persistent`](https://github.com/higherkindness/mu-haskell/blob/master/examples/with-persistent/src/Schema.hs).
