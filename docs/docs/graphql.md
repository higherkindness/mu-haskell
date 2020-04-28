---
layout: docs
title: GraphQL services
permalink: graphql/
---

# GraphQL services

[GraphQL](https://graphql.github.io/) defines a language for queries, mutations, and subscriptions much more powerful than RPC or REST-based microservices. The key ingredient is a more complex query language, in which you do not only specify what you want to obtain, but also precisely describe the parts of the data you require.

We are going to implement a server for the following GraphQL schema, which is roughly based on the one in the [Apollo Server docs](https://www.apollographql.com/docs/apollo-server/schema/schema/). For those not used to the GraphQL schema language, we define a `Query` type with two *fields*, which represent the different queries we can do against the server. The `author` field takes one *argument*; the exclamation mark means that they are *not* optional, although they have a default value. `Book`s and `Author`s define *object* types which can be further queried; note in particular that there is a recursive reference between them, this is allowed in GraphQL schemas.

```graphql
type Query {
  author(name: String! = ".*"): Author
  books: [Book!]!
}

type Book {
  id: Int!
  title: String!
  author: Author!
}

type Author {
  id: Int!
  name: String!
  books: [Book!]!
}
```

## Importing the schema

The first step is to import this schema as a type-level definition for Mu. The `graphql` function takes three arguments:

* The first one defines the name of the *service declaration*, in which we find the (result) objects from the GraphQL schema.
* The second one is the route to the file *with respect to the project root*.

```haskell
{-# language TemplateHaskell #-}

import Mu.GraphQL.Quasi

graphql "ServiceDefinition" "schema.graphql"
```

This might be surprising for people already used to GraphQL, the separation between input objects and enumerations, and the rest of the objects may seem quite artificial. However, this is needed because Mu-Haskell strongly separates those part of a service which only hold data, from those which may have some behavior associated with it (sometimes called *resolvers*).

## Mapping each object type

Unsurprisingly, in order to implement a server for this schema you need to define a resolver for each of the objects and fields. There's one question to be answered beforehand, though: how do you represent *result* type of those resolvers, that is, how do we represent a (**not** input) *object*? We define those using a *type mapping*, which specifies the type associated to each GraphQL object type, except for the root types like `Query`.

This is better explained with the example. The question here is: how do we represent an `Author`? Our type mapping says that simply using an `AuthorId`:

```haskell
{-# language TypeApplications, DataKinds #-}

type TypeMapping
  = '[ "Author" ':-> AuthorId
     , "Book"   ':-> (BookId, AuthorId) ]
```

This means *two* things:

1. The result the `author` method in `Query` should be `Maybe AuthorId`. We obtain this type by noticing that in the definition of that field, `Author` has no exclamation mark, so it's optional, and the type mapping says that `Author` is mapped to an `AuthorId`.
2. The resolver for each of the fields for `Author` take an *additional* argument given by this type mapping. For example, the resolver for the `name` field should have type `AuthorId -> m String`, the argument coming from the type mapping, and the result type being defined by the schema.

You might be wondering why this is so complicated? The reason is that we don't want to do too much work upfront. In a traditional RPC service you would return the *whole* `Author`, with every field inside. In contrast, a GraphQL query defines *exactly* which fields are required, and we only want to run the resolvers we need. On the other hand, we still need to have a way to connect the dots, and we use the author identifier for that.

The following schema shows the way we traverse a GraphQL query and the types involved in it.

```graphql
{
  author(name: ".*Ende.*") {   --> 1. return a Maybe AuthorId
    name                       --> 2. from that (optional) AuthorId return a Text
    books {                    --> 3. from that AuthorId return [(BookId, AuthorId)]
      title                    --> 4. from each (BookId, AuthorId) return a Text
    }
  }
}
```

Note that we could have made `Book` be represented simply by a `BookId` and then query some database to figure our the author. However, in this case we assume this query is going to be quite common, and we cache this information since the beginning. Note that from the implementation point of view, the resolver for the `author` field of `Book` should have the type:

```haskell
bookAuthor :: (BookId, AuthorId) -> m AuthorId
bookAuthor (_, aid) = pure aid
```

The argument and result types come from the type mapping, since they are both object types. Given that we have cached that information, we can return it right away.

## Implementing the server

The whole implementation looks as a big list defining each of the resolvers for each of the objects and their fields. There's only one subtlety: for *root* operations we use `method` instead of `field`. The reason is that those fields do not take any information passed by, they are the initial requests.

```haskell
{-# language ScopedTypeVariables, PartialTypeSignatures #-}

libraryServer :: forall m. (MonadServer m)
              => ServerT TypeMapping ServiceDefinition m _
 = resolver
      ( object @"Query"  ( method @"author" findAuthor
                         , method @"books"  allBooks )
      , object @"Author" ( field  @"id"     authorId
                         , field  @"name"   authorName
                         , field  @"books"  authorBooks )
      , object @"Book"   ( field  @"id"     bookId
                         , field  @"author" bookAuthor
                         , field  @"title"  bookTitle ) )
  where -- Query fields
        findAuthor :: Text -> m (Maybe AuthorId)
        allBooks   :: m [(BookId, AuthorId)]
        -- Author fields
        authorId    :: AuthorId -> m Int
        authorName  :: AuthorId -> m Text
        authorBooks :: AuthorId -> m [(BookId, AuthorId)]
        -- Book fields
        bookId     :: (BookId, AuthorId) -> m Int
        bookAuthor :: (BookId, AuthorId) -> m AuthorId
        bookAuthor (_, aid) = pure aid
        bookTitle  :: (BookId, AuthorId) -> m Text
        -- implementation
```

In the above code we have defined all fields in a big `where` block, but of course those may be defined as top-level functions, or inline in call to `field` or `method`.

The final touch is to start the GraphQL server defined by `libraryServer`. The `Mu.GraphQL.Server` module defines tons of different ways to configure how the server behaves; the simplest option just requires a port and the name of the root type for queries.

```haskell
main = runGraphQLAppQuery 8080 libraryServer (Proxy @"Query")
```

## Mutations

Queries are not the only [operation](https://graphql.github.io/learn/queries/#operation-name) supported by GraphQL. The next simpler one are *mutations*. The format of requests to the server do not change between them both, but the semantics do, as hinted by their names: whereas queries are intended for requests which do not change the underlying data, mutations are the converse.

Unfortunately, we cannot guarantee those properties in the Mu handlers: in both cases we can perform any operation allowed by `IO`. The bright side is that implementing the mutation part of a GraphQL schema looks exactly like implementing the query part. The only difference is that we can no longer use the `runGraphQLAppQuery` function to start the server, we need to use the more complex variant in which you specify the names of query, mutations, and subscription types.

```haskell
main = runGraphQLApp 8080 libraryServer
         (Proxy @('Just "Query"))
         (Proxy @('Just "Mutation"))
         (Proxy @Nothing)
```

GraphQL does not mandate for any of these sections to be present, hence the use of a (type-level) `Maybe` to indicate whether the corresponding operation is present or absent.

## Subscriptions as streams

The third type of operations are _subscriptions_. In contrast to queries and mutations, which return a single value, subscriptions keep an open connection from which a stream of values can be obtained. Within Mu, these streams are represented using [Conduit](https://github.com/snoyberg/conduit). In particular, a subscription resolver gets an additional _sink_ argument to which you should write the returned values.

For example, let's create a version of `allBooks` which produces a stream of books instead of a single list. As discussed above, the argument is the sink to where elements must be "dumped".

```haskell
allBooksStream :: ConduitM (BookId, AuthorId) Void m () -> m ()
allBooksStream sink = runConduit $ yieldMany allBooks .| sink
```

We do not want to repeat here the awesome [Conduit tutorial](https://github.com/snoyberg/conduit#synopsis), so we shall give just a few strokes of how it works. The `yieldMany` combinator simply takes a list and turns it into a stream. Then we connect that stream to the provided `sink` by means of `(.|)`. All this on itself does nothing: a Conduit is just a description of a computation. To really execute it, we wrap everything on `runConduit`.

Of course, in real code you would not just return a list. The Conduit ecosystem has adapter to the file system, [databases]({% link docs/db.md %}), messaging queues, and many others.

## Comparison with other libraries

There are other libraries targeting GraphQL server definition in Haskell: `graphql-api` and Morpheus GraphQL. The latter also supports defining GraphQL *clients*, a feature not (yet) implemented in Mu.

[`graphql-api`](https://github.com/haskell-graphql/graphql-api#readme) shares with Mu the encoding of the GraphQL schema in the type-level. In fact, as the [tutorial](https://haskell-graphql-api.readthedocs.io/en/latest/tutorial/Introduction.html) shows, its encoding is much closer to GraphQL's schema definition.

```haskell
type Hello
  = Object "Hello" '[]
           '[ Argument "who" Text :> Field "greeting" Text ]
```

This is expected: Mu's ability to target both RPC and GraphQL microservices means that sometimes there's some mismatch.

[Morpheus GraphQL](https://morpheusgraphql.com/) also exposes GraphQL servers from Haskell code. Morpheus shared with Mu the ability to import a GraphQL schema into Haskell code. However, the types and fields are not represented by a type-level encoding, but *directly* as Haskell *records*.

```haskell
data GreetingArgs = GreetingArgs { argname :: Text } deriving (Generic, GQLType)
data Hello m = Hello { greeting :: GreetingArgs -> m Text } deriving (Generic, GQLType)
```

At the moment of writing, Mu has the ability to use records for schema types. In GraphQL terms, that means that you can use Haskell records for input objects and enumerations, but resolvers for each object fields need to be defined separately, as described above.

Another interesting comparison point is how the different libraries ensure that only the required data is ever consulted. This is quite important, since otherwise we might end up in infinite loops (find an author, query the books, for each book query the author, for each author the books, ...). Both `graphql-api` and Morpheus rely on Haskell's laziness, whereas Mu asks to define a type mapping which is then used as connecting point between objects.
