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

* The first one defines the name of the *schema* type, which includes the enumerations and the input objects in the schema.
* The second one defines the name of the *service declaration*, in which we find the (result) objects from the GraphQL schema.
* The third argument is the route to the file *with respect to the project root*.

```haskell
{-# language TemplateHaskell #-}

import Mu.GraphQL.Quasi

graphql "Schema" "ServiceDefinition" "schema.graphql"
```

This might be surprising for people already used to GraphQL, the separation between input objects and enumerations, and the rest of the objects may seem quite artificial. However, this is needed because Mu-Haskell strongly separates those part of a service which only hold data, from those which may have some behavior associated with it (sometimes called *resolvers*).

## Implementing a server

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

```haskell
{
  author(name: ".*Ende.*") {   --> 1. return a Maybe AuthorId
    name                       --> 2. from that (optional) AuthorId return a String
    books {                    --> 3. from that AuthorId return [(BookId, AuthorId)]
      title                    --> 4. from each (BookId, AuthorId) return a String
    }
  }
}

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
```

## Subscriptions as streams

## Comparison with other libraries
