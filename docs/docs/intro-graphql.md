---
layout: docs
title: Introduction for GraphQL
permalink: intro-graphql/
---

# Introduction to Mu-Haskell for GraphQL

This document will help you get started in writing your first GraphQL server using Mu-Haskell and `mu-graphql`!

## Using the Stack template

First of all, we've prepared a Stack template to boostrap your project easily, just run:

```sh
$ stack new your-project https://raw.githubusercontent.com/higherkindness/mu-haskell/master/templates/graphql-server.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
```

After doing this you'll have a compiling project with all the dependencies in place and a simple `Main.hs` file created.

## Defining your GraphQL Schema

The template will generate a `schema.graphql` file with a basic "hello" query, feel free to change it with your domain types. For the sake of this intro we'll use our beloved library example from the [Apollo GraphQL docs](https://www.apollographql.com/docs/apollo-server/schema/schema/):

```graphql
type Book {
  title: String!
  author: Author!
}

type Author {
  name: String!
  books: [Book!]!
}

type Query {
  authors: [Author!]!
  books: [Book!]!
}
```

This schema defines **three types: a book, an author, and a query**. All GraphQL APIs have a query type which defines all the possible queries you can ask. In our case, we have a query asking for all the authors in the library and another one for all the books. 📚 The exclamation marks all over the place (`!`) in GraphQL mean that the data is required to be returned, removing those would result in a `Maybe type` in Haskell! 😉

## Implementing the GraphQL Server

Now let's have a look at the already finished `src/Main.hs` and we'll go bit by bit explaining what is going on:

```haskell
graphql "ServiceDefinition" "schema.graphql"

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLAppQuery 8080 server (Proxy @"Query")

type ServiceMapping = '[
    "Book"   ':-> (Text, Text)
  , "Author" ':-> Text
  ]

library :: [(Text, [Text])]
library
  = [ ("Robert Louis Stevenson", ["Treasure Island", "Strange Case of Dr Jekyll and Mr Hyde"])
    , ("Immanuel Kant", ["Critique of Pure Reason"])
    , ("Michael Ende", ["The Neverending Story", "Momo"])
    ]

server :: forall m. MonadServer m => ServerT ServiceMapping ServiceDefinition m _
server = resolver
  ( object @"Book"
    ( field  @"title"  bookTitle
    , field  @"author" bookAuthor )
  , object @"Author"
    ( field  @"name"  authorName
    , field  @"books" authorBooks )
  , object @"Query"
    ( method @"authors" allAuthors
    , method @"books"   allBooks )
  )
  where
    bookTitle :: (Text, Text) -> m Text
    bookTitle (_, title) = pure title
    bookAuthor :: (Text, Text) -> m Text
    bookAuthor (auth, _) = pure auth

    authorName :: Text -> m Text
    authorName = pure
    authorBooks :: Text -> m [(Text, Text)]
    authorBooks name = pure $ (name,) <$> maybe [] snd (find ((==name) . fst) library)

    allAuthors :: m [Text]
    allAuthors = pure $ fst <$> library
    allBooks :: m [(Text, Text)]
    allBooks = pure [(author, title) | (author, books) <- library, title <- books]
```

## Schema generation explained

Notice this important line:

```haskell
graphql "ServiceDefinition" "schema.graphql"
```

This is where your `schema.graphql` file gets converted to a type-level expression that represents your Schema in terms of Mu-Haskell. Here, we're assigning the new schema the name `ServiceDefinition`, which will be later use in the next section below.

## Server declaration explained

As you've seen, we declare the GraphQL server the following way:

```haskell
server :: forall m. MonadServer m => ServerT ServiceMapping ServiceDefinition m _
server = resolver
  ( object @"Book"
    ( field  @"title"  bookTitle
    , field  @"author" bookAuthor )
  , object @"Author"
    ( field  @"name"  authorName
    , field  @"books" authorBooks )
  , object @"Query"
    ( method @"authors" allAuthors
    , method @"books"   allBooks )
  )
```

Here we use the given name `ServiceMapping` for the type-level generated GraphQL Schema that we've created for you!

To declare the server, you need provide a resolver for each field, using the `object` and `field` functions. Use `method` for the fields in "Query", since it's a special case for us (you can read more about [why here](https://higherkindness.io/mu-haskell/graphql/#implementing-the-server)).

> The ordering here is casual, you can declare each type wherever you like (as long as it is inside of the same tuple, of course) and it will still work! 🎉

## `ServiceMapping` explained

You might have noticed this strange piece of code:

```haskell
type ServiceMapping = '[
    "Book"   ':-> (Text, Text)
  , "Author" ':-> Text
  ]
```

This is where we establish the _relationship_ between the types. You may have notice that the GraphQL schema contained **recursive** references for the types, that is OK both for GraphQL and for Haskell, but it would not be the case for _other languages!_

## Resolvers explained

Now let's get to the meat. Although GraphQL will use the least amount of calls possible to find the result to your query, you need to provide "paths" or roads to every possible piece of your data. Although the example here is a hardcoded list, it could be anything ranging from a text file to a fully-fledged database! 🚀

```haskell
bookTitle :: (Text, Text) -> m Text
bookTitle (_, title) = pure title
bookAuthor :: (Text, Text) -> m Text
bookAuthor (auth, _) = pure auth

authorName :: Text -> m Text
authorName = pure
authorBooks :: Text -> m [(Text, Text)]
authorBooks name = pure $ (name,) <$> maybe [] snd (find ((==name) . fst) library)

allAuthors :: m [Text]
allAuthors = pure $ fst <$> library
allBooks :: m [(Text, Text)]
allBooks = pure [(author, title) | (author, books) <- library, title <- books]
```

What is that `m` that appears everywhere? Well, since we are using `ScopedTypeVariables`, that `m` refers to the above typeclass constrain `MonadServer m`. That is also why we need to lift with `pure` all of our results into the `MonadServer`.

We tend to put all those resolver functions in a `where` block, but of course you are free to move them around wherever you want! 😉

## Where to go from here

Here's a [more complete example](https://github.com/higherkindness/mu-haskell/tree/master/graphql/exe) of how a finished server looks like.

If you are confused or you'd like to know more about how this all works, have a look at our [GraphQL docs](https://higherkindness.io/mu-haskell/graphql/), or feel free to [open an issue](https://github.com/higherkindness/mu-haskell/issues) in the repo and we'll be happy to help!

Happy hacking! 🔥
