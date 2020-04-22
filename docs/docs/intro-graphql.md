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

The template will generate a `schema.graphql` file with a basic "hello" query, feel free to change it with your domain types. For the sake of this intro we'll use our beloved library example from the Apollo GraphQL docs:

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

## Implementing the GraphQL Server

Now let's have a look at the `src/Main.hs` file:

```haskell
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TemplateHaskell       #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}

module Main where

import           Data.Proxy
import qualified Data.Text         as T

import           Mu.GraphQL.Quasi
import           Mu.GraphQL.Server
import           Mu.Schema
import           Mu.Server

graphql "ServiceDefinition" "schema.graphql"

-- GraphQL App

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLAppQuery 8080 server (Proxy @"Query")

type ServiceMapping = '[]

server :: MonadServer m => ServerT ServiceMapping ServiceDefinition m _
server = resolver ( object @"Query" ( method @"hello" $ error "not implemented" ) )
```

Notice this important line:

```haskell
graphql "ServiceDefinition" "schema.graphql"
```

This is where your `schema.graphql` file gets converted to a type-level expression that represents your Schema in terms of Mu-Haskell.

You'll need to implement the resolvers and provide a custom mapping (`ServiceMapping` in our template) to make it work.

Here's a [complete example](https://github.com/higherkindness/mu-haskell/tree/master/graphql/exe) of how a finished server looks like.

## Where to go from here

If you are confused or you'd like to know more about how this all works, have a look at our [GraphQL docs](https://higherkindness.io/mu-haskell/graphql/), or feel free to [open an issue](https://github.com/higherkindness/mu-haskell/issues) in the repo and we'll be happy to help!

Happy hacking! 🚀
