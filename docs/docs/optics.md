---
layout: docs
title: Optics
permalink: optics/
---

# Optics

We created a new package `mu-optics` is available from **release v0.3 of Mu-Haskell**, and provides an easier API to build both servers and clients using lenses and prisms (probably the ultimate API 😉). This document aims to be a reference of how to use this library and the common use cases for it.

## Accessing fields with `#field`

When you want to refer to a method of your type-level schema, whereas in the server or the client, you can use the get operation from the lens (`^.`) in conjunction with the `OverloadedLabels` extension to access that field, as in this example. In this case, the label `#name` should have the same name as the field declared in the corresponding schema definition file.

```haskell
{-# language OverloadedLabels #-}

sayHello :: MonadServer m => HelloRequestMessage' -> m HelloReplyMessage'
sayHello (HelloRequestMessage nm) = pure $ record ("hi, " <> nm ^. #name)
```

## Generate records with `record` and `record1`

Sometimes you need to create values that match the generated type-level schema representations, for example, to construct the request of a certain service. Usually, those types will be records, and to help you achieve that task we provided the helpers `record` and `record1`:

```haskell
get :: GRpcConnection PersistentService 'MsgProtoBuf -> String -> IO ()
get client idPerson = do
  let request = read idPerson
  putStrLn $ "GET: is there some person with id: " ++ idPerson ++ "?"
  response <- client ^. #getPerson $ record1 request -- <- using `record1` to create a request
  putStrLn $ "GET: response was: " ++ show response
```

Why the difference? Well, due to some ambiguity in the context of our Schemas, we need to help GHC to know if the record we're creating contains only one field (`record1`) or more (`record`) contained in a tuple of elements.

> This design might be improved in the future, by using "[OneTuple](https://hackage.haskell.org/package/OneTuple-0.2.2.1/docs/Data-Tuple-OneTuple.html) to rule them all."

```haskell
add :: GRpcConnection PersistentService 'MsgProtoBuf -> String -> String -> IO ()
add client nm ag = do
  let person = record (Nothing, T.pack nm, read ag) -- <- using `record` to create Person, a more complex type
  putStrLn $ "ADD: creating new person " ++ nm ++ " with age " ++ ag
  response <- client ^. #newPerson person
  putStrLn $ "ADD: was creating successful? " ++ show response
```

## Generating enums with `enum`

Sometimes, besides using records, you'll have your types defined as something like enums, as in this protobuf example:

```protobuf
enum Weather {
  sunny = 0;
  cloudy = 1;
  rainy = 2;
}
```

As expected, we also provided you with the tools you need to construct a valid enum-like value with the helper `enum`:

```haskell
{-# language TypeApplications #-}

import Mu.Schema
import Mu.Schema.Optics

type Weather = Term WeatherProtocol (WeatherProtocol :/: "Weather")

sunnyDays :: Int -> [Weather]
sunnyDays n = replicate n (enum @"sunny") -- <- see the magic here! ✨
```

Simply enable `TypeApplications` and provide the value you are looking for to construct the enum! 🚀

## Accesing enums with prisms!

Following with the example above, if you need to read an enum value, you can do so using prisms!

```haskell
{-# language OverloadedLabels #-}

getWeather :: Weather -> IO ()
getWeather e
  | e `is` #sunny  = putStrLn "is sunny! 😄"
  | e `is` #cloudy = putStrLn "is cloudy 😟"
  | e `is` #rainy  = putStrLn "is rainy... 😭"
```

Again, notice the use of `OverloadedLabels` to refer to the possible enum values and our special `is` prism helper, which is just `is s k = isJust (preview k s)`, got it? isJust... badum tss! 🥁

## Accessing unions

Besides this, you have `_U0`, `_U1`, ... and `_Next`, with the goal of giving prisms for the different possibilities of a union. So, lets say you have a field that is an union of `String` and `Int`, you can get prisms using `#field % _U0` and `#field % _U1`.

> We know the naming of these might be terrible, but they follow the usual convention for this kind of stuff: `_` for a prism, `U` from "Union" and then the index.
