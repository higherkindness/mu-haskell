{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Data.List         (find)
import           Data.Maybe        (fromMaybe)
import           Data.Proxy
import           Data.Text         (Text)

import           Mu.GraphQL.Server
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

main :: IO ()
main = runGraphQLApp 8080 libraryServer (Proxy @"Query") (Proxy @"Mutation")

type ServiceDefinition
  = 'Package ('Just "library")
      '[ Object "Book" '[]
        '[ ObjectField "id"     '[] '[] ('RetSingle ('PrimitiveRef Integer))
         , ObjectField "title"  '[] '[] ('RetSingle ('PrimitiveRef Text))
         , ObjectField "author" '[] '[] ('RetSingle ('ObjectRef "Author"))
         ]
      , Object "Author" '[]
        '[ ObjectField "id"    '[] '[] ('RetSingle ('PrimitiveRef Integer))
         , ObjectField "name"  '[] '[] ('RetSingle ('PrimitiveRef Text))
         , ObjectField "books" '[] '[] ('RetSingle ('ListRef ('ObjectRef "Book")))
         ]
      , Object "Query" '[]
         '[ ObjectField "author" '[]
              '[ 'ArgSingle ('Just "name") '[] ('PrimitiveRef Text)]
              ('RetSingle ('OptionalRef ('ObjectRef "Author")))
          ]
      , Object "Mutation" '[] '[]
      ]

type ServiceMapping = '[
    "Book"   ':-> (Integer, Integer)
  , "Author" ':-> Integer
  ]

library :: [(Integer, Text, [(Integer, Text)])]
library
  = [ (1, "alex", [(1, "haskell is nice"), (2, "haskell is cool")])
    , (2, "kant", [(3, "critique of pure reason")])]

libraryServer :: forall m. (MonadServer m) => ServerT ServiceMapping ServiceDefinition m _
libraryServer
  = Services $ (bookId :<||>: bookTitle :<||>: bookAuthor :<||>: H0)
               :<&>: (authorId :<||>: authorName :<||>: authorBooks :<||>: H0)
               :<&>: (noContext findAuthor :<||>: H0)
               :<&>: H0 :<&>: S0
  where bookId (_, bid) = pure bid
        bookTitle (aid, bid)
          = case find (\(aid', _, _) -> aid == aid') library of
              Nothing            -> pure ""
              Just (_, _, books) -> pure $ fromMaybe "" (lookup bid books)
        bookAuthor (aid, _) = pure aid

        authorId = pure
        authorName aid
          = case find (\(aid', _, _) -> aid == aid') library of
              Nothing            -> pure ""
              Just (_, aname, _) -> pure aname
        authorBooks aid
          = case find (\(aid', _, _) -> aid == aid') library of
              Nothing            -> pure []
              Just (_, _, books) -> pure $ map ((aid, ) . fst) books

        findAuthor aname
          = case find (\(_, aname', _) -> aname == aname') library of
              Nothing          -> pure Nothing
              Just (aid, _, _) -> pure $ Just aid
