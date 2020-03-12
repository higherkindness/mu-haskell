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
import           Data.Text         (Text, toCaseFold)

import           Mu.GraphQL.Server
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLApp 8080 libraryServer (Proxy @"Query") (Proxy @"Mutation")

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
          , ObjectField "book" '[]
              '[ 'ArgSingle ('Just "title") '[] ('PrimitiveRef Text)]
              ('RetSingle ('ListRef ('ObjectRef "Book")))
          , ObjectField "authors" '[]
              '[] ('RetSingle ('ListRef ('ObjectRef "Author")))
          , ObjectField "books" '[]
              '[] ('RetSingle ('ListRef ('ObjectRef "Book")))
          ]
      , Object "Mutation" '[] '[]
      ]

type ServiceMapping = '[
    "Book"   ':-> (Integer, Integer)
  , "Author" ':-> Integer
  ]

library :: [(Integer, Text, [(Integer, Text)])]
library
  = [ (1, "Robert Louis Stevenson", [(1, "Treasure Island"), (2, "Strange Case of Dr Jekyll and Mr Hyde")])
    , (2, "Immanuel Kant", [(3, "Critique of Pure Reason")])
    , (3, "Michael Ende", [(4, "The Neverending Story"), (5, "Momo")])
    ]

libraryServer :: forall m. (MonadServer m) => ServerT ServiceMapping ServiceDefinition m _
libraryServer
  = Services $ (bookId :<||>: bookTitle :<||>: bookAuthor :<||>: H0)
               :<&>: (authorId :<||>: authorName :<||>: authorBooks :<||>: H0)
               :<&>: (noContext findAuthor
                      :<||>: noContext findBookTitle
                      :<||>: noContext allAuthors
                      :<||>: noContext allBooks
                      :<||>: H0)
               :<&>: H0 :<&>: S0
  where
    findBook i = find (\(id', _, _) -> i == id') library

    bookId (_, bid) = pure bid
    bookTitle (aid, bid)
      = case findBook aid of
          Nothing            -> pure ""
          Just (_, _, books) -> pure $ fromMaybe "" (lookup bid books)
    bookAuthor (aid, _) = pure aid

    authorId = pure
    authorName aid
      = case findBook aid of
          Nothing            -> pure ""
          Just (_, aname, _) -> pure aname
    authorBooks aid
      = case findBook aid of
          Nothing            -> pure []
          Just (_, _, books) -> pure $ map ((aid,) . fst) books

    findAuthor aname
      = case find (\(_, aname', _) -> toCaseFold aname == toCaseFold aname') library of
          Nothing          -> pure Nothing
          Just (aid, _, _) -> pure $ Just aid

    findBookTitle title
      = pure [(aid, bid) | (aid, _, books) <- library, (bid, title') <- books, toCaseFold title == toCaseFold title']

    allAuthors = pure [x | (x, _, _) <- library]

    allBooks = pure [(aid,bid) | (aid, _, books) <- library, (bid, _) <- books]
