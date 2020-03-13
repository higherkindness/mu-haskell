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

import           Data.List              (find)
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Proxy
import qualified Data.Text              as T
import           Text.Regex.TDFA
import           Text.Regex.TDFA.Text   ()

import           Mu.GraphQL.Annotations
import           Mu.GraphQL.Server
import           Mu.Rpc
import           Mu.Schema
import           Mu.Server

-- GraphQL App

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8080"
  runGraphQLApp 8080 libraryServer (Proxy @"Query") (Proxy @"Mutation")

type ServiceDefinition
  = 'Package ('Just "library")
      '[ Object "Book" '[]
        '[ ObjectField "id"     '[] '[] ('RetSingle ('PrimitiveRef Integer))
         , ObjectField "title"  '[] '[] ('RetSingle ('PrimitiveRef T.Text))
         , ObjectField "author" '[] '[] ('RetSingle ('ObjectRef "Author"))
         ]
      , Object "Author" '[]
        '[ ObjectField "id"    '[] '[] ('RetSingle ('PrimitiveRef Integer))
         , ObjectField "name"  '[] '[] ('RetSingle ('PrimitiveRef T.Text))
         , ObjectField "books" '[] '[] ('RetSingle ('ListRef ('ObjectRef "Book")))
         ]
      , Object "Query" '[]
         '[ ObjectField "author" '[]
              '[ 'ArgSingle ('Just "name") '[DefaultValue ('VCString ".*")] ('PrimitiveRef T.Text)]
              ('RetSingle ('OptionalRef ('ObjectRef "Author")))
          , ObjectField "book" '[]
              '[ 'ArgSingle ('Just "title") '[DefaultValue ('VCString ".*")] ('PrimitiveRef T.Text)]
              ('RetSingle ('OptionalRef ('ObjectRef "Book")))
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

library :: [(Integer, T.Text, [(Integer, T.Text)])]
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
    findBook i = find ((==i) . fst3) library

    bookId (_, bid) = pure bid
    bookTitle (aid, bid) = pure $ maybe "" (fromMaybe "" . lookup bid . trd3) (findBook aid)
    bookAuthor (aid, _) = pure aid

    authorId = pure
    authorName aid = pure $ maybe "" snd3 (findBook aid)
    authorBooks aid = pure $ maybe [] (map ((aid,) . fst) . trd3) (findBook aid)

    findAuthor rx = pure $ listToMaybe
      [aid | (aid, name, _) <- library, name =~ rx]

    findBookTitle rx = pure $ listToMaybe
      [(aid, bid) | (aid, _, books) <- library
                  , (bid, title) <- books
                  , title =~ rx]

    allAuthors = pure $ fst3 <$> library
    allBooks = pure [(aid, bid) | (aid, _, books) <- library, (bid, _) <- books]

-- helpers

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
