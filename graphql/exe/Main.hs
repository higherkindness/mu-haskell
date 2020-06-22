{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language OverloadedStrings     #-}
{-# language PartialTypeSignatures #-}
{-# language PolyKinds             #-}
{-# language ScopedTypeVariables   #-}
{-# language TemplateHaskell       #-}
{-# language TupleSections         #-}
{-# language TypeApplications      #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import           Data.Conduit
import           Data.Conduit.Combinators          (yieldMany)
import           Data.List                         (find)
import           Data.Maybe                        (fromMaybe, listToMaybe)
import           Data.Proxy
import qualified Data.Text                         as T
import           Text.Regex.TDFA                   ((=~))
import           Text.Regex.TDFA.Common            (fst3, snd3, thd3)

import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Mu.GraphQL.Quasi
import           Mu.GraphQL.Server
import           Mu.Schema
import           Mu.Server

#if __GHCIDE__
graphql "ServiceDefinition" "graphql/exe/schema.graphql"
#else
graphql "ServiceDefinition" "exe/schema.graphql"
#endif

-- GraphQL App

main :: IO ()
main = do
  putStrLn "starting GraphQL server on port 8000"
  let hm = addHeaders [
             ("Access-Control-Allow-Origin", "*")
           , ("Access-Control-Allow-Headers", "Content-Type")
           ]
  run 8000 $ hm $ graphQLApp libraryServer
    (Proxy @('Just "Query"))
    (Proxy @'Nothing)
    (Proxy @('Just "Subscription"))

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

libraryServer :: forall m i. (MonadServer m)
              => ServerT ServiceMapping i ServiceDefinition m _
libraryServer
  = resolver ( object @"Book"   ( field  @"id"      bookId
                                , field  @"title"   bookTitle
                                , field  @"author"  bookAuthor )
             , object @"Author" ( field  @"id"      authorId
                                , field  @"name"    authorName
                                , field  @"books"   authorBooks )
             , object @"Query"  ( method @"author"  findAuthor
                                , method @"book"    findBookTitle
                                , method @"authors" allAuthors
                                , method @"books"   allBooks' )
             , object @"Subscription" ( method @"books" allBooksConduit )
             )
  where
    findBook i = find ((==i) . fst3) library

    bookId (_, bid) = pure bid
    bookTitle (aid, bid) = pure $ maybe "" (fromMaybe "" . lookup bid . thd3) (findBook aid)
    bookAuthor (aid, _) = pure aid

    authorId = pure
    authorName aid = pure $ maybe "" snd3 (findBook aid)
    authorBooks aid = pure $ maybe [] (map ((aid,) . fst) . thd3) (findBook aid)

    findAuthor rx = pure $ listToMaybe
      [aid | (aid, name, _) <- library, name =~ rx]

    findBookTitle rx = pure $ listToMaybe
      [(aid, bid) | (aid, _, books) <- library
                  , (bid, title) <- books
                  , title =~ rx]

    allAuthors = pure $ fst3 <$> library
    allBooks = [(aid, bid) | (aid, _, books) <- library, (bid, _) <- books]
    allBooks' = pure allBooks

    allBooksConduit :: ConduitM (Integer, Integer) Void m () -> m ()
    allBooksConduit sink = runConduit $ yieldMany allBooks .| sink
