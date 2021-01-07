{-# language CPP                   #-}
{-# language DataKinds             #-}
{-# language FlexibleContexts      #-}
{-# language LambdaCase            #-}
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

import qualified Data.Aeson                        as JSON
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

data WritingMapping
  = ABook (Integer, Integer) | AnArticle (Integer, Integer)

type ServiceMapping = '[
    "Book"    ':-> (Integer, Integer)
  , "Article" ':-> (Integer, Integer)
  , "Author"  ':-> Integer
  , "Writing" ':-> WritingMapping
  ]

library :: [(Integer, T.Text, [(Integer, (T.Text, Integer))])]
library
  = [ (1, "Robert Louis Stevenson", [(1, ("Treasure Island", 4)), (2, ("Strange Case of Dr Jekyll and Mr Hyde", 4))])
    , (2, "Immanuel Kant", [(3, ("Critique of Pure Reason", 1))])
    , (3, "Michael Ende", [(4, ("The Neverending Story", 5)), (5, ("Momo", 3))])
    ]

articles :: [(Integer, T.Text, [(Integer, (T.Text, Integer))])]
articles
  = [ (1, "Fuencislo Robles", [(6, ("On Warm Chocolate", 4)), (2, ("On Cold Chocolate", 4))]) ]

libraryServer :: forall m i. (MonadServer m)
              => ServerT ServiceMapping i ServiceDefinition m _
libraryServer
  = resolver ( object @"Book"   ( field  @"id"      bookOrArticleId
                                , field  @"title"   bookTitle
                                , field  @"author"  bookOrArticleAuthor
                                , field  @"info"    bookInfo )
             , object @"Article" ( field @"id"      bookOrArticleId
                                , field  @"title"   articleTitle
                                , field  @"author"  bookOrArticleAuthor )
             , object @"Author" ( field  @"id"      authorId
                                , field  @"name"    authorName
                                , field  @"writings" authorBooks )
             , object @"Query"  ( method @"author"  findAuthor
                                , method @"book"    findBookTitle
                                , method @"authors" allAuthors
                                , method @"books"   allBooks' )
             , object @"Subscription" ( method @"books" allBooksConduit )
             , union @"Writing" (\case (ABook     x) -> pure $ unionChoice @"Book"    x
                                       (AnArticle x) -> pure $ unionChoice @"Article" x)
             )
  where
    findBook i = find ((==i) . fst3) library
    findArticle i = find ((==i) . fst3) articles

    bookOrArticleId (_, bid) = pure bid
    bookOrArticleAuthor (aid, _) = pure aid
    bookTitle (aid, bid) = pure $ fromMaybe "" $ do
      bk <- findBook aid
      ev <- lookup bid (thd3 bk)
      pure (fst ev)
    bookInfo (aid, bid) = pure $ do
      bk <- findBook aid
      ev <- lookup bid (thd3 bk)
      pure $ JSON.object ["score" JSON..= snd ev]
    articleTitle (aid, bid) = pure $ fromMaybe "" $ do
      bk <- findArticle aid
      ev <- lookup bid (thd3 bk)
      pure (fst ev)

    authorId = pure
    authorName aid = pure $ maybe "" snd3 (findBook aid)
    authorBooks aid = pure $ maybe [] (map (ABook . (aid,) . fst) . thd3) (findBook aid)
                           <> maybe [] (map (AnArticle . (aid,) . fst) . thd3) (findArticle aid)

    findAuthor rx = pure $ listToMaybe
      [aid | (aid, name, _) <- library, name =~ rx]

    findBookTitle rx = pure $ listToMaybe
      [(aid, bid) | (aid, _, books) <- library
                  , (bid, (title, _)) <- books
                  , title =~ rx]

    allAuthors = pure $ fst3 <$> library
    allBooks = [(aid, bid) | (aid, _, books) <- library, (bid, _) <- books]
    allBooks' = pure allBooks

    allBooksConduit :: ConduitM (Integer, Integer) Void m () -> m ()
    allBooksConduit sink = runConduit $ yieldMany allBooks .| sink
