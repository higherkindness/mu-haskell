{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (finally)
import Mu.GRpc.Client.Record
import Mu.GRpc.Client.TyApps
import Mu.GRpc.Server
import Mu.Server hiding (resolver)
import Network.HTTP2.Client
import ProtoExample
import Prelude

------------------------------------------------------------------------------
-- app

runServer :: IO ()
runServer = do
  runGRpcApp msgProtoBuf 8070 grpcServer

------------------------------------------------------------------------------
-- grpc server api

grpcServer :: MonadServer m => SingleServerT i Service m _
grpcServer =
  singleService
    ( method @"SayHello" sayHello
    )

sayHello :: MonadServer m => HelloRequestMessage -> m HelloReplyMessage
sayHello (HelloRequestMessage nm) = do
  case nm of
    -- in some cases we want to throw an error, here when the name sent is 'Bob'
    "Bob" -> throwError $ ServerError NotFound "Bob not there"
    _ -> pure $ HelloReplyMessage ("hi, " <> nm)

------------------------------------------------------------------------------
-- grpc client calls

outboundSayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply Text)
outboundSayHello' host port req = do
  attempt <- setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- fmap (\(HelloReplyMessage r) -> r) <$> outBoundSayHello c (HelloRequestMessage req)
      pure x
    _ -> undefined

outBoundSayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
outBoundSayHello = gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello"

------------------------------------------------------------------------------
-- test

runTest :: IO ()
runTest = do
  putStrLn "testing things..."
  aliceResult <- outboundSayHello' "127.0.0.1" 8070 "Alice"
  putStr "The result for saying hello to Alice: "
  print (show aliceResult)
  bobResult <- outboundSayHello' "127.0.0.1" 8070 "Bob"
  -- bobResult should give a valid error from the server (NotFound), instead of a generic "not enough bytes"
  putStr "The result for saying hello to Bob: "
  print bobResult
  putStrLn "done with the haskell test, waiting 10 seconds before shutting down..."
  threadDelay $ 10 * 1000000 -- wait 10 seconds to allow running grpcurl against the server, too.

main :: IO ()
main = do
  server <- Async.async runServer
  finally runTest (Async.cancel server)
