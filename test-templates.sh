#!/bin/sh

mkdir template-check
cd template-check

stack new muavro https://raw.githubusercontent.com/higherkindness/mu-haskell/master/templates/grpc-server-avro.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
cd muavro
stack build
cd ..

stack new muprotobuf https://raw.githubusercontent.com/higherkindness/mu-haskell/master/templates/grpc-server-protobuf.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
cd muprotobuf
stack build
cd ..

cd ..
rm -rf template-check
