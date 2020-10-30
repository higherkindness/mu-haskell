#!/bin/sh

mkdir template-check
cd template-check

# copy files (cannot use .. in Stack)
cp ../templates/*.hsfiles .

stack new muavro grpc-server-avro.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
cd muavro
stack build
cd ..

stack new muprotobuf grpc-server-protobuf.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
cd muprotobuf
stack build
cd ..

stack new mugraphql graphql-server.hsfiles -p "author-email:haskell.curry@47deg.com" -p "author-name:Haskell Curry"
cd mugraphql
stack build
cd ..

cd ..
rm -rf template-check
