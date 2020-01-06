#!/bin/sh

DOCSDIR=docs/haddock

echo "Installing required packages"
stack install standalone-haddock

echo "Removing previous docs"
rm -rf ${DOCSDIR}

echo "Building the project"
stack clean && stack build

echo "Generating new docs"
stack exec --no-ghc-package-path standalone-haddock -- -o ${DOCSDIR} \
  --compiler-exe=$(stack path --compiler-exe) \
  --dist-dir=$(stack path --dist-dir) \
  --package-db=$(stack path --snapshot-pkg-db) \
  --package-db=$(stack path --local-pkg-db) \
  core/schema core/rpc \
  adapter/avro adapter/protobuf adapter/persistent \
  grpc/client grpc/server \
  compendium-client
