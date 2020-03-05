#!/bin/sh

DOCSDIR=docs/haddock

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
  --hyperlink-source \
  core/schema core/rpc core/optics \
  adapter/avro adapter/protobuf adapter/persistent adapter/kafka \
  grpc/common grpc/client grpc/server graphql

echo "Setting Linuwial theme on Haddock generated docs"
find ${DOCSDIR} -name "ocean.css" -exec cp -rf docs/css/linuwial.css {} \;
