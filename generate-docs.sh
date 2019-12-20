#!/bin/sh

echo "Removing previous docs"
rm -rf docs/haddock
echo "Generating new docs"
DOCSDIR=`stack haddock --no-haddock-deps mu-schema 2>&1 | pcregrep -o1 -M "Updating Haddock index for local packages in\s(.*)\/doc\/index\.html"`
echo "Copying new docs"
cp -r ${DOCSDIR}/doc docs/haddock
