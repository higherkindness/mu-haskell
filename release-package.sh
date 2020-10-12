#!/bin/sh

cabal sdist ${1}
cabal upload dist-newstyle/sdist/${1}-${2}.tar.gz
cabal upload --publish dist-newstyle/sdist/${1}-${2}.tar.gz
echo "Check that it has been published correctly, and press Enter"
read
cabal v2-haddock --builddir="dist-newstyle" --haddock-for-hackage --enable-doc ${1}
cabal upload -d --publish dist-newstyle/${1}-${2}-docs.tar.gz
