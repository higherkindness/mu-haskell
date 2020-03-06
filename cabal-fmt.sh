find . -name '*.cabal' -exec sh -c 'cabal-fmt -i $0' {} ';'
