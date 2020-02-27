find . -name '*.cabal' -exec sh -c 'cabal-fmt $0 > output.tmp; mv output.tmp $0' {} ';'
