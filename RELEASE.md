# Releasing a new version of Mu-Haskell

1. Run `./test-schema.sh` and check that no errors are found
    - If found, abort and open issue
2. Publish a new release in GitHub:
    - Tag by running `git tag -a vX.Y -m "Release X.Y"`
    - Push the tag `git push --tags`
    - Create a new release in [GitHub](https://github.com/higherkindness/mu-haskell/releases/new) for that tag, or if using [`hub`](https://hub.github.com/hub-release.1.html), run `hub release create vX.Y`
3. For each package, run the following commands:
   - create the package: `cabal sdist package`
   - publish a candidate: `cabal upload route-to.tar.gz`, and check it
   - publish definitely: `cabal upload --publish route-to.tar.gz`
   - create the docs: `cabal v2-haddock --builddir="dist-newstyle" --haddock-for-hackage --enable-doc package`
   - publish docs: `cabal upload -d --publish route-to-docs.tar.gz`
4. Run `./test-templates.sh` and check that no errors are found
    - If found, update templates in `templates` folder and open a PR
