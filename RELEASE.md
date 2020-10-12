# Releasing a new version of Mu-Haskell

This list assumes you have your Hackage username and password set in your `.cabal/config` file.

1. Run `./test-schema.sh` and check that no errors are found
    - If found, abort and open issue
2. Check that you can build with all compilers, and update project files if required:
    - `stack build` (for the current LTS)
    - `stack build --stack-yaml stack-nightly.yaml` (for the next version)
    - `cabal build all`
3. For each package, run the following commands:

    ```
    ./release-package.sh <package> <version>
    ```

4. Push and merge any pending changes
5. Run `./test-templates.sh` and check that no errors are found
    - If found, update templates in `templates` folder and open a PR
6. Publish a new release in GitHub:
    - Tag by running `git tag -a vX.Y -m "Release X.Y"`
    - Push the tag `git push --tags`
    - Create a new release in [GitHub](https://github.com/higherkindness/mu-haskell/releases/new) for that tag, or if using [`hub`](https://hub.github.com/hub-release.1.html), run `hub release create vX.Y`
