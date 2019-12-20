# Docs for Mu-Haskell

The documentation is built through a Jekyll site as base.

## Prerequisites

* You need to have [ruby >= 2.4.0](https://rvm.io/) installed on your system.
* [Bundler >= 2](https://bundler.io/v2.0/guides/bundler_2_upgrade.html) is also needed.


## Building the docs

To preview the site locally, execute the following command from the project root dir. This will install website dependencies under `docs/vendor/bundle`:

```bash
bundle install --gemfile docs/Gemfile --path vendor/bundle
```

Then, through this command, you will run the locally installed Jekyll instance to serve the site:


```bash
BUNDLE_GEMFILE=./docs/Gemfile bundle exec jekyll serve -s docs -b /mu-haskell
```


Finally, to have a look at the site, visit:

http://localhost:4000/mu-haskell
