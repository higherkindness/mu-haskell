#!/bin/sh

bundle install --gemfile docs/Gemfile --path vendor/bundle
BUNDLE_GEMFILE=./docs/Gemfile bundle exec jekyll serve -s docs -b /mu-haskell
