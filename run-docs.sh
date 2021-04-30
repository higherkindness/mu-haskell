#!/bin/sh

bundle config set path 'vendor/bundle'
bundle install --gemfile docs/Gemfile
BUNDLE_GEMFILE=./docs/Gemfile bundle exec jekyll serve -s docs -b /mu-haskell
