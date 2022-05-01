#!/usr/bin/env bash

set -eu

: "${CACHE_DIR:=$HOME/.cache/git/repositories}"

shopt -s globstar
shopt -s nullglob

for dir in $CACHE_DIR/*/*; do
    git -C $dir fetch --all --prune
done
