#!/usr/bin/env bash

set -eu

: "${CACHE_DIR:=$HOME/.cache/git/repositories}"

for url in "$@"; do
    repo="$(basename "$url")"
    baseurl="$(dirname "$url")"
    baseurl=${url/https:\/\//git@}
    basedir="$(dirname "${baseurl/\//:}")" # replace <name>/ with <name>:
    if [[ ! -d "${CACHE_DIR}/$basedir/$repo" ]]; then
        echo mkdir -p "${CACHE_DIR}/$basedir"
        echo git -C "${CACHE_DIR}/$basedir" clone --bare "$url"
    fi
done
