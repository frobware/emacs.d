#!/usr/bin/env bash

set -eu

: "${CACHE_DIR:=$HOME/.cache/git/repositories}"

for url in "$@"; do
    without_proto="${url#*:\/\/}"
    without_auth="${without_proto##*@}"
    [[ $without_auth =~ ^([^:\/]+)(:[[:digit:]]+\/|:|\/)?(.*) ]]
    PROJECT_HOST="${BASH_REMATCH[1]}"
    PROJECT_PATH="${BASH_REMATCH[3]}"
    path="${CACHE_DIR}/${PROJECT_HOST}/$PROJECT_PATH"
    [[ $path == *.git ]] || path="${path}.git"
    if [[ ! -d "$path" ]]; then
        project_dir="$(dirname "$path")"
        echo mkdir -p "${project_dir}"
        echo /usr/bin/git -C "${project_dir}" clone --bare "${url} ${url##*/}.git"
    fi
done
