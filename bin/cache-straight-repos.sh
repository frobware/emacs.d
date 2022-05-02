#!/usr/bin/env bash

set -eu
set -o pipefail

thisdir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

for i in $(fd --full-path -H .git/config ~/.emacs.d/straight/repos); do
    dir="$(dirname "$(dirname "$i")")"
    "${thisdir}"/cache-git-repo.bash "$(git -C "$dir" remote get-url origin)"
done
