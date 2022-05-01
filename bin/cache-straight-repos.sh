#!/usr/bin/env bash

set -eu
set -o pipefail

for i in $(fd --full-path -H .git/config ~/.emacs.d/straight/repos); do
    dir="$(dirname $(dirname $i))"
    cache-git-repo.bash $(git -C $dir remote get-url origin)
done
