#!/usr/bin/env bash

thisdir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

for i in $(gh repo list -L 2000 ${1:-frobware} | awk '{print $1}'); do
    ${thisdir}/cache-git-repo.bash https://github.com/${i};
done
