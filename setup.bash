#!/usr/bin/env bash

# Copied from straight/radon

set -e

script="$(realpath "$0")"
srcdir="$(dirname "$script")"

safe_link() {
    if [[ -e "$2" && ! -L "$2" ]]; then
        echo "already exists and not a symlink: $2" >&2
        exit 1
    fi

    ln -sf "$1" "$2"
}

mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$srcdir/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "$srcdir/init.el" "$HOME/.emacs.d/init.el"
safe_link "$srcdir/versions.el" "$HOME/.emacs.d/straight/versions/frobware.el"
