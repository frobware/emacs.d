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

    (cd $(dirname $2); ln -sf "$1")
}

mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$srcdir/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "$srcdir/init.el" "$HOME/.emacs.d/init.el"
safe_link "$srcdir/modus-themes" "$HOME/.emacs.d/modus-themes"
safe_link "$srcdir/use-package" "$HOME/.emacs.d/use-package"
safe_link "$srcdir/gcmh" "$HOME/.emacs.d/gcmh"
safe_link "$srcdir/hrs" "$HOME/.emacs.d/hrs"
safe_link "$srcdir/magit" "$HOME/.emacs.d/magit"
