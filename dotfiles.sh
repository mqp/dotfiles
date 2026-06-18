#!/usr/bin/env bash

scriptdir=$(cd "$(dirname -- "$0")" ; pwd -P)

function symlink() {
    if [[ -e "$2" && ! -L "$2" ]] ; then
        echo "$2 exists and is not a symlink. Ignoring it." >&2
        return 1
    fi

    # "ln -sf source link" follows symlinks by default on ancient BSD (thus on macOS)
    # the fix on ancient BSD is to add -h, which would be incompatible with GNU
    # which doesn't do any of this nonsense and doesn't accept -h either
    #
    # Thus we get to -f ourselves because shell scripting is jank.
    [[ -L "$2" ]] && rm "$2"

    ln -sv "${scriptdir}/$1" "$2"
}

mkdir -p ~/.config
symlink emacs/.emacs.d ~/.config/emacs
