#!/bin/sh

set -e

[ $# -eq 1 ] || {
    echo "usage: ${0##*/} FILE" >&2
    exit 1
}

## `xclip' mangles Cyrillic alphabet.
# which xclip >/dev/null && { xclip -in "$1"; exit 0; }

gnuclient -q -batch -eval "
  (let ((default-directory \"`pwd`\"))
      (view-file \"$1\")
      (kill-new (filter-buffer-substring (point-min) (point-max)))
      (View-quit))"
