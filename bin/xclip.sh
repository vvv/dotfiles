#!/bin/sh

set -e

[ $# -eq 1 ] || {
    echo "usage: ${0##*/} FILE" >&2
    exit 1
}

## `xclip' mangles Cyrillic alphabet.
# which xclip >/dev/null && { xclip -in "$1"; exit 0; }

gnuclient -q -batch -eval "
  (view-file \"$1\")
  (let ((transient-mark-mode nil))
    (copy-region-as-kill (point-min) (point-max)))
  (View-quit)"
