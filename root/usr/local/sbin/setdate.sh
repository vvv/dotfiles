#!/bin/sh

set -e

[ $UID -eq 0 ] || exit 1

d="$(HEAD kernel.org | grep '^Date: ' | cut -b 7-)" || exit 2

date --utc --set="$d"
