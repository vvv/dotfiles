#!/bin/sh

set -e

[ `id -u` -eq 0 ] || exit 1

d="$(HEAD -t30 kernel.org | grep '^Date: ' | cut -b 7-)" || exit 2
[ -z "$d" ] && exit 3

date --utc --set="$d"
