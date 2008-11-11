#!/bin/sh

## Command to compare CSV (comma separated values) files:
##
##     diff-with.sh old.csv new.csv awk -F, '{
##       printf("----- %d -----\n", NR);
##       for (i=1; i <= NF; i++)
##         printf("%d  %s\n", i, $i); }'
##
## The following commands are equivalent:
##
##     diff-with.sh f1 f2 cat
##
##     diff -u f1 f2

set -e

[ $# -lt 3 ] && {
    echo "usage: ${0##*/} oldfile newfile program [arguments...]" 1>&2
    exit 1
}

old=$1; shift
new=$1; shift

which tempfile >&/dev/null && tmp=`tempfile` || tmp="/tmp/_XXX-$$"
trap "rm $tmp" 0

"$@" $old >$tmp
"$@" $new | diff -u -L "[mung] $old" -L "[mung] $new" $tmp -
