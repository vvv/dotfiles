#!/usr/bin/env bash
set -eu -o pipefail
# set -x
export PS4='+ [${BASH_SOURCE[0]##*/}:${LINENO}${FUNCNAME[0]:+:${FUNCNAME[0]}}] '

echo 'Hello, world!'
