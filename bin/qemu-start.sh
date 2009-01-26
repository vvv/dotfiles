#!/bin/sh

set -e

qemu_prog=kvm
qemu_user=win
qemu_host=qemu-win  # see `/etc/hosts'

_rdp () { nc -w2 -z $qemu_host 3389; }
_time () { perl -wse print\ time; }

if ! _rdp; then
    ps -C $qemu_prog -o user --no-headers | grep -q "^${qemu_user}$" || {
	sudo su - $qemu_user -c 'qemu.sh -nographic </dev/null' \
	    &>~/.qemu-errors &
    }

    start=$(_time); stop=$(( $start + 95 )) # `~/.qemu-nic-timings' ==> 95
    until _rdp; do echo "$(_time) $start $stop"; done | dzen2-dbar | dzen2

    echo $(( $(_time) - $start )) >>~/.qemu-nic-timings
fi

rdesktop -g 95% $qemu_host &
