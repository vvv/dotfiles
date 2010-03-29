#!/bin/sh

set -e

grep -q '^tap0=' /etc/network/run/ifstate || sudo ifup tap0

kvm -m 256 -localtime -usb -net nic \
    -net tap,ifname=tap0,script=no,downscript=no -hda ~/hd.raw "$@"
