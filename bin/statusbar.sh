#!/bin/sh

while true ; do
    echo "[ `date +'%a %Y-%m-%d %H:%M'` || load: `loadavg-dzen.pl`" \
    "`battery-dzen.pl`]"
    sleep 3
done | dzen2 -y 885 -ta l -bg black \
    -fn -misc-fixed-medium-r-*-*-13-*-*-*-*-60-*-*
