#!/bin/sh

while true ; do
    echo "[ $(date +'%a %Y-%m-%d %H:%M') || load: $(loadavg-dzen.pl)" \
    "$(battery-dzen.sh)]"
    sleep 3
done | dzen2 -y 784 -ta l -bg black \
    -fn '-misc-fixed-medium-r-*-*-14-*-*-*-*-*-iso10646-1'
