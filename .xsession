#!/bin/sh
setxkbmap us,ru ,winkeys ctrl:swapcaps grp_led:scroll grp:toggle
xscreensaver -no-splash &
[ -f ~/.profile ] && . ~/.profile
statusbar.sh &
xterm -e 'reset; screen -DR' &
xmonad

gmessage -nearmouse -buttons _logout,_restart,_shutdown \
  "What should `hostname` do?"
case $? in
  102) sudo shutdown -r now;;
  103) sudo shutdown -h now;;
esac
