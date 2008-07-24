#!/bin/sh
xscreensaver -no-splash &
[ -f ~/.profile ] && . ~/.profile
statusbar.sh &
xterm -e 'reset; screen -DR' &
xmonad

xmessage -buttons logout,hibernate,restart,shutdown "What should `hostname` do?"
case $? in
  102) sudo s2disk;;
  103) sudo shutdown -r now;;
  104) sudo shutdown -h now;;
esac
