#!/bin/sh

setxkbmap -rules evdev us,ru ,winkeys ctrl:swapcaps grp_led:scroll grp:toggle
xscreensaver -no-splash &
[ -f ~/.profile ] && . ~/.profile
xsetroot -solid '#1a1a1a'
statusbar.sh &
xterm -e 'reset; screen -DR' &
which synclient &>/dev/null && DISPLAY=:0 synclient TapButton1=1
xmonad

[ "`hostname`" = 'hobbiton' ] || {
    gxmessage -nearmouse -buttons _logout,_restart,_shutdown \
	"What should `hostname` do?"
    case $? in
	102) sudo shutdown -r now;;
	103) sudo shutdown -h now;;
    esac
}
