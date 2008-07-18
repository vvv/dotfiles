xscreensaver -no-splash &
[ -d ~/bin ] && export PATH=~/bin:$PATH
statusbar.sh &
xterm -e 'reset; screen -DR' &
xmonad

xmessage -buttons logout,hibernate,restart,shutdown "What should `hostname` do?"
case $? in
  102) sudo shutdown s2disk;;
  103) sudo shutdown -r now;;
  104) sudo shutdown -h now;;
esac
