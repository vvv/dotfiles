logical_interface=umc-static

_interface() { sudo if${1} eth0=${logical_interface:-umc-dynamic}; }

case "$1" in
    '-d'|'-u') _interface down; return;; # disconnect
esac
# [ "$1" = '-d' ] && { _interface down; return $?; } # disconnect

/usr/local/bin/work-lan-p.sh && return

_interface up || return

. /usr/local/bin/work-proxy-vars.sh
sudo -E /etc/init.d/lastfmsubmitd restart
sudo -E /usr/local/sbin/setdate.sh

fetchmail
