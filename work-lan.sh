logical_interface=umc-static

_interface() { sudo if${1} eth0=${logical_interface:-umc-dynamic}; }

case "$1" in
    '-d'|'-u') # disconnect
	_interface down
	for proto in http https ftp; do unset ${proto}_proxy; done
	return;;
esac

/usr/local/bin/work-lan-p.sh && return

_interface up || return

. /usr/local/bin/work-proxy-vars.sh
sudo -E /usr/local/sbin/setdate.sh

sudo -E /etc/init.d/lastfmsubmitd restart
fetchmail
