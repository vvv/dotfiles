/usr/local/bin/work-lan-p.sh && return

sudo ifup eth0 || return

. /usr/local/bin/work-proxy-vars.sh
sudo -E /etc/init.d/lastfmsubmitd restart
sudo -E /usr/local/sbin/setdate.sh
