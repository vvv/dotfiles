/usr/local/bin/work-lan-p.sh && return

#sudo ifup eth0=umc-dynamic || return
sudo ifup eth0=umc-static || return

. /usr/local/bin/work-proxy-vars.sh
sudo -E /etc/init.d/lastfmsubmitd restart
sudo -E /usr/local/sbin/setdate.sh
