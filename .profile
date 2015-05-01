# MacPorts Installer addition on 2015-02-18_at_09:00:30: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH:/usr/sbin:/sbin"
# Finished adapting your PATH environment variable for use with MacPorts.

[ -d ~/bin ] && export PATH="$HOME/bin:$PATH"

### >= 10.10.3 (https://twitter.com/launchderp/status/585874100939137024)
# sudo launchctl config system path "$PATH"

export PS1='\h:\W\$ '
export LC_CTYPE='en_US.UTF-8'

[ -f ~/.functions ] && . ~/.functions
[ -f ~/.bashrc ] && . ~/.bashrc
