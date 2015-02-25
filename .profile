# MacPorts Installer addition on 2015-02-18_at_09:00:30: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

[ -d ~/bin ] && export PATH="$HOME/bin:$PATH"

## Set `PATH' for new processes started by Spotlight.
## http://stackoverflow.com/a/3756686
launchctl setenv PATH "$PATH"

export PS1='\h:\W\$ '

[ -f ~/.functions ] && . ~/.functions
[ -f ~/.bashrc ] && . ~/.bashrc
