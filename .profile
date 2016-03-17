_path_prepend() {
    case ":$PATH:" in
        *:"$1":*) ;; # already there
        *) PATH="$1:$PATH";;
    esac
}

_path_prepend_safe() {
    if [ -d "$1" ]; then
        _path_prepend "$1"
    fi
}

_path_prepend /usr/sbin:/sbin
_path_prepend /usr/local/bin
[ -d /opt/local ] && _path_prepend /opt/local/bin:/opt/local/sbin # MacPorts
_path_prepend_safe /opt/packer # https://www.packer.io/
_path_prepend_safe ~/Library/Python/2.7/bin # mavlink
_path_prepend_safe ~/bin
_path_prepend_safe /opt/local/libexec/qt4/bin # qt4-mac

unset -f _path_prepend_safe _path_prepend
export PATH

### >= 10.10.3 (https://twitter.com/launchderp/status/585874100939137024)
# sudo launchctl config system path "$PATH"

export PS1='\h:\W\$ '
export LC_CTYPE='en_US.UTF-8'

[ -f ~/.functions ] && . ~/.functions
[ -f ~/.bashrc ] && . ~/.bashrc
