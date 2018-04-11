_path_prepend() {
    case ":$PATH:" in
        *:"$1":*) ;; # already there
        *) PATH="$1:$PATH";;
    esac
}

_path_prepend_safe() {
    if [[ $1 = *:* ]]; then
        echo "_path_prepend_safe: Argument may not contain ':'" >&2
        return 1
    fi
    if [ -d "$1" ]; then
        _path_prepend "$1"
    fi
}

# XXX 2017-03-26 (Sun)
# coreutils has the following notes:
#   The tools provided by GNU coreutils are prefixed with the character 'g' by
#   default to distinguish them from the BSD commands.
#   For example, cp becomes gcp and ls becomes gls.
#
#   If you want to use the GNU tools by default, add this directory to the front
#   of your PATH environment variable:
#       /opt/local/libexec/gnubin/

_path_prepend_safe /sbin
_path_prepend_safe /usr/sbin
_path_prepend_safe /usr/local/sbin
_path_prepend_safe /usr/local/bin
_path_prepend_safe ~/.cargo/bin # https://github.com/rust-lang-nursery/rustfmt
_path_prepend_safe ~/.cabal/bin # cabal install hasktags
_path_prepend_safe ~/.local/bin # stack install hlint
_path_prepend_safe ~/bin

unset -f _path_prepend_safe _path_prepend
export PATH

### >= 10.10.3 (https://twitter.com/launchderp/status/585874100939137024)
# sudo launchctl config system path "$PATH"

export PS1='\h:\W\$ '
export LC_CTYPE='en_US.UTF-8'
export HISTCONTROL='ignoreboth'
# export EDITOR=emacs

[ -x /opt/local/bin/bash ] && export SHELL=/opt/local/bin/bash

#XXX if type -f brew &>/dev/null; then
#XXX     ### http://docs.brew.sh/Gems,-Eggs-and-Perl-Modules.html#perl-cpan-modules-without-sudo
#XXX     eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
#XXX fi
[ -d ~/perl5/perlbrew ] && . ~/perl5/perlbrew/etc/bashrc

[ -f ~/.functions ] && . ~/.functions
[ -f ~/.bashrc ] && . ~/.bashrc
[ -f ~/.bash-private ] && . ~/.bash-private
