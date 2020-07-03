if [ $(uname) = Darwin ]; then
    if [ -f /etc/profile ]; then
        ## tmux runs as login shell; /etc/profile is read, which calls
        ## path_helper(8).  This workaround prevents path_helper from
        ## leaving $HOME/bin in the middle of the $PATH (I need it at
        ## the _beginning_ of the $PATH).
        ##
        ## See https://superuser.com/a/583502
        PATH=''
        . /etc/profile
    fi

    BREW_PREFIX=$(brew --prefix)
    if [ -n "$BREW_PREFIX" ]; then
        [ -f $BREW_PREFIX/etc/bash_completion.d/rustup ] &&
            . $BREW_PREFIX/etc/bash_completion.d/rustup
    fi
fi

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

# brew info sphinx-doc
_path_prepend_safe /usr/local/opt/sphinx-doc/bin

#XXX # pipx [https://pipxproject.github.io/pipx/]
#XXX # `pipx` is the recommended installation method of `sysfacts`
#XXX # [https://pypi.org/project/sysfacts/].
#XXX _path_prepend_safe ~/Library/Python/3.7/bin

unset -f _path_prepend_safe _path_prepend
export PATH

### >= 10.10.3 (https://twitter.com/launchderp/status/585874100939137024)
# sudo launchctl config system path "$PATH"

# export PS1='\h:\W\$ '
export LC_CTYPE='en_US.UTF-8'
# export EDITOR=emacs

### History settings
shopt -s histappend  # don't overwrite the history on exit, append to it

if (( ${BASH_VERSINFO[0]} < 5 )); then
    export HISTSIZE=50000
else
    export HISTSIZE=-1  # don't limit the history list
fi
export HISTCONTROL='ignoreboth:erasedups'
# export HISTTIMEFORMAT='%FT%T%z%t'  # iso8601 format with a tab at the end

#XXX if type -f brew &>/dev/null; then
#XXX     ### http://docs.brew.sh/Gems,-Eggs-and-Perl-Modules.html#perl-cpan-modules-without-sudo
#XXX     eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
#XXX fi
[ -d ~/perl5/perlbrew ] && . ~/perl5/perlbrew/etc/bashrc

if [[ $(uname) = Darwin ]]; then
    GIT_CORE=/Library/Developer/CommandLineTools/usr/share/git-core
else
    GIT_CORE=/usr/share/doc/git-1.8.3.1/contrib/completion
fi
[ -f $GIT_CORE/git-completion.bash ] && . $GIT_CORE/git-completion.bash
[ -f $GIT_CORE/git-prompt.sh ] && {
    . $GIT_CORE/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    # export PS1='\h:\W$(__git_ps1 " (%s)")\$ '
}

__prompt_command() {
    local rc=$?

    PS1=
    if ((rc != 0)); then
        local red='\[\e[0;31m\]'
        local reset='\[\e[0m\]'
        local boom='\xf0\x9f\x92\xa5'
        PS1+="${red}$rc${reset}$(printf $boom) "
    fi
    PS1+='\h:\W'
    if command -v __git_ps1 >/dev/null; then
        PS1+='$(__git_ps1 " (%s)")'
    fi
    PS1+='\$ '
}
export -f __prompt_command
PROMPT_COMMAND=__prompt_command

if [[ $(uname) = Darwin ]]; then
    ssh-add -A &>/dev/null
fi

[ -f ~/.functions ] && . ~/.functions
[ -f ~/.bash-private ] && . ~/.bash-private
