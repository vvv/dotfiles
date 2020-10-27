# --------------------------------------------------------------------
# PATH

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
_path_prepend_safe ~/.cargo/bin
_path_prepend_safe ~/.local/bin
_path_prepend_safe ~/bin

unset -f _path_prepend_safe _path_prepend
export PATH
# --------------------------------------------------------------------

### >= 10.10.3 (https://twitter.com/launchderp/status/585874100939137024)
# sudo launchctl config system path "$PATH"

export LC_CTYPE='en_US.UTF-8'


### History settings
shopt -s histappend  # don't overwrite the history on exit, append to it
if (( ${BASH_VERSINFO[0]} < 5 )); then
    export HISTSIZE=50000
else
    export HISTSIZE=-1  # don't limit the history list
fi
export HISTCONTROL='ignoreboth:erasedups'
# export HISTTIMEFORMAT='%FT%T%z%t'  # iso8601 format with a tab at the end


# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Make less more friendly for non-text input files, see lesspipe(1).
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


#XXX if type -f brew &>/dev/null; then
#XXX     ### http://docs.brew.sh/Gems,-Eggs-and-Perl-Modules.html#perl-cpan-modules-without-sudo
#XXX     eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)
#XXX fi
[ -d ~/perl5/perlbrew ] && . ~/perl5/perlbrew/etc/bashrc

# --------------------------------------------------------------------
# Git

if [[ $(uname) == Darwin ]]; then
    # Assuming git was installed with `brew install git`.
    _git_prompt=/usr/local/etc/bash_completion.d/git-prompt.sh
    _git_completion=/usr/local/etc/bash_completion.d/git-completion.bash
elif [[ $(uname) == Linux ]]; then
    # Tested on Ubuntu 20.04.
    _git_prompt=/etc/bash_completion.d/git-prompt
    _git_completion=/usr/share/bash-completion/completions/git
fi

if [[ -f $_git_prompt ]]; then
    . $_git_prompt
    export GIT_PS1_SHOWDIRTYSTATE=1
    export GIT_PS1_SHOWUNTRACKEDFILES=1
fi
if [[ -f $_git_completion ]]; then
    . $_git_completion
fi
unset _git_prompt _git_completion
# --------------------------------------------------------------------

# --------------------------------------------------------------------
# Prompt

__prompt_command() {
    local rc=$?

    PS1=
    if ((rc != 0)); then
        local red='\[\e[0;31m\]'
        local reset='\[\e[0m\]'
        local boom='\xf0\x9f\x92\xa5'
        PS1+="${red}$rc${reset}$(printf $boom) "
    fi
    PS1+='\A \h:\W'
    if command -v __git_ps1 >/dev/null; then
        PS1+='$(__git_ps1 " (%s)")'
    fi
    PS1+='\$ '
}
export -f __prompt_command
PROMPT_COMMAND=__prompt_command
# --------------------------------------------------------------------

if [[ $(uname) == Darwin ]]; then
    ssh-add -A &>/dev/null
fi

if [[ -e ~/.nix-profile/etc/profile.d/nix.sh && -z ${NIX_PATH:-} ]]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi

for f in \
    ~/.bash_aliases \
    ~/.functions \
    ~/.bash-private
do
    if [[ -f $f ]]; then
        . $f
    fi
done
