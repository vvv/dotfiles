# --------------------------------------------------------------------
# PATH

if [[ $(uname) == Darwin && -f /etc/profile ]]; then
    ## tmux runs as login shell; /etc/profile is read, which calls
    ## path_helper(8).  This workaround prevents path_helper from
    ## leaving $HOME/bin in the middle of the $PATH (I need it at
    ## the _beginning_ of the $PATH).
    ##
    ## See https://superuser.com/a/583502
    PATH=''
    . /etc/profile
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

_path_prepend_safe /sbin
_path_prepend_safe /usr/sbin
_path_prepend_safe /usr/local/sbin
_path_prepend_safe /usr/local/bin
if [[ $(uname) == Darwin ]]; then
    # This is where unversioned symlinks for python, python-config, pip, &c.
    # are installed; see https://docs.brew.sh/Homebrew-and-Python
    _path_prepend_safe $(brew --prefix)/opt/python/libexec/bin
fi

# https://github.com/hlissner/doom-emacs
_path_prepend_safe ~/.emacs.d/bin

_path_prepend_safe ~/zig
_path_prepend_safe ~/.cargo/bin
_path_prepend_safe ~/.local/bin
_path_prepend_safe ~/bin

unset -f _path_prepend_safe _path_prepend
export PATH
# --------------------------------------------------------------------

export LC_CTYPE='en_US.UTF-8'

### History settings
shopt -s histappend  # don't overwrite the history on exit, append to it
if (( ${BASH_VERSINFO[0]} < 5 )); then
    export HISTSIZE=50000
else
    export HISTSIZE=-1  # don't limit the history list
fi
export HISTCONTROL='ignoreboth:erasedups'

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Make less more friendly for non-text input files, see lesspipe(1).
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

# --------------------------------------------------------------------
# Homebrew, shell completions; see https://docs.brew.sh/Shell-Completion

if type brew &>/dev/null; then
    for d in $(brew --prefix)/etc/bash_completion.d/*; do
        if [[ -r $d ]]; then
            . $d
        fi
    done
    unset d
fi
# --------------------------------------------------------------------

if [[ $(uname) == Darwin ]]; then
    d=/usr/local/opt/fzf/shell
else
    d=/usr/share/doc/fzf/examples
fi
for f in $d/{completion,key-bindings}.bash; do
    if [[ -f $f ]]; then
        . $f
    fi
done
unset f d

if [[ $(uname) != Darwin && -d /home/linuxbrew/.linuxbrew/etc/bash_completion.d ]]; then
    for f in /home/linuxbrew/.linuxbrew/etc/bash_completion.d/*; do
        . $f
    done
fi
unset f

# Set EDITOR to Vim for better `C-x C-e` experience;
# see `edit-and-execute-command` in bash(1)
for e in nvim vim vi; do
    if [[ -n $(type -p $e) ]]; then
        export EDITOR=$e
        break
    fi
done
unset e

if type -p direnv >&/dev/null; then
    eval "$(direnv hook bash)"
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
unset f
