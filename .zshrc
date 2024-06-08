if type -p fzf >&/dev/null; then
    source <(fzf --zsh)
fi

# Allow comments even in interactive shells.
setopt interactivecomments

if [[ -d ~/.zsh/functions ]]; then
    fpath=( ~/.zsh/functions "${fpath[@]}" )
    autoload -Uz gicl
    autoload -Uz gisbo
    autoload -Uz mkcd
    autoload -Uz wttr
fi

# Enable Git completions;
# see https://git-scm.com/book/en/v2/Appendix-A:-Git-in-Other-Environments-Git-in-Zsh
autoload -Uz compinit && compinit
#
# Setup Git prompt
f=$HOMEBREW_PREFIX/etc/bash_completion.d/git-prompt.sh
if [ -n "$f" ]; then
    source "$f"
    setopt PROMPT_SUBST
    PS1='[%c$(__git_ps1 " (%s)")]\$ '
    GIT_PS1_SHOWDIRTYSTATE=1
    GIT_PS1_SHOWUNTRACKEDFILES=1
fi
unset f

# Stop backward-kill-word (M-DEL) on path delimiter ('/').
autoload -U select-word-style
select-word-style bash

# Add `help` command à la Bash.
# See https://superuser.com/a/1563859
unalias run-help
autoload run-help

if [ -n "$HOMEBREW_PREFIX" ]; then
    HELPDIR=$(brew --prefix)/share/zsh/help
else
    HELPDIR=/usr/share/zsh/${ZSH_VERSION}/help
fi
alias help=run-help

if type -p nvim >&/dev/null; then
    export EDITOR=nvim
fi

# Emacs key bindings; see zshzle(1).
bindkey -e

if type -p direnv >&/dev/null; then
    eval "$(direnv hook zsh)"
fi

#XXX if type -p zoxide >&/dev/null; then
#XXX     eval "$(zoxide init zsh)"
#XXX fi

if type -p starship >&/dev/null; then
    eval "$(starship init zsh)"
fi

if type -p atuin >&/dev/null; then
    eval "$(atuin init zsh --disable-up-arrow)"
fi
