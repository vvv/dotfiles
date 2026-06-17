#XXX-DELETEME if [ -f ~/.shell-paths ]; then
#XXX-DELETEME     source ~/.shell-paths
#XXX-DELETEME fi

if type -p fzf >&/dev/null; then
    source <(fzf --zsh)
fi

# Stop eating space before pipe symbol!
ZLE_REMOVE_SUFFIX_CHARS=''

# Don't auto-remove the trailing slash from directory names.
setopt no_auto_remove_slash

# Don't add command line to history when it starts with space.
setopt HIST_IGNORE_SPACE

# Colored Tab completions.
# Similar to `set colored-completion-prefix` in ~/.inputrc for Bash.
zstyle -e ':completion:*:default' list-colors \
    'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==1;35=0}:${(s.:.)LS_COLORS}")'

# Allow comments even in interactive shells.
setopt interactivecomments

if [[ -d ~/.zsh/functions ]]; then
    fpath=( ~/.zsh/functions "${fpath[@]}" )
    autoload -Uz cg
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

# Use `C-x C-e` to edit the current command line in the editor.
bindkey '^Xe' edit-command-line

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
