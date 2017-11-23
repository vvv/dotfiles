BREW_PREFIX=$(brew --prefix)
if [[ -n $BREW_PREFIX ]]; then
    [ -f $BREW_PREFIX/etc/bash_completion.d/rustup ] &&
        . $BREW_PREFIX/etc/bash_completion.d/rustup
fi

if [[ $(uname) = Darwin ]]; then
    GIT_CORE=/Library/Developer/CommandLineTools/usr/share/git-core
else
    GIT_CORE=/usr/share/doc/git-1.8.3.1/contrib/completion
fi
[ -f $GIT_CORE/git-completion.bash ] && . $GIT_CORE/git-completion.bash
[ -f $GIT_CORE/git-prompt.sh ] && {
    . $GIT_CORE/git-prompt.sh
    export GIT_PS1_SHOWDIRTYSTATE=1
    export PS1='\h:\W$(__git_ps1 " (%s)")\$ '
}

[ -f ~/lib/tmuxinator/completion/tmuxinator.bash ] &&
    . ~/lib/tmuxinator/completion/tmuxinator.bash

ssh-add -A &>/dev/null
