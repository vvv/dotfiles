GIT_CORE=/Library/Developer/CommandLineTools/usr/share/git-core

[ -f $GIT_CORE/git-completion.bash ] && . $GIT_CORE/git-completion.bash
[ -f $GIT_CORE/git-prompt.sh ] && {
	. $GIT_CORE/git-prompt.sh
	export GIT_PS1_SHOWDIRTYSTATE=1
	export PS1='\h:\W$(__git_ps1 " (%s)")\$ '
}
