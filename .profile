[ -d ~/bin ] && PATH=~/bin:$PATH
export PATH

em () {
  if lsof `which emacs` | grep -q $USER; then
    gnuclient -q "$@"
  else
    emacs "$@" &
  fi
}
export -f em

dict () {
  /usr/bin/dict "$@" | less
}
export -f dict

[ -f ~/.bash-work ] && . ~/.bash-work
