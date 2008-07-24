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

[ -f ~/.bash-work ] && /usr/local/bin/work-lan-p.sh && . ~/.bash-work
