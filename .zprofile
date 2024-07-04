if [ -f ~/.shell-paths ]; then
    source ~/.shell-paths
fi

if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

if [ -f ~/.aliases.priv ]; then
    source ~/.aliases.priv
fi

if [ -d ~/.functions ]; then
    fpath=( ~/.functions "${fpath[@]}" )
fi
