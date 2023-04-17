# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

if [ -d ~/.emacs.d/bin ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

if [ -f ~/.aliases ]; then
    source ~/.aliases
fi

if [ -d ~/.functions ]; then
    fpath=( ~/.functions "${fpath[@]}" )
fi
