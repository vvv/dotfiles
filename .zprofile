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

if [ -f ~/.aliases.priv ]; then
    source ~/.aliases.priv
fi

if [ -d "$(brew --prefix)/share/google-cloud-sdk" ]; then
    source "$(brew --prefix)/share/google-cloud-sdk/path.zsh.inc"
    source "$(brew --prefix)/share/google-cloud-sdk/completion.zsh.inc"
fi
