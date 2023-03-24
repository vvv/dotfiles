# Set PATH, MANPATH, etc., for Homebrew.
eval "$(/opt/homebrew/bin/brew shellenv)"

if [ -d ~/.emacs.d/bin ]; then
    export PATH="$HOME/.emacs.d/bin:$PATH"
fi

for f in \
    ~/.aliases \
    ~/.functions \
    ~/.zsh-private \
    /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
do
    if [ -f $f ]; then
        source $f
    fi
done; unset f
