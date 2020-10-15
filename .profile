if [ -n "$BASH_VERSION" ]; then
    # We are running bash.
    [ -f ~/.bashrc ] && . ~/.bashrc
fi

if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
fi # added by Nix installer, modified by vvv
