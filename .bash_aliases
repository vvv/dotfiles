alias cg=cargo
alias cgc='cargo clippy'
alias cgcl='cargo clean'
alias cgf='cargo fmt'

alias dk=docker
alias dkr='docker run --rm -it'

alias e='emacsclient --no-wait'

alias gia='git add --patch'
alias gib='git branch'
alias gibr='git branch --remotes'
alias gibrc='git branch --remotes --contains'
alias gic='git commit'
alias gica='git commit --amend'
alias gicaH='git commit --amend --reuse-message=HEAD'
alias gico='git checkout'
alias gicv='git commit --verbose'
alias gicw='git commit --message WIP'
alias gid='git diff'
alias gif='git fetch --prune --all'
alias gig='git grep -nHE'
alias gil='git log'
alias gil1='git log --oneline'
alias gilb='git log --pretty=%B'
alias gim='git merge --ff-only'
alias gir='git rev-parse'
alias girs='git rev-parse --short'
alias gis='git status'
alias gisb='git show-branch'
# `gisbo` is not an alias, but it belongs `gi*` group of commands
gisbo() {
    local ref
    for ref in {origin,upstream}/{master,main}; do
        if git rev-parse --quiet --verify $ref >/dev/null; then
            git show-branch HEAD $ref "$@"
            return
        fi
    done
    echo >&2 'Cannot find the "main" reference'
    return 1
}
export -f gisbo
alias gisbu='git show-branch HEAD @{upstream}'

alias j=just
alias l=exa
alias vi=nvim
