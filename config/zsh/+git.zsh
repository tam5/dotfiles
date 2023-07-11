# ┌────────────────────────────────────────────────────────────────────────────┐
# │  +git.zsh
# └────────────────────────────────────────────────────────────────────────────┘

alias gs="git status"

alias wip="git add . && git commit -m 'wip' --no-verify"
alias nah="git reset --hard head && git clean -fd"
alias gpop="git reset --soft head~1 && git reset head"

### TODO make these smarter functions
alias ga="git add"
alias gap="git add --patch"
alias gc="git commit"
alias gp="git push"
alias gd="git diff"
# alias gco="git checkout"

function gco() {
  if [[ ! -z $@ ]]; then git checkout "$@"; return; fi

  local branches branch
  branches=$(git for-each-ref --count=50 --sort=-committerdate --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo $branch | rg -o "([^/]+/)?[^/]+$")
}
