#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | Git
# |-------------------------------------------------------------------------------
# | 
# | Git is great.
# | 
# /

alias gs='git status'

alias nah="git reset --hard head && git clean -fd"

alias gstash="git add . && git commit -m 'stash'"
alias gpop="git reset --soft head~1 && git reset head"

alias gsu='git branch --set-upstream-to=origin/$(git rev-parse --abbrev-ref HEAD)'
alias gpu='git push -u origin $(git rev-parse --abbrev-ref HEAD)'

# destructive, ye' be warned
grho() {
    branch=$(git branch | grep \* | cut -d ' ' -f2)
    git reset --hard origin/$branch
}

gri() {
    git rebase -i head~$1
}

# git diff without the git
gdiff() {
    git diff --no-index $1 $2 | diff-so-fancy | less
}

alias glGraph='git log --graph --color=always --format="%Cred%h%Creset - %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" "$@"'
_gitLogLineToHash="echo {} | grep -o '[a-f0-9]\{7\}' | head -1"
_viewGitLogLine="$_gitLogLineToHash | xargs -I % sh -c 'git show --color=always % | delta'"

# gco - checkout git branch (including remote branches), sorted by most recent commit, limit 50 last branches
unalias gco 2> /dev/null
gco() {
  if [[ ! -z $@ ]]; then git checkout "$@"; return; fi

  local branches branch
  branches=$(git for-each-ref --count=50 --sort=-committerdate --format="%(refname:short)") &&
  branch=$(echo "$branches" |
           fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##" | sed "s/.*\///")
}

# glo - Replacement for git log, shows previews, allows selecting commits to view further
unalias glo 2> /dev/null
glo() {
    glGraph |
        fzf --no-sort --no-mouse --reverse --tiebreak=index --no-multi \
            --ansi --preview="$_viewGitLogLine" \
                --header "enter to view, ctrl-y to copy hash" \
                --bind "enter:execute:$_viewGitLogLine   | less -R" \
                --bind "ctrl-y:execute:$_gitLogLineToHash | pbcopy"
}

# the opposite of glo (shows incoming changes)
alias gro="glo ..@{u}"