# ┌────────────────────────────────────────────────────────────────────────────┐
# │  +navigation.zsh
# └────────────────────────────────────────────────────────────────────────────┘

alias -- -="cd -"
alias ..="cd ../"
alias home="cd ~"
alias desk="cd ~/Desktop"
alias code="cd ~/Code"
alias dotfiles="cd ~/Code/dotfiles"
alias dmd="cd ~/.config/doom"
alias emd="cd ~/.config/emacs"

## gls --color --group-directories-first --time-style=long-iso --human-readable -o --almost-all --directory * .*, considering
alias ls="exa --long --color-scale --all --group-directories-first --binary --time-style long-iso"
alias ll="ls"

j() {
    finder_cmd="fd --max-depth=1 \
      --type=directory \
      --search-path=$HOME/Code"

    local jump
    jump=$(eval "$finder_cmd" | fzf) && cd "$jump"
}

