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
  local base_directory=$HOME/Code
    finder_cmd="fd \
      -HI "\\.git$" \
      --max-depth=5 \
      --type=directory \
      --base-directory=${base_directory} \
      | xargs -I {} dirname {}"

    local selection
    selection=$(eval "$finder_cmd" | fzf-tmux -p) && cd "${base_directory}/${selection}"
}

