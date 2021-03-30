# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

### Fix slowness of pastes with zsh-syntax-highlighting.zsh
pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
}

pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish
### Fix slowness of pastes

# /-----------------------------------------------------
# | Antigen plugins
# |-----------------------------------------------------
# /
ANTIGEN_PATH=~/.dotfiles
source $ANTIGEN_PATH/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle command-not-found
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle b4b4r07/enhancd

antigen theme romkatv/powerlevel10k

antigen apply

# /-----------------------------------------------------
# | Paths
# |-----------------------------------------------------
# /
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/code/liveintent/platform:$PATH"

ENHANCD_DISABLE_DOT=1
ENHANCD_DISABLE_HYPHEN=1

# /-----------------------------------------------------
# | Load other configs
# |-----------------------------------------------------
# /
HB_CNF_HANDLER="$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-command-not-found/handler.sh"
if [ -f "$HB_CNF_HANDLER" ]; then
    source "$HB_CNF_HANDLER";
fi

[[ ! -f ~/.aliases ]] || source ~/.aliases
[[ ! -f ~/.fzf ]] || source ~/.fzf
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export AWS_PAGER=""
export EDITOR=vim

# Created by `pipx` on 2021-03-09 02:06:02
export PATH="$PATH:/Users/arimiller/.local/bin"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
