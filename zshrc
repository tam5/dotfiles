# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# /-----------------------------------------------------
# | Antigen plugins
# |-----------------------------------------------------
# /
source /usr/local/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle command-not-found
antigen bundle zsh-users/zsh-autosuggestions

antigen theme romkatv/powerlevel10k

antigen apply

# /-----------------------------------------------------
# | Paths
# |-----------------------------------------------------
# /
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/code/liveintent/platform:$PATH"

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
