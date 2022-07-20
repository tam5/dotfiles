if [ -f $HOME/.env ]; then
    set -o allexport; source $HOME/.env; set +o allexport
fi

export AWS_PAGER=""
export EDITOR=vim

# /
# |-------------------------------------------------------------------------------
# | Powerlevel 10k
# |-------------------------------------------------------------------------------
# | 
# | Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# | Initialization code that may require console input (password prompts, [y/n]
# | confirmations, etc.) must go above this block; everything else may go below.
# | 
# /
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# /
# |-------------------------------------------------------------------------------
# | Antigen
# |-------------------------------------------------------------------------------
# | 
# | Antigen is a small set of functions that help you easily manage your shell
# | plugins, called bundles. The concept is pretty much the same as bundles in
# | a typical vim+pathogen setup. Antigen is to zsh, what Vundle is to vim.
# | 
# /
source $(brew --prefix)/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle command-not-found
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle b4b4r07/enhancd

antigen theme romkatv/powerlevel10k

antigen apply

# /
# |-------------------------------------------------------------------------------
# | Load Custom Config
# |-------------------------------------------------------------------------------
# | 
# | Load the entrypoint into the rest of our custom config.
# | 
# /

[[ ! -f ~/.dotfiles/extra/terminal/terminal.sh ]] || source ~/.dotfiles/extra/terminal/terminal.sh 
