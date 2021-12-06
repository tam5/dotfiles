#!/usr/bin/env bash

# /-------------------------------------------------------------------------------
# | fzf
# |-------------------------------------------------------------------------------
# | 
# | Make things fuzzy findable with fzf
# | https://github.com/junegunn/fzf
# | 
# /

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='--layout=reverse'

export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
 --color=fg:#f1f1f0,bg:#171c20,hl:#88c0d0
 --color=fg+:#88c0d0,bg+:#292f38,hl+:#88c0d0
 --color=info:#303d4a,prompt:#50fa7b,pointer:#f1f1f0
 --color=marker:#b46ead,spinner:#303d4a,header:#87afaf'
