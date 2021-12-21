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
 --color=fg:regular:0,bg:-1,hl:bold:04
 --color=fg+:regular:015,bg+:-1,hl+:bold:04
 --color=info:0,prompt:0,pointer:05,input:015
 --color=marker:#b46ead,spinner:#303d4a,header:#87afaf'
