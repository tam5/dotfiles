#!/usr/bin/env bash

HERE=$HOME/.dotfiles/extra/terminal

# /
# |-------------------------------------------------------------------------------
# | Source Our Modules
# |-------------------------------------------------------------------------------
# | 
# | Bla bla bla, helpful message...
# | 
# /

for f in $HERE/src/shared/*; do source $f; done
for f in $HERE/src/modules/*; do source $f; done