#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | Node
# |-------------------------------------------------------------------------------
# | 
# | js
# | 
# /

##
# Fuzzy run a package.json script
#
yr() {
  local cmd

  cmd=$(cat package.json | jq -r '.scripts | keys[]' | fzf) &&
      yarn run $cmd
}

