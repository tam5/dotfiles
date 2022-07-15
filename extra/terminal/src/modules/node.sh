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
# Fuzzy run a package.json script with npm
#
nr() {
  local cmd

  cmd=$(cat package.json | jq -r '.scripts | keys[]' | fzf) &&
      npm run $cmd
}

##
# Fuzzy run a package.json script with yarn
#
yr() {
  local cmd

  cmd=$(cat package.json | jq -r '.scripts | keys[]' | fzf) &&
      yarn run $cmd
}
