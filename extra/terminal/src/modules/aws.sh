#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | AWS
# |-------------------------------------------------------------------------------
# | 
# | Helpers for interacting with the aws cli.
# | 
# /

##
# Fuzzy switch aws profile
#
au() {
  local profile

  profile=$(aws configure list-profiles | fzf) &&
      export AWS_PROFILE=$profile
}