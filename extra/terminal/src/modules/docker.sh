#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | Docker
# |-------------------------------------------------------------------------------
# | 
# | Helpers for interacting with docker containers.
# | 
# /

##
# Follow logs for a running container.
#
dlog() {
  local container

  container=$(docker ps --format '{{.Names}}' | fzf) && docker logs $container -f
}

##
# Exec into a running container.
#
dex() {
  local container

  container=$(docker ps --format '{{.Names}}' | fzf) && docker exec -it $container bash
}
