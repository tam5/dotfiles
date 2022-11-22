#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | LIStack
# |-------------------------------------------------------------------------------
# | 
# | 
# | 
# /

alias lstack="cd ~/code/liveintent/li-stack"
alias lssh="lstack && colima ssh --profile=listack"
alias lps="lssh nerdctl ps"

unalias ll
ll() {
    local container

    container=$(lps -- --format '{{.Names}}' | fzf) && lssh sudo nerdctl logs $container -- -f
}

le() {
    local container

    container=$(lps -- --format '{{.Names}}' | fzf) && \
        lssh sudo nerdctl exec -- -it $container bash
}

lu() {
    local file

    file=$(lstack && /bin/ls | grep docker-compose | fzf) && \
        lssh nerdctl compose -- -f $file up -d
}
# devstack_thing() {
#     app=$1
#     dir=$(pwd)
#     shift
#     cd "$HOME/code/liveintent/li-stack"
#     set -o allexport; source .env; set +o allexport

#     echo "> docker-compose -f ~/code/liveintent/devstack/services/$app/docker-compose.devstack.yml $@"
#     docker-compose -f "$HOME/code/liveintent/devstack/services/$app/docker-compose.devstack.yml" "$@"
#     cd "$dir"
# }

