#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | LiveIntent
# |-------------------------------------------------------------------------------
# | 
# | 
# | 
# /

tunnel() {
    ssh -i ~/.ssh/li.id_rsa -L $2:$1-rds.liveintenteng.com:3306 $1-bastion
}

devstack_thing() {
    app=$1
    dir=$(pwd)
    shift
    cd "$HOME/code/liveintent/devstack"
    set -o allexport; source .env; set +o allexport

    echo "> docker-compose -f ~/code/liveintent/devstack/services/$app/docker-compose.devstack.yml $@"
    docker-compose -f "$HOME/code/liveintent/devstack/services/$app/docker-compose.devstack.yml" "$@"
    cd "$dir"
}

j() {
    projects_dir="$HOME/code/liveintent/li-stack/services"

    local project
    project=$(/bin/ls "$projects_dir" | fzf) && cd "$projects_dir/$project"
}

alias li="devstack_thing"
