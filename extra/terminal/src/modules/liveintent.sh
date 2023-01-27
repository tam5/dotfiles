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

dpl() {
    kubectl -n $1 describe deployment $1 | grep GIT
}

_login() {
    echo "Logging in to ${1} with username $LI_USERNAME"
    export LI_TOKEN=$(curl -s ${1}/auth/login -H 'Content-type: application/json' -d "{ \"username\": \"$LI_USERNAME\", \"password\": \"$LI_PASSWORD\" }" | jq -r '.token')
    echo $LI_TOKEN | pbcopy;
    echo $LI_TOKEN;
}

login-local() {
    _login localhost:6200
}

login-qa() {
    _login https://qa-merlin.liveintenteng.com
}

login-stage() {
    _login https://stage-merlin.liveintent.com
}

alias li="devstack_thing"
