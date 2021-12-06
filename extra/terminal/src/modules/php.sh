#!/usr/bin/env bash
# /
# |-------------------------------------------------------------------------------
# | PHP
# |-------------------------------------------------------------------------------
# | 
# | PHP doesn't actually suck believe it or not.
# | 
# /

alias phpunit='vendor/bin/phpunit'
alias pu='phpunit'
alias pf="phpunit --filter"
alias tf="composer test-watch -- --filter"
alias tw="composer test-watch"

# laravel stuff
alias art='php artisan'
alias rr='php artisan route:pretty --group=path'

test_in_laravel() {
    grep laravel/framework composer.json > /dev/null 2>&1
}

start_tinker_session() {
    if test_in_laravel; then
        php artisan tinker "$@"
    else
        php ~/code/laravel/artisan tinker "$@"
    fi
}

tinker() {
    if [ -z "$1" ]; then
        start_tinker_session
    else
        start_tinker_session --execute="dd($1);"
    fi
}

composer-link() {
    dir=$1

    if [[ -z $dir ]]; then
        print_error "Please specificy a package name."
        return;
    fi

    if [[ ! -d $dir ]]; then
        print_error "Directory '$(pwd)/$dir' does not exist."
        return;
    fi

    if [[ ! -f $dir/composer.json ]]; then
        print_error "$dir is not an installable PHP package."
        return;
    fi

    jq --indent 4 \
        '.repositories |= [{"type": "path", "url": "'$dir'", "options": {"symlink": true}}] + . ' composer.json \
        > composer.tmp.json \
        && mv composer.tmp.json composer.json

    packageName=$(jq -r '.name' $dir/composer.json)

    composer require $packageName @dev
}

composer-unlink() {
  git checkout composer.json composer.lock
  composer update
}