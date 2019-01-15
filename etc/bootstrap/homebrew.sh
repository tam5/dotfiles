#!/usr/bin/env bash

# Abort on error
set -e

# Imports
. ./chalk.sh

echo "Checking if Homebrew is already installed..."; 

# Checks if Homebrew is installed
# Credit: https://gist.github.com/codeinthehole/26b37efa67041e1307db
if test ! $(which brew); then
  echo "Installing Homebrew...";
  yes | /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" &> /dev/null
  print_success "Homebrew succesfully installed."
else
  print_success "Homebrew is already installed.";
fi

echo; echo "Ensuring Homebrew is up to date...";
yes | brew update

outdated=$(brew outdated)

if [ $($outdated | wc -l) -gt 0 ]; then
    echo 'stuff to update';
else
    echo 'nothing to update';
fi

#yes | brew upgrade

print_success "Homebrew succesfully updated."; echo;
