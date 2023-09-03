#!/usr/bin/env sh

defaults write "Apple Global Domain" KeyRepeat -int 1 # default lowest is 2
defaults write "Apple Global Domain" InitialKeyRepeat -int 10 # default lowest is 15
