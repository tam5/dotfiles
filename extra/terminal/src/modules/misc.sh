#!/usr/bin/env bash

# /
# |-------------------------------------------------------------------------------
# | cd
# |-------------------------------------------------------------------------------
# | 
# | Some frequently used locations...
# | 
# /
alias home='cd ~'
alias desk='cd ~/desktop'
alias dotfiles='cd ~/.dotfiles'
alias code='cd ~/code'
alias emd='cd ~/.emacs.d'
alias dmd='cd ~/.doom.d'
alias mods="cd ~/.dotfiles/extra/terminal/src/modules"

# /
# |-------------------------------------------------------------------------------
# | cat
# |-------------------------------------------------------------------------------
# | 
# | ccat is just like cat, but it adds syntax highlighting.
# | https://github.com/owenthereal/ccat
# | 
# /
alias cat="ccat"

# /-------------------------------------------------------------------------------
# | env
# |-------------------------------------------------------------------------------
# | 
# | Make env fuzzy with fzf
# | 
# /
 alias env="/usr/bin/env | fzf"

# /
# |-------------------------------------------------------------------------------
# | ls
# |-------------------------------------------------------------------------------
# | 
# | Exa is a modern replacement for ls.
# | https://github.com/ogham/exa
# | 
# /
source ~/.exa-colors
alias ls="exa -lb --time-style=long-iso --group-directories-first -a"
alias l="ls"
alias la="exa -lbhHigUmuSa@ --time-style=long-iso --git --color-scale"

# /
# |-------------------------------------------------------------------------------
# | rm
# |-------------------------------------------------------------------------------
# | 
# | Because you can't go back from a rm -rf...
# | https://github.com/andreafrancia/trash-cli
# | 
# /
alias rm="echo Dude, stop using rm! Use \'trash\' or, if you insist, \'/bin/rm\'."

# /
# |-------------------------------------------------------------------------------
# | jwt-decode
# |-------------------------------------------------------------------------------
# |
# | Decode a jwt token.
# |
# /
function jwt-decode() {
    jq -R 'split(".") | .[1] | @base64d | fromjson' <<< "$1JWT"
}

# /
# |-------------------------------------------------------------------------------
# | export as pdf
# |-------------------------------------------------------------------------------
# |
# | Reformat an image as a pdf.
# |
# /
function to-pdf() {
    name="${1%.*}"
    sips -s format pdf $1 --out $name.pdf
}

# /
# |-------------------------------------------------------------------------------
# | export as jpg
# |-------------------------------------------------------------------------------
# |
# | Reformat an image as a jpg.
# |
# /
function to-jpeg() {
    name="${1%.*}"
    sips -s format jpeg $1 --out $name.jpeg
}

# /
# |-------------------------------------------------------------------------------
# | Help
# |-------------------------------------------------------------------------------
# |
# | Help me help you
# |
# /
alias aliases="alias | fzf"
