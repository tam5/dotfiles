#/ --------------------------------------------------------------
#| Helper functions for writing pretty messages to the console.
#/ --------------------------------------------------------------

# Colors
end="\033[0m"
black="\033[0;30m"
blackb="\033[1;30m"
white="\033[0;37m"
whiteb="\033[1;37m"
red="\033[0;31m"
redb="\033[1;31m"
green="\033[0;32m"
greenb="\033[1;32m"
yellow="\033[0;33m"
yellowb="\033[1;33m"
blue="\033[0;34m"
blueb="\033[1;34m"
purple="\033[0;35m"
purpleb="\033[1;35m"
lightblue="\033[0;36m"
lightblueb="\033[1;36m"

bg_red="\033[0;101m"

pad_length=2

print_error() {
    padding="$(printf ' %.0s' {1..$pad_length})"
    message="${padding}${1}${padding}"
    strlen=${#message}
    framer="$(printf ' %.0s' {1..$strlen})"

    color="${bg_red}"

    echo
    echo -e "${color}${framer}${end}"
    echo -e "${color}${message}${end}"
    echo -e "${color}${framer}${end}"
    echo
}
