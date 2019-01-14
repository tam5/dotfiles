blue="\033[0;34m"
blackb="\033[1;30m"
green="\033[0;32m"
end="\033[0m"

function blue {
  echo -e "${blue}${1}${end}"
}

function green {
  echo -e "${green}${1}${end}"
}

function blackb {
  echo -e "${blackb}${1}${end}"
}

function print_info {
    blue "ℹ ${1}"
}

function print_success {
    green "✔ ${1}"
}
