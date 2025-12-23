#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Make me admin
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🛠️
# @raycast.description Elevate current user to admin group on this machine

currentUser="$(stat -f%Su /dev/console)"

/usr/bin/osascript <<EOF
do shell script "/usr/sbin/dseditgroup -o edit -a $currentUser -t user admin" with administrator privileges
EOF

