#!/bin/bash

# Put our own styles into slack

# allow calling this script from wherever
setup_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$setup_dir"

slack_theme_location=/Applications/Slack.app/Contents/Resources/app.asar.unpacked/src/static/ssb-interop.js

# for atom one TODO add more choices
theme_file=../slack/ssb-interop.js.atom-one-dark

sudo cp $theme_file $slack_theme_location

if [ $? -eq 0 ]; then
    echo Succesfully copied $theme_file into $slack_theme_location
else
    echo Slack theme file was not copied
fi
