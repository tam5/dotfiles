#!/usr/bin/env sh

CONFIG_FILE="$DOTFILES/macos.yaml"

get_app_path() {
  local app_path="/Applications/${1}.app"

  if [ ! -d "${app_path}" ] && [ -d "/System/${app_path}" ]; then
      echo "/System${app_path}"
  else
      echo "${app_path}"
  fi
}

### not sure if this part works right now
defaults write "Apple Global Domain" KeyRepeat -int $(yq e '.keyboard.key_repeat' $CONFIG_FILE) # default lowest is 2
defaults write "Apple Global Domain" InitialKeyRepeat -int $(yq e '.keyboard.key_repeat_delay' $CONFIG_FILE) # default lowest is 15

defaults write com.apple.dock tilesize -int $(yq e '.dock.tile_size' $CONFIG_FILE)
defaults write com.apple.dock largesize -int $(yq e '.dock.tile_size_magnified' $CONFIG_FILE)

defaults write com.apple.dock show-recents -int 0
defaults write com.apple.dock persistent-others -array
defaults write com.apple.dock persistent-apps -array

IFS=$'\n'
for app in $(yq e '.dock.apps[]' $CONFIG_FILE); do
  defaults write com.apple.dock persistent-apps -array-add \
      "<dict>
       <key>tile-data</key>
           <dict>
               <key>file-data</key>
               <dict>
                   <key>_CFURLString</key>
                   <string>$(get_app_path $app)</string>
                   <key>_CFURLStringType</key>
                   <integer>0</integer>
               </dict>
           </dict>
       </dict>"
done

sudo killall Dock
