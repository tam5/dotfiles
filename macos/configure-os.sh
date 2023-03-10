#!/usr/bin/env bash
#
# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                                                                            ║
# ║                          Configure OS Preferences                          ║
# ║                                                                            ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# Close any open System Preferences panes
osascript -e 'tell application "System Preferences" to quit'

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until script has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

sudo scutil --set ComputerName "${COMPUTER_NAME}"
sudo scutil --set HostName "${COMPUTER_NAME}"

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                   Sound                                   │
# └────────────────────────────────────────────────────────────────────────────┘
 
# Disable "Play sound on startup"*
sudo nvram SystemAudioVolume=" "

# Enable "Play feedback when volume is changed"
defaults write -globalDomain "com.apple.sound.beep.feedback" -int 0

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                  General                                  │
# └────────────────────────────────────────────────────────────────────────────┘

# Enable "Automatically check for updates"
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true

# Check for software updates daily, not just once per week*
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Enable "Download new updates when available"
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1

# Enable "Responses and system files"
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1

# Enable "Install application updates from the App Store"
defaults write com.apple.commerce AutoUpdate -bool true

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                            󰹆 Privacy & Security                            │
# └────────────────────────────────────────────────────────────────────────────┘

# Disable the “Are you sure you want to open this application?” dialog*
defaults write com.apple.LaunchServices LSQuarantine -bool false

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                               Energy Saving                               │
# └────────────────────────────────────────────────────────────────────────────┘

# Sleep the display after 15 minutes*
sudo pmset -a displaysleep 15

# Disable machine sleep while charging*
sudo pmset -c sleep 0

# Set machine sleep to 15 minutes on battery*
sudo pmset -b sleep 15

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                  Keyboard                                 │
# └────────────────────────────────────────────────────────────────────────────┘

# Set a blazingly fast keyboard repeat rate
defaults write "Apple Global Domain" KeyRepeat -int 1
defaults write "Apple Global Domain" InitialKeyRepeat -int 10

# Enable "Keyboard navigation" (e.g. enable Tab in modal dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                 󰟸 Trackpad                                 │
# └────────────────────────────────────────────────────────────────────────────┘

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                           󰐪 Printers & Scanners                            │
# └────────────────────────────────────────────────────────────────────────────┘

# Expand the print dialog by default*
defaults write com.google.Chrome PMPrintingExpandedStateForPrint2 -bool true
defaults write com.google.Chrome.canary PMPrintingExpandedStateForPrint2 -bool true

# Automatically quit printer app once the print jobs complete*
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                  󰀶 Finder                                  │
# └────────────────────────────────────────────────────────────────────────────┘

# Hide Recent Tags from sidebar
defaults write com.apple.finder ShowRecentTags -bool false

# Expand save panel by default*
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Save to disk (not to iCloud) by default
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Set Home as the default location for new Finder windows
# For other paths, use `PfLo` and `file:///full/path/here/`
defaults write com.apple.finder NewWindowTarget -string "PfLo"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}"

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Keep folders on top when sorting by name
defaults write com.apple.finder _FXSortFoldersFirst -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Avoid creating .DS_Store files on network or USB volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Show the ~/Library folder
chflags nohidden ~/Library

# Show the /Volumes folder
sudo chflags nohidden /Volumes

# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true
