#!/usr/bin/env bash
#
# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                                                                            ║
# ║                       Configure Desktop Preferences                        ║
# ║                                                                            ║
# ╚════════════════════════════════════════════════════════════════════════════╝

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                 󰨙 Menubar                                  │
# └────────────────────────────────────────────────────────────────────────────┘

# Enable "Display the time with seconds"
defaults write com.apple.menuextra.clock ShowSeconds -int 1

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                                   󱂩 Dock                                   │
# └────────────────────────────────────────────────────────────────────────────┘

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

# Disable "Show recent applications in Dock"
defaults write com.apple.dock show-recents -bool false

# Currently broken, do it manually
#dockutil --no-restart --remove all

#dockutil --no-restart --add  "/Applications/iTerm.app"
#dockutil --no-restart --add  "/Applications/Google Chrome.app"
#dockutil --no-restart --add  "/Applications/Things3.app"
#dockutil --no-restart --add  "/Applications/Cron.app"
#dockutil --no-restart --add  "/System/Applications/Notes.app"
#dockutil --no-restart --add  "/Applications/Notion.app"
#dockutil --no-restart --add  "/Applications/Mimestream.app"
#dockutil --no-restart --add  "/Applications/Slack.app"
#dockutil --no-restart --add  "/System/Applications/Messages.app"
#dockutil --no-restart --add  "/Applications/TablePlus.app"
#dockutil --no-restart --add  "/System/Applications/Music.app"

#killall Dock

# ┌────────────────────────────────────────────────────────────────────────────┐
# │                               󰘇 Hot Corners                                │
# └────────────────────────────────────────────────────────────────────────────┘

# Hot corner - Bottom Left → Start screen saver
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0

# Hot corner - Bottom Right → Start screen saver
defaults write com.apple.dock wvous-br-corner -int 5
defaults write com.apple.dock wvous-br-modifier -int 0
