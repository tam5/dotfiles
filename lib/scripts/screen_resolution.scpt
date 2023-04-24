tell application "Finder"
    set screenResolution to bounds of window of desktop
    set screenWidth to item 3 of screenResolution
    set screenHeight to item 4 of screenResolution
    return screenWidth & " x " & screenHeight
end tell

