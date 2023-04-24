tell application "Finder"
    set screenResolution to bounds of window of desktop
    set screenWidth to item 3 of screenResolution
    set screenHeight to item 4 of screenResolution
end tell

set newWidth to screenWidth - 16
set newHeight to screenHeight - 16

tell application "System Events"
    repeat with theApp in (every process whose background only is false)
        tell theApp
            repeat with theWindow in (every window)
                set newX to 8
                set newY to 8
                
                set position of theWindow to {newX, newY}
                set size of theWindow to {newWidth, newHeight}
            end repeat
        end tell
    end repeat
end tell

