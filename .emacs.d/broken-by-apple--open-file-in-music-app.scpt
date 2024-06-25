#!/usr/bin/osascript

on run argv
    tell application "Visual Studio Code" to open item 1 of argv
    return "This doesn't work, because the Music.app AppleScript API no longer supports opening music that's not in your Library."
end run
