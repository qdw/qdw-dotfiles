#!/usr/bin/osascript

on run argv
    set myUrl to item 1 of argv
    tell application "Music" to open location myUrl
end run
