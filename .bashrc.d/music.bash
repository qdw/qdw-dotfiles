function stems {
    check-args 1 1 'AUDIO_FILE' "$@" || return 1
    local AUDIO_FILE="$1"
    demucs -n htdemucs_6s --two-stems guitar "$AUDIO_FILE"
}

function guitar-tabs { # download guitar tabs from ultimate-guitar.com
    check-args 1 1 'SOME_URL.ultimate-guitar.com' "$@" ||return 1
    local URL="$1"

    # 1. Fetch the page (curl).
    # 
    # 2. Extract the tablature payload from the JS-encoded HTML (perl).
    #
    # 3. Convert each DOS CRLF newline, represented in the JS-encoded payload
    #    as the literal four-char string "\r\n", to an actual newline (sed 1).
    # 
    # 4. Remove Ultimate Guitar's idiosyncratic [tab] and [/tab]
    #    HTML-like tags (sed 2).
    #
    # 5. Ditto for the [ch] and [/ch] pseudo-tags (sed 3).
    #
    # 6. Convert the HTML entity reference for an apostrophe
    #     - which is  &#039; - to a literal Unicode ' (sed 4).
    # 
    # Later we will probably need to replace many more HTML entity references,
    # and probably more [/pseudo-tags]. I will address them as I notice
    # them in tablature that I actually read.
    
    curl -s "$URL" \
        | perl -0777 -ne 'print "$1" if $_ =~ m|{&quot;wiki_tab&quot;:{&quot;content&quot;:&quot;(.*?)(?=&quot;,&quot;revision_id)|smx;' \
        | sed -e "s|\\\r\\\n|\n|g" \
        | sed -e 's|\[tab\]||g' -e 's|\[/tab\]||g' \
        | sed -e 's|\[ch\]||g'  -e 's|\[/ch\]||g' \
        | sed -e "s|&#039;|'|g"
    local EXIT_STATUS=$?
    echo
    return $EXIT_STATUS
}

if on-macos; then
    function song-id {
        local MANNAME="get the ID of the Apple Music song currently playing"
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e \
            'tell application "Music" to get persistent ID of current track'
    }

    function song-name {
        local MANNAME="get the name of the Apple Music song currently playing"
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e 'tell application "Music" to get name of current track'
    }

    function album-id {
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e '
tell application "Music"
    set currentTrack to current track
    set trackAlbum to album of currentTrack
    set trackArtist to album artist of currentTrack
    if trackArtist is "" then set trackArtist to artist of currentTrack
    set theAlbum to first album of library playlist 1 whose name is trackAlbum and artist is trackArtist
    get database ID of theAlbum
end tell
'
    }

    function play {
        check-args 1 1 'BEGINNING_OF_SONG_NAME' "$@" || return 1
        local PREF="$1"
        
        /usr/bin/osascript -e "
tell application \"Music\"
    set t to first track of library playlist 1 whose name starts with \"$PREF\"
    play t
end tell"
    }

    function play-album-untested {
        check-args 1 1 'APPLE_MUSIC_ALBUM_ID' "$@" || return 1
        local ALBUM_ID="$1"

        local APPLESCRIPT="
set albumID to $ALBUM_ID
tell application \"Music\"
    set theAlbum to first album whose database ID is albumID
    play theAlbum
end tell
"
        echo "$APPLESCRIPT" | /usr/bin/osascript
    }
fi # end macOS-specific functions

#### Video game music

function zophar {
    local MANNAME="search zophar.net for video game music"
    local URL
    local NES_URL="https://mail.zophar.net/music/nintendo-nes-nsf"
    
    if [[ $# -eq 0 ]]; then
        # default: NES
        URL="$NES_URL"
    else
        local CONSOLE="$1"
        case "$CONSOLE" in
            NES|nes)
                URL="$NES_URL";;
            SNES|snes)
                URL="https://www.zophar.net/music/nintendo-snes-spc";;
            PS2|PlayStation)
                URL="https://www.zophar.net/music/playstation2-psf2";;
            PSP)
                URL="https://www.zophar.net/music/playstation-portable-psp";;
            *)
                err "console '$1' is unsupported"
                return 1;;
        esac
        
        open "$URL"
    fi
}
