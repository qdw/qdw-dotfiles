#### Steam

if on-macos; then
    function steam {
        local STEAM_GAMES_DIR="$HOME/Library/Application Support/Steam/steamapps/common"
        check-args 0 1 "[GAME_NAME] # or its first few chars"  "$@" || return 1
        if [[ $# -eq 0 ]]; then
            command ls -1 "$STEAM_GAMES_DIR" \
                | command grep -v "Steam Controller Configs"
        elif [[ $# -eq 1 ]]; then
            local PREF="$1"
            open "$STEAM_GAMES_DIR/${PREF}"*/*.app
        fi
    }
fi

#### Playdate (handheld indie console) game dev settings

function pd_encode {
    local INPUT OUTPUT

    if ! [[ $# -eq 1 ]] || [[ $# -eq 2 ]]; then
        echo "Wrong number of arguments. Usage: pd_encode INPUT_FILENAME [OUTPUT_FILENAME"
    else
        INPUT="$1"
        OUTPUT=${2:-$INPUT.pd.wa}
        set -x
        ffmpeg -i "$INPUT" -acodec adpcm_ima_wav "$OUTPUT"
        set +x
    fi
}

#### SDL2 game dev settings (FIXME: port to SDL 3)

function sd2lcompile {
    gcc -o "$@"  `sdl2-config --cflags --libs`
}

function sdd { sdl2compile "$@" ;}
