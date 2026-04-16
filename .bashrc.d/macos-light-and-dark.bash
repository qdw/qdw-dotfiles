LIGHT_OR_DARK_FILE="$HOME/tmp/light-or-dark-state"
OBLIGATE_TOGGLE_FUNCTIONS="toggle-dark-reader"
LIGHT_WALLPAPER="$HOME/wallpaper/light/favorites/castle.jpg"
DARK_WALLPAPER="$HOME/wallpaper/dark/favorites/anemone.jpg"
MODE_SPEC="'dark' | 'light'"

function validate-mode {
    check-args 1 1 "$MODE_SPEC # anything else returns false" "$@" \
        || return 1
    local MODE="$1"
    case "$MODE" in
        light|dark)
            return 0;;
        *)
            _stderr "Invalid  mode '$MODE'. You must pass 'light' or 'dark'."
            return 1;;
    esac
}

function _set_macos_dark_mode {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local BOOL="$1"
    /usr/bin/osascript -e "tell app \"System Events\" to tell appearance preferences to set dark mode to $BOOL"
}

function change-macos-to {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    validate-mode "$MODE" || return 1
    case "$MODE" in
        light) _set_macos_dark_mode "false";;
        dark) _set_macos_dark_mode "true";;
    esac
}

function _set_wallpaper {
    local IMAGE_FILENAME="$1"

    # FIXME: make this work for all desktops, somehow
    /opt/homebrew/bin/wallpaper set "$IMAGE_FILENAME"
}

function change-wallpaper-to {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    validate-mode "$MODE" || return 1
    case "$MODE" in
        light) _set_wallpaper "$LIGHT_WALLPAPER";;
        dark) _set_wallpaper "$DARK_WALLPAPER";;
    esac
}

function change-emacs-to {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    validate-mode "$MODE" || return 1
    local LISP_FUNCTION_NAME
    case "$MODE" in
        light|dark) LISP_FUNCTION_NAME="$MODE"
                    emacsclient --eval "($LISP_FUNCTION_NAME)" >/dev/null;;
    esac
}

function firefox-is-running {
    ps -xwu "$USER" \
        | command grep -v grep \
        | command grep 'Firefox[.]app' > /dev/null
}

function toggle-dark-reader {
    if ! firefox-is-running; then
        return
    fi
    
    local OSA="/usr/bin/osascript"
    local ORIG_APP="$("$OSA" -e \
        'tell application "System Events" to get name of first process whose frontmost is true')"

    "$OSA" -e \
        'tell application "Firefox" to activate' -e \
        'tell application "System Events" to keystroke "x" using {option down, command down}'

    "$OSA" -e \
        "tell application \"$ORIG_APP\" to activate"
}

function mode {
    check-args 0 1 '[NEW_MODE]' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        local MODE=$(cat "$LIGHT_OR_DARK_FILE")
        if [[ "$MODE" = "light" ]] || [[ "$MODE" = "dark" ]]; then
            echo "$MODE"
            return 0
        else
            err 
            return 2
        fi
    elif [[ $# -eq 1 ]]; then
        echo "$1" > "$LIGHT_OR_DARK_FILE"
    fi
}

function currently-dark {
    [[ "$(mode)" = "dark" ]]
}

function currently-light {
    [[ "$(mode)" = "light" ]]
}

function toggle-toggles {
    for FN in $OBLIGATE_TOGGLE_FUNCTIONS; do
        $FN
    done
}

function light {
    if currently-dark; then
        change-macos-to "light"
        change-wallpaper-to "light"
        toggle-toggles
        echo "light" > "$LIGHT_OR_DARK_FILE"
    fi
}

function dark {
    if currently-light; then
        change-macos-to "dark"
        change-wallpaper-to "dark"
        toggle-toggles
        echo "dark" > "$LIGHT_OR_DARK_FILE"
    fi
}
