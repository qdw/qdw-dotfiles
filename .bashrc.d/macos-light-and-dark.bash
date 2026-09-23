########     dependencies:
########         00-bash-metaprogramming.bash

DARK_WALLPAPER="$HOME/wallpaper/dark/current-favorite"
LIGHT_WALLPAPER="$HOME/wallpaper/light/current-favorite"
export LIGHT_OR_DARK_FILE="$HOME/tmp/light-or-dark-state"
MODE_SPEC="'dark' | 'light'"
OBLIGATE_TOGGLE_FUNCTIONS="toggle-dark-reader"

declare -a BROWSERS_USING_DARK_READER=("Firefox.app" "Waterfox.app" "Safari.app")

# BLEN="${#BROWSERS_USING_DARK_READER[@]}"
# for (( i = 0; i < BLEN; i++ )); do
#     echo "${BROWSERS_USING_DARK_READER[$i]}"
# done

function validate-mode {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    case "$MODE" in
        light|dark)
            return 0;;
        *)
            err "invalid  mode '$MODE'. You must pass 'light' or 'dark'."
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
    check-args 1 1 "$@" \
        'do not run directly; run change-wallpaper-to instead' || return 1
    local IMAGE_FILENAME="$1"
    # echo "Image filename is '$IMAGE_FILENAME'" >&2
    # echo "real path is" "$(realpath $IMAGE_FILENAME)" >&2
    

    # FIXME: make this work for all desktops, somehow
    /opt/homebrew/bin/wallpaper set "$IMAGE_FILENAME"
}

function change-wallpaper-to {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    validate-mode "$MODE" || return 1
    # echo "Change wallpaper to '$MODE'" >&2
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
        light|dark)
            LISP_FUNCTION_NAME="$MODE";
            emacsclient --eval "($LISP_FUNCTION_NAME)" >/dev/null;;
        *)
            err "Invalid  mode '$MODE'. You must pass 'light' or 'dark'.";
            return 1;;
    esac
}

function _running {
    check-args 1 1 'MyApp.app' "$@" || return 1
    local APP="$1"
    
    ps auxwww \
        | command grep -F "$USER" \
        | command grep -F "$APP" \
        | command grep -vF "grep"  \
        > /dev/null        
}

function toggle-dark-reader {
    # shellcheck disable=SC2034
    local MANNAME="use AppleScript to toggle the Dark Reader browser ext on/off
    (obsolete for recent versions of Dark Reader, which include a feature
    to auto-follow the system-wide setting)"

    check-args 0 0 '[no arguments]' "$@" || return 1
    
    for BROWSER in "${BROWSERS_USING_DARK_READER[@]}"; do
        if _running "$BROWSER"; then
            local OSA="/usr/bin/osascript"
            local ORIG_APP="$("$OSA" -e 'tell application "System Events" to get name of first process whose frontmost is true')"
            "$OSA" -e 'tell application "Waterfox" to activate' \
                   -e 'tell application "System Events" to keystroke "x" using {option down, command down}'
            "$OSA" -e "tell application \"$ORIG_APP\" to activate"
        fi
    done
}

function current-mode {
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

function toggle-toggles {
    for FN in $OBLIGATE_TOGGLE_FUNCTIONS; do
        "$FN"
    done
}

function light {
    change-all-to "light"
}

function dark {
    change-all-to "dark"
}

function other-mode {
    check-args 0 0 '[no arguments]' "$@" || return 1
    local CURRENT_MODE OTHER_MODE
    
    CURRENT_MODE="$(current-mode)"
    case "$CURRENT_MODE" in
        dark) OTHER_MODE="light";;
        light) OTHER_MODE="dark";;
        *) err "what even is the opposite of '$CURRENT_MODE'?"; return 2;;
    esac

    echo "$OTHER_MODE"
}

function change-alacritty-to {
    check-args 1 1 "$MODE_SPEC" "$@" || return 1
    local MODE="$1"
    validate-mode "$MODE" || return 1

    if [[ ! "$MODE" = "$(current-mode)" ]] && [[ -f "$ALACRITTY_SOCKET" ]]
    then
        alacritty-change-theme-to "${MODE}-default.toml" # see ./terminal.bash
    fi
}

function change-all-to {
    validate-mode "$@"
    local DESIRED_MODE="$1"
    local CURRENT_MODE="$(current-mode)"

    if [[ "$CURRENT_MODE" = "$DESIRED_MODE" ]]; then
        return 0
    else
        change-alacritty-to "$DESIRED_MODE"
        change-macos-to "$DESIRED_MODE"
        change-wallpaper-to "$DESIRED_MODE"
        change-emacs-to "$DESIRED_MODE"
        toggle-toggles        
        echo "$DESIRED_MODE" > "$LIGHT_OR_DARK_FILE"
    fi                                                 
}
