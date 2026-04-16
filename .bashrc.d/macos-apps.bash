function _function-name-for {
    local APPNAME="$1"

    # Remove exclamation points.
    APPNAME=${APPNAME//!/}
    # Convert spaces to hyphens.
    APPNAME=${APPNAME// /-}

    echo "$APPNAME"
}

function list-apps {
    command ls \
            -d \
            --color=never \
            /Applications/*.app \
        | sed -e 's|/Applications/\(.*\)[.]app|\1|'
}

function create-functions {
    local APP_LIST_FILE="$HOME/.site.bash.d/mac-apps-for-which-to-create-bash-functions"

    while IFS="" read -r APP || [[ -n "$APP" ]]; do
        # if [[ ! -d "/Applications/$APP.app" ]]; then
        #     continue
        # fi
        
        FN="$(_function-name-for "$APP")"
        eval "function $FN { open '/Applications/$APP.app' ;}"
        eval "function $FN.app { open '/Applications/$APP.app' ;}"
    done < "$APP_LIST_FILE"

    # Special case: some macOS builtin apps live in /System/Applications

    for AP in "Clock" "Photos" "Home" "Messages" "Music"; do
        FK="$(_function-name-for "$AP")"
        eval "function $FK { open '/System/Applications/$AP.app' ;}"
        eval "function $FK.app { open '/System/Applications/$AP.app' ;}"
    done

    # Irregular cases

    function Settings { open "open /System/Applications/System Settings.app" ;}
    function SystemSettings { Settings ;}
}

create-functions
