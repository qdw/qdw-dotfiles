function create-functions {
    local APP_LIST_FILE="$HOME/.site.bash.d/mac-apps-for-which-to-create-bash-functions"

    function function-name-for {
        local FN="$1"

        # Remove exclamation points.
        FN=${FN//!/}
        # Convert spaces to hyphens.
        FN=${FN// /-}

        echo "$FN"
    }

    while IFS="" read -r APP || [[ -n "$APP" ]]; do
        FN="$(function-name-for "$APP")"
        eval "function $FN { open '/Applications/$APP.app' ;}"
        eval "function $FN.app { open '/Applications/$APP.app' ;}"
    done < "$APP_LIST_FILE"

    # Special case: some macOS builtin apps live in /System/Applications
    for AP in "Photos" "Home"; do
        FK="$(function-name-for "$AP")"
        eval "function $FK { open '/System/Applications/$AP.app' ;}"
        eval "function $FK.app { open '/System/Applications/$AP.app' ;}"
    done

    # Special-case nicknames for long app names

    # function NITW {
    #     open "/Applications/Night in the Woods.app"
    # }

    # function Kinesis {
    #     open "/Applications/SmartSet App for Freestyle Edge RGB for macOS (Version 2.1.5) (64 Bit).app"
    # }
}

create-functions
