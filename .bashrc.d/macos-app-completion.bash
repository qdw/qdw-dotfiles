# Shamelessly generated with Claude
#
# NAME
#     la - launch macOS apps by substring
# 
# USAGE
#     [source this file, perhaps in ~/.bashrc]
#     la SUBSTR <tab>
#
# DESCRIPTION
#     la is a tab-completion utility for launching macOS .apps from
#     the command line. Type a 'la SOME_SUBSTRING' and hit tab,
#     and it will show you all the Mac apps that match that substring
#     (case-insensitively).
# AUTHORS
#     Claude Sonnet 4.6 Adaptive, via Claude.app
#     me

function _complete_macos_apps {
    local CUR="${COMP_WORDS[COMP_CWORD]}"
    local LOWER_CUR="${CUR,,}"
    local -a MATCHES=()
    local DIRS=(
        "/Applications"
        "/Applications/Utilities"
        "/System/Applications"
    )

    for DIR in "${DIRS[@]}"; do
        [[ -d "$DIR" ]] || continue
        while IFS="" read -r -d "" APP; do
            local NAME="${APP##*/}"
            local NAME_SANS_EXT="${NAME%.app}"
            if [[ "${NAME_SANS_EXT,,}" == *"$LOWER_CUR"* ]]; then
                MATCHES+=("$NAME_SANS_EXT")
            fi
        done < <(find "$DIR" -maxdepth 1 -name "*.app" -print0 2>/dev/null)
    done

    if [[ ${#MATCHES[@]} -gt 0 ]]; then
        # Backslash-escape spaces in completions.
        compopt -o filenames

        COMPREPLY=("${MATCHES[@]}")
    fi
}

complete -F _complete_macos_apps la
