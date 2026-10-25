# shellcheck disable=SC2086,SC2089,SC2090

########     dependencies:
########        01-bash-debugging.bash
########        013-homebrew.bash
########        015-terminal.bash
########        04-pager.bash

# Use the best available grep-like tool, and make them all behave the same.
# 
# In order of preference:
#
# - rg
# - recent GNU grep installed via Homebrew (on macOS)
# - old, pre-GPLv3 GNU grep bundled with macOS

COMMON_ARG_CHECK_COMMAND="check-args 1 varargs '[MY_GREP_ARG ...] REGEX' \"$@\" || return 1"

if installed "rg"; then
    export MY_GREP="rg"
    function my-grep {
        # check-args 1 varargs "[MY_GREP_ARG ...] REGEX" "$@" || return 1
        $COMMON_ARG_CHECK_COMMAND
        rg \
            --color=always --follow \
            --no-heading --follow --no-one-file-system \
            "$@" \
          | $PAGER
    }
else
    # rg not installed? Use the actual grep command.
   if on-macos && installed "ggrep"; then
        export MY_GREP="ggrep"
    else
        export MY_GREP="grep"
    fi

    # Match rg's default color scheme.
    export GREP_COLORS="fn=${MAGENTA}ln=${GREEN}"

    function my-grep {
        # check-args 1 varargs "[${MY_GREP}_ARG ...] REGEX" "$@" || return 1
        $COMMON_ARG_CHECK_COMMAND

        if [[ $# -eq 1 ]]; then
            set -- "."
        fi
        
        "$MY_GREP" \
            -EIi --color=always \
            -R \
            --exclude-dir=.git \
            --exclude-dir=.github \
            --exclude-dir=cdk.out \
            --exclude-dir=node_modules \
            --exclude=.npmignore \
            --exclude=package-lock.json \
            --exclude-dir=__snapshots__ \
            --exclude '*.js' \
            "$@" \
          | $PAGER
    }
fi

if [[ "$MY_GREP" = "rg" ]]; then
    function highlight {
        local MANNAME="highlight - print everything, using color to highlighting matches"
        $COMMON_ARG_CHECK_COMMAND
        my-grep --passthru "$@"
    }
fi

function gbr {
    local MANNAME="gbr - grep Bash dotfiles"
    # check-args 1 varargs "[${MY_GREP}_ARG ...] REGEX" "$@" || return 1
    $COMMON_ARG_CHECK_COMMAND

    my-grep \
        "$@" \
        ~/.bashr* \
        ~/.*profile* \
        ~/.site.bash*
}

function ge {
    local MANNAME="ge - grep Emacs dotfiles"
    # check-args 1 varargs "[${MY_GREP}_ARG ...] REGEX" "$@" || return 1
    $COMMON_ARG_CHECK_COMMAND
    my-grep "$@" ~/.emacs.d
}

function occur {
    local MANNAME="occur - print filenames with at least one match"
    # check-args 1 varargs "[${MY_GREP}_ARG ...] REGEX" "$@" || return 1
    $COMMON_ARG_CHECK_COMMAND
    my-grep --files-with-matches --max-count=1 "$@" | $PAGER
}

function dirs-matching {
    local MANNAME="dirs-matching - print directories with descendants that contain REGEX"
    check-args 1 varargs 'REGEX [DIR ...]' "$@" || return 1

    local REGEX="$1"
    shift
    
    for DIR in "$@"; do
        if my_grep --max-count=1 "$REGEX" "$DIR" >/dev/null 2>&1; then
            echo "$DIR"
        fi
    done
}
