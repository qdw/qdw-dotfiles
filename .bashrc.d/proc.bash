# shellcheck disable=SC2086

function pstree {
    command pstree -g 3 -u "$USER" "$@" | $PAGER
}

function p {
    check-args 1 varargs '[GREP_FLAGS] REGEX' "$@" || return 1

    # assumes set -o pipefail, which I have enabled globally
    ps auxwwwwww \
        | command grep -vE 'grep|rg' \
        | $MY_GREP $MY_GREP_OPTIONS $GREP_COLOR_FLAGS "$@" \
        | $PAGER
    local EXIT_STATUS=$?

    eval "$ORIG_OPTS"
    return $EXIT_STATUS
}

function ph {
    ps auxwwwwww | head -1
    p "$@"
}

# Display only processes that belong to me.
function pu {
    p "$USER"
}
