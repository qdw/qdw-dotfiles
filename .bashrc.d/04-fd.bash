# shellcheck disable=SC2086

########     dependencies:
########         00-bash-metaprogramming.bash
########         pager.bash

############ Customizations for fd itself

function fdd {
    command fd \
        --follow \
        --full-path \
        --hidden \
        "$@"
}

function fd {
    fdd --color=always "$@" | $PAGER
}

############ fd-plus-grep combos

# Same as fdgrep, but search TypeScript files only
function tsgrep {
    fdgrep "[.]ts$" "$@"
}

function gfd {
    check-args 1 1 'REGEX' "$@" || return 1
    local GFD_REGEX="$1"
    (fd "$GFD_REGEX"; my-grep "$GFD_REGEX") | $PAGER
}

function fdgrep {
    check-args 2 varargs 'FD_REGEX [GREP_ARGUMENTS ...]' "$@" || return 1
    local FILENAME_REGEX="$1"; shift
    fdd --type f "$FILENAME_REGEX" -x my-grep "$@"
}

function fdoccur {
    check-args 2 varargs 'FILENAME_REGEX [GREP_ARGUMENTS ...]' "$@" || return 1
    local FILENAME_REGEX="$1"; shift
    fdgrep "$FILENAME_REGEX" --files-with-matches --max-count 1 "$@"
}

function _file_type_helper {
    fdd -t f "$@" \
        | sed 's/.*\([.].*\)/\1/' `extract basename` \
        | my-grep -v '[ ][.]git/' `exclude .git dirs`
}

function ftypes {
    check-args 0 varargs '[FD_ARGUMENTS ...]' "$@" || return 1
    _file_type_helper "$@" | sort -u
}

function file-type-counts {
    check-args 0 varargs '[FD_ARGUMENTS ...]' "$@" || return 1
    _file_type_helper "$@" \
        | uniq --count \
        | sort --reverse --general-numeric-sort
}
