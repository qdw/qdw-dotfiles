#### Dependencies:
####   ./bash-metaprogramming.bash
####   ./grep.bash

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

function gfd {
    check-args 1 1 'gfd REGEX' "$@" || return 1
	local GFD_REGEX="$1"
	(fd -i "$GFD_REGEX"; g -i "$GFD_REGEX") | $PAGER
}

function fdgrep {
    # check-args 1 varargs "fdgrep FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
    local FILENAME_REGEX="$1"; shift    
    (fdd --type f "$FILENAME_REGEX" -x $MY_GREP -I --color=always $GREP_EXCLUDE_FLAGS "$@" \;)
}

# Recursively search the current working directory for files that don't
# end in a single newline, and print their names
function newlineless {
    fd -t f -x bash -c 'tail -c 1 "$1" | command grep -qvE "^$" && echo "$1"' _ {} \;
}

function fdoccur {
    # check-args 2 varargs "fdoccur FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
    local FILENAME_REGEX="$1"; shift
    fdgrep "$FILENAME_REGEX" -l -m 1 "$@"
}

function _file-types {
    fdd -t f | sed 's/.*\([.].*\)/\1/' | sort \
        | uniq \
        | grep '|(rb|kt|kts|py|jar|java|sql|ts|js|json)|yaml|yml'
}

function file-type-counts {
    fdd -t f | sed 's/.*\([.].*\)/\1/' | sort \
        | uniq -c \
        | sort --reverse --general-numeric-sort | grep '|(rb|kt|kts|py|jar|java|sql|ts|js|json)|yaml|yml'
}
