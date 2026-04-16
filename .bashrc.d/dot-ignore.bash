########     dependencies:
########         00-bash-metaprogramming.bash

# Show all ignore patterns in effect for the cwd.
# Programs that respect ignorefiles include git, fd, and rg

function ignorefiles-helper {
    # Run command on any ignorefiles in our home dir

    check-args 1 varargs 'ignorefiles-helper "COMMAND TO RUN ON EACH IGNOREFILE"' "$@"
    for FILE in "$HOME/.gitignore" "$HOME/.ignore"; do
        if [[ -f "$FILE" ]]; then
            eval "$@" "$FILE"
        fi
    done
    
    # Walk up the pwd running the command on any ignorefiles
    # in any enclosing dirs (except our home dir, which we already did)
    
    local DIR="."
    while [[ "$(realpath "$DIR")" != "/" ]]; do
        for FILE in "$DIR/.gitignore" "$DIR/.ignore"; do
            if [[ -f "$FILE" ]]; then
                if [[ "$(realpath "$FILE")" != "$HOME/$FILE" ]]; then
                    eval "$@" "$FILE"
                fi
            fi
        done
        
        DIR="$DIR/.."
    done
}

function ignorefiles-show-all-patterns {
    check-args 0 0 'ignore-patterns-show-all' "$@"
    ignorefiles-helper "cat" \
        | command grep -vE '^[ ]*[#]' \
        | command grep -vE '^[ ]*$' \
        | sort -u \
        | $PAGER
}

function ignorefiles-list {
    check-args 0 0 'ignorefiles' "$@"
    ignorefiles-helper "echo" | sort | $PAGER
}

function ignorefiles-which-files-contain-pattern {
    check-args 1 varargs 'ignorefiles-which-files-contain-pattern GREP_ARGUMENTS' "$@"
    local REGEX="$1"
    ignorefiles-helper command grep --color=auto -H -E "$@"
}

function ignorefiles-show-all-patterns-by-file {
    check-args 0 0 'ignorefiles-show-patterns-by-file' "$@"
    ignorefiles-which-files-contain-pattern "."
}
