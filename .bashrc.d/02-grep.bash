#### grep.bash
#### Dependencies:
#### ./metaprogramming.bash
#### ./pager.bash

# Use the best available grep-like tool, and make them all behave the same.
# 
# In order of preference:
#
# - rg
# - recent GNU grep installed via Homebrew (on macOS)
# - old, pre-GPLv3 GNU grep bundled with macOS


set -a

if installed "rg"; then
    # Use ripgrep if installed.
    MY_GREP="rg"
    MY_GREP_OPTIONS="--color=always" # --column" --no-line-number"
    GREP_RECURSE="--no-heading --follow --no-one-file-system --no-line-number"
    GREP_EXCLUDE="" # rg uses a .ignore file, not command-line flags
    GREP_COLOR_FLAGS="--colors path:fg:blue --colors line:fg:yellow --colors column:fg:cyan --colors match:fg:red"
else
    # Use the actual grep command.
    if on-macos && installed "ggrep"; then
        MY_GREP="ggrep"
    else
        MY_GREP="command grep"
    fi
    
    MY_GREP_OPTIONS="-EIin --color=always"
    GREP_RECURSE="-R"
    GREP_EXCLUDE="--exclude-dir=.git --exclude-dir=.github --exclude-dir=cdk.out --exclude-dir=node_modules --exclude=.npmignore --exclude=package-lock.json --exclude-dir=__snapshots__ --exclude '*.js'"
    local GREEN="32"
    local YELLOW="33"
    local FILENAME_COLOR="fn=${GREEN}"
    local LINE_NUMBER_COLOR="ln=${YELLOW}"
    GREP_COLOR_FLAGS=""
    GREP_COLORS="${FILENAME_COLOR}:${LINE_NUMBER_COLOR}"
fi

set +a

function grep {
    set -o pipefail
    
    # When using rg as my grep implementation, I often type -R out of habit,
    # but rg has no -R flag (and it's recursive by default).
    # So, if I try to pass that flag to rg, just remove it.
    local ARGS=()
    if [[ "$MY_GREP" = "rg" ]]; then
        for ARG in "$@"; do
            # FIXME: rewrite this properly, using Bash's builtin getopt
            if [[ "$ARG" != "-R" ]]; then
                ARGS+=("$ARG")
            fi
        done
    else
        ARGS=($@)
    fi

    $MY_GREP \
        $MY_GREP_OPTIONS \
        $GREP_COLOR_FLAGS \
        $GREP_EXCLUDE_FLAGS "${ARGS[@]}" | $PAGER
        
    set +o pipefail
}

function gi {
    grep -i "$@"
}

function grep-terms {
    check-args 2 varargs 'FILENAME PATTERN [PATTERN ...]' "$@" || return 1
    local FILENAMES="$1"; shift
    local REGEX=""

    for TERM in "$@"; do
        REGEX="${REGEX}(${TERM})|"
    done
    
    # Remove trailing '|'
    REGEX="${REGEX%|}"
    
    # echo "$REGEX" >&2 # for debugging
    grep "$REGEX" $FILENAMES
}

function g {
    check-args 1 varargs 'REGEX [PATH1 [PATH2 [...]]]' "$@" || return 1
 
    local REGEX="$1"
    shift
    if [[ $# -eq 0 ]]; then
        grep -i $GREP_RECURSE "$REGEX" "."
    else
        grep -i $GREP_RECURSE "$REGEX" "$@"
    fi
}

function gbr {
    check-args 1 varargs 'gbr [ADDITIONAL_GREP_FLAGS] REGEX' "$@" || return 1
    g \
        "$@" \
        ~/.bashr* \
        ~/.*profile* \
        ~/.site.bash*
}

function ge {
    g "$@" ~/.emacs.d/init.el
}

#### grep-settings: grep for section names,
#### which I mark I use 4 leading comment chars, as in this line.
#### Infer the comment char from the file extension.

function grep-settings {
    # check-args 2 varargs
    local GREP_SETTINGS_REGEX="$1"
    shift
    
    function file-ext-in {
        local FILE
        
        for EXT in "$@"; do
            if [[ "$FILE" =~ "[.]${EXT}$" ]]; then
                return 0
            fi
        done

        return 1
    }
    
    local COMMENT_PREFIX
    for FILE in "$@"; do
        if file-ext-in 'el' 'l' 's'; then
            COMMENT_PREFIX=';;;; '
        elif file-ext-in 'ts' 'js' 'c' 'cpp' 'cxx' 'c++'; then
            COMMENT_PREFIX='//// '
        elif file-ext-in 'bash' 'py' 'sh' 'pl'; then
            COMMENT_PREFIX="#### "
        else
            complain "Unrecognized file extension for file '$FILE'"
            return
        fi
    done

    grep -i \
        "^${COMMENT_PREFIX}.*(${GREP_SETTINGS_REGEX}.*settings|settings.*${GREP_SETTINGS_REGEX})"
}

# count: recursively grep the specified dirs (or '.', if no dirs specified)
# for a pattern, then print the number of times that pattern occurs
# in each matching file. Don't show files with zero matches.
function count { 
    command grep -Rc $GREP_EXCLUDE_FLAGS "$@" \
        | command grep -v ':0$' \
        | sed -e 's/\(.*\):\(.*\)/\2 \1/' \
        | sort --reverse --general-numeric-sort | grep '.* ' | $PAGER
}

function count_one_level {
    local COUNT  PATTERN

    PATTERN="$1"
    shift

    (for FILE in $@; do
         COUNT=$(command grep -REio -E $GREP_EXCLUDE_FLAGS --exclude "cdk.out" --exclude "*.js" --exclude "*.json"  "$FILE" | wc -l)
         echo "$COUNT $FILE"
    done) \
    | command grep -vE "^0 " \
    | sort --reverse --general-numeric-sort
}

# occur: recursively grep the specified dirs (or '.', if no dirs specified) and print all filenames with at least one match.
function occur {
    $MY_GREP $GREP_RECURSE --files-with-matches --max-count=1 $GREP_EXCLUDE_FLAGS "$@" | $PAGER
}

# dirs-matching REGEX [DIR ...]
# Take a list of dirs,
# and print those whose recursive contents
# match REGEX (case-insensitive).
function dirs-matching {
    check-args 1 varargs 'dirs-matching REGEX [DIR ...]' "$@"

    local REGEX="$1"
    shift
    
    for DIR in "$@"; do
        if rg -i "$REGEX" "$DIR" >/dev/null 2>&1; then
            echo "$DIR"
        fi
    done
}
