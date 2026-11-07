function which-realpath {
    local MANNAME="show the real (non-symlink) path to an executable program"
    check-args 1 1 'PROGRAM_NAME' "$@" || return 1
    local PROG="$1"
    local PTH=$(which "$PROG")
    realpath "$PTH"
}

function dashify-path {
    if ! check-args 0 1 '[PATH] # defaults to current working directory]' "$@"
    then
        return 1
    elif [[ $# -eq 0 ]]; then
        local ABSOLUTE="$(realpath)"
    elif [[ $# -eq 1 ]]; then
        local ABSOLUTE="$(realpath "$1")"
    fi
    
    local DASHIFIED="$(echo "$ABSOLUTE" | sed 's|^/|| ; s|[/]|---|g')"
    echo "$DASHIFIED"
}

function cdd { cd "$HOME/Desktop/$1" ;}
function cdq { cd "$HOME/src/1/qdw-dotfiles/$1" ;}
function cd3 { cd "$HOME/src-3/$1" ;}
function cd1 { cd "$HOME/src/1/$1" ;}
function cds { cd "$HOME/symlinks" ;}

# #FIXME: clean up comments; move to SYNOPSIS -> If the argument is a symlink, resolve it and then cd to the enclosing dir
# of the file that it point to.
#
# If the user passes no arguments, then do that to the directory portion
# of the previous argument passed ($_).
function cdl {
    # shellcheck disable=2016
    local MANNAME='cd to the enclosing dir of a file (or of $_ if none passed)'
    local ARG  REALPATH

    # shellcheck disable=2016
    check-args 0 1 '[TARGET_FILE] # default: $_' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        ARG="$_"
    else
        ARG="$1"
    fi
    
    REALPATH="$(realpath "$ARG")"
    if [[ -d "$REALPATH" ]]; then
        err "makes no sense when called on a directory"
        return 2
    else
        local DIR="$(dirname "$REALPATH")"
        cd "$DIR"
    fi
}

# cdr ("CD Realpath") - cd to the enclosing directory that a symlink points to

function cdr {
    check-args 1 1 'FILE_OR_DIRECTORY' "$@" || return 1
    REALPATH="$(realpath "$1")"

    local DIR
    if [[ -d "$REALPATH" ]]; then
        DIR="$1"
    else
        DIR="$(dirname "$REALPATH")"
    fi
    
    cd "$REALDIR"
}

function mcd {
    local DIRNAME
    DIRNAME="$1"
    mkdir "$DIRNAME" \
        && cd "$DIRNAME"
}

# Tell file(1) to report symlinks as symlinks.

function file { command file --no-dereference "$@" ;}

# path - given a relative path, print its relative path. Does not dereference symlinks. Does not work on absolute paths; hence does not work for tilde expansion.
function path {
    local REL_PATH ABS_PATH
    REL_PATH="$1"
    ABS_PATH="$(pwd)/$REL_PATH"

    if [[ ! -e "$ABS_PATH" ]]; then
        echo "Error: file '$ABS_PATH' does not exist"
    else
        # If path starts with home dir, convert to tilde notation.
        ABS_PATH="${ABS_PATH/#${HOME}\//~/}"
        echo "$ABS_PATH" | tee >(pbcopy)
    fi
}
