########     dependencies:
########         QBASHD env var setting in ~/.bashrc
########         optional: USE_GREP_TO_PRINT_BASH_DEPS override in ~/.bashrc

############ Diagnostics

function _stderr {
    echo "$@" >&2
}

function err {
    local CALLER="${FUNCNAME[1]}"
    _stderr "$CALLER: error:" "$@"
}

function carp {
    local CALLER="${FUNCNAME[1]}"
    _stderr "$CALLER:" "$@"
}

function _debug {
    if [[ "$MY_DEBUG_BASH" ]]; then
        local CALLER="${FUNCNAME[1]}"
        if [[ "$CALLER" ]]; then
            _stderr "$CALLER: debug:" "$@"
        else
            _stderr "debug:" "$@"
        fi
    fi
}

function complain { # for multiline msgs (pass each line as a separate arg)
    for STRING in "$@"; do
        _stderr "$STRING" >&2
    done
}

############ Utilty functions

# Check for functions, aliases, and executables.
function command-exists {
    check-args 1 1 'COMMAND_NAME' "$@" || return 1
    local COMMAND="$1"
    command -V "$COMMAND" >/dev/null 2>&1
}

# Check for executables only.
function installed {
    check-args 1 1 'COMMAND_NAME' "$@" || return 1
    local COMMAND="$1"
    type -P "$COMMAND" >/dev/null 2>&1
}

function foreach-line-in {
    check-args 2 varargs 'FILE "COMMAND WITH {} PLACEHOLDER FOR LINE"' "$@" \
        || return 1

    local FILE="$1"
    local COMMAND="$2"

    parallel "$COMMAND" < "$FILE"
}

function check-args {
    # check-args
    # ==========
    # 
    # This function implements ad hoc, runtime argument-checking.
    # It is meant to be called by end-user functions, not by users directly.
    # 
    # It checks whether the end-user function passed the right number of
    # arguments. If not, it prints (to standard error) an informative
    # error message, plus a usage string.
    # 
    # Exception: if the end user passed -h or --help, then check-args prints
    # the usage string without the error message.
    # 
    # check-args returns 0 if the user passed the right number of arguments
    # and 1 if the user passed the wrong number of arguments or passed -h
    # or --help. Note that this differs from the behavior of most Unix
    # utilities, where invoking with --help returns 0.
    #
    # DETAILS
    # =======
    # 
    # check-args checks the *number* of arguments that the user passed;
    # checking their *contents* is left to the caller. The end-user function
    # must call check-args with 3 arguments:
    # 
    # check-args $MIN $MAX "$USAGE" "$@" || return $NONZERO
    # 
    # - $MIN is the minimum number of arguments required (which can be 0).
    # 
    # - $MAX is the maximum number of argumwnts allowed (can be "varargs").
    # 
    # - $USAGE is a brief human-readable explanation provided by the
    #   end-user function. For details, see section RECOMMENDED FORMATTING.
    # 
    # Now if the end-user passes the wrong number of arguments,
    # or if they pass -h or --help, check-args prints a line
    # that shows the correct usage and tells the user whether they
    # passed too many argument or not enough.
    # 
    # EXTENDED FORMAT: MANNAME
    # ========================
    # 
    # Optionally, the end-user function may set the magic local variable
    # MANNAME, a one-line string describing what the end-user command
    # actually does. Passing MANNAME causes check-args to print
    # usage info in a verbose, multi-line format reminiscent of
    # a Unix man page. For details, see section RECOMMENDED_FORMATTING.
    # 
    # RECOMMENDED FORMATTING
    # ======================
    #
    # You can use whatever strings you like for USAGE amd MANNAME,
    # but the intended format is as follows.
    # 
    # Formatting if MANNAME is not set
    # --------------------------------
    # 
    # If the end-user function does not set MANNAME, then check-args
    # prints a compact message that lists the expected arguments
    # but doesn't say what the end-user function actually does.
    # For example:
    # 
    # function my_ln_terse {
    #     check-args 1 2 "SOURCE_FILE [TARGET_FILE]" "$@" || return 1
    #     
    #     # ...
    # }
    # 
    # Now if the end user passes no arguments, they will see the following:
    # 
    # my_ln_terse: error: too few arguments (0 given, 1 required).
    # Usage: my_ln_terse SOURCE_FILE [TARGET_FILE]
    # 
    # If the user passes -h or --help, they will see the same usage message,
    # but without the error line:
    # 
    # Usage: my_ln_terse SOURCE_FILE [TARGET_FILE]
    # 
    # Formatting if MANNAME is set
    # ----------------------------
    # 
    # If the end-user function sets the magic local variable MANNAME,
    # then check-args prints usage info in a verbose, multi-line format
    # that mimics a Unix man page. For example:
    # 
    # function my_ln_verbose {
    #     local MANNAME="create symbolic link to a file"
    #     check-args 1 2 "SOURCE_FILE [TARGET_FILE]" "$@" || return 1
    #     # ...
    # }
    # 
    # Now if the end user passes no arguments, they will see the following:
    # 
    # my_ln_verbose: error: too few arguments (0 given, 1 required).
    # 
    # Usage:
    # 
    # NAME
    #     my_ln - link files
    #      
    # SYNOPSIS
    #     my_ln SOURCE_FILE [TARGET_FILE]
    # 
    # 
    # If the user passes -h or --help, they will see the same output,
    # but without this line:
    # my_ln_verbose: error: too few arguments (0 given, 1 required).

    local CALLER="${FUNCNAME[1]}"
    
    # First check number of args passed to check-args itself!

    if [[ $# -lt 3 ]]; then
        # Wrong number of args to check-args itself.
        # Print an error and return.

        local ARGUMENTS_INFLECTED
        if [[ $# -eq 1 ]]; then
            ARGUMENTS_INFLECTED="argument"
        else
            ARGUMENTS_INFLECTED="arguments"
        fi

        complain "check-args: error: $CALLER passed the wrong number of arguments to check-args itself" \
                 "($CALLER called check-args with $# $ARGUMENTS_INFLECTED, but it requires" \
                 "at least 3.)" \
                 "Usage: check-args  MIN_NUM_ARGS  MAX_NUM_ARGS  USAGE_STRING"
        return 250
    fi

    # Okay, the caller passed the correct number of args to check-args itself.
    # Now check whether the user passed the right number of arguments
    # to the function that called check-args.

    local MIN_ARGS="$1"; shift
    local MAX_ARGS="$1"; shift
    local USAGE="$1"; shift
    
    local SYNOPSIS="$CALLER $USAGE"
    
    # Optionally, the caller may set the magic local variable MANNAME.
    # This should be a one-line string describing the command,
    # in the same format as the NAME section of a Unix man page.
    # If MANNAME is set, then we print the whole usage message
    # in a verbose, man-page-like, multiline format. Otherwise
    # we print a one-liner.

    if [[ "$MANNAME" ]]; then
        USAGE="Usage:

NAME
     $CALLER - $MANNAME
     
SYNOPSIS
     $SYNOPSIS
"
    else
        USAGE="Usage: $SYNOPSIS"
    fi
    
    # Having formatted the usage string, figure out whether we
    # actually need to print it, that is, whether the user passed
    # the right number of arguments or not.

    local ARGC=$#
    if [[ $ARGC -lt $MIN_ARGS ]]; then
        local VARARGS_CLAUSE
        if [[ "$MAX_ARGS" = "varargs" ]]; then
            VARARGS_CLAUSE=" or more"
        else
            VARARGS_CLAUSE=""
        fi

        complain "$CALLER: error: too few arguments ($ARGC given, ${MIN_ARGS}${VARARGS_CLAUSE} required)." "" \
                 "$USAGE"
        
        return 1
    elif [[ "$MAX_ARGS" != "varargs" ]] && ((ARGC > MAX_ARGS)); then
        complain "check-args: error: $CALLER passed the wrong number of arguments to check-args itself" \
                 "($CALLER called check-args with $# $ARGUMENTS_INFLECTED, but it requires" \
                 "at least 3.)" \
                 "Usage: check-args  MIN_NUM_ARGS  MAX_NUM_ARGS  USAGE_STRING"
        return 250


        complain "$CALLER: error: too many arguments ($ARGC given, $MAX_ARGS allowed)." "" \
                 "$USAGE"
        return 2
    else
        return 0
    fi
}

function help_flag_in {
    if [[ $# -ne 1 ]]; then
        return 1
    else
        case "$1" in
            -h|--help) return 0;;
            *) return 1;;
        esac
    fi
}

function print-help {
    # This is a helper function for check-args. It is not meant to be
    # called by other code. check-args calls it after ensuring that
    # check-args itself was invoked with the correct arguments,
    # then extracting the information relevant to this function.

    local FN="$1"
    local USAGE="$2"
    local OPTIONAL_MANNAME="$3"
    
    # Arguments explained:
    # 
    # - $FN is the name of the end-user function that callled check-args.
    # 
    # - $USAGE is the minimal usage string that the minimal user function
    #   passed to check-args.
    # 
    # - $MANNAME is an optional description of what the end-user function
    #   actually does.
    
    case "$#" in
        2)
            _stderr "$FUNCTION USAGE"
            return 1 ;;

        
        3)
            _stderr "NAME
    $FN - $OPTIONAL_MANNAME
    
SYNOPSIS
    $FN $USAGE"
           return 2 ;;
           
        
        *) _stderr "print-help: error: wrong number of args (wanted 2 or 3)"
           return 3 ;;
    esac
}


## Implementations of simple Unix commands in bash, to save fork overhead

function basenam {
    # Remove trailing slashes
    local path="${1%/}"    
    # Extract the filename
    echo "${path##*/}"
}

function dirnam {
    # Remove trailing slashes
    local path="${1%/}"
    # Extract the directory path
    echo "${path%/*}"
}

## Helper functions for working with Bash arrays

function member {
    check-args 2 varargs "member ELEM ARG1 [ARG2 ...]" "$@"
    local ELEM="$1"
    shift
    
    for elem in "$@"; do
        if [[ "$ELEM" = "$elem" ]]; then
            return true
        fi
    done

    return false
}

function dirdo {
    check-args 1 varargs 'dirdo "SOME COMMANDS" [DIR [DIR2 ... ]]' "$@"
    local COMMAND="$1"; shift
    local ORIG_DIR="$(pwd)"
    
    local DIRS=("$@")
    if [[ ${#DIRS[@]} -eq 0 ]]; then
        DIRS=(".")
    fi
    
    for DIR in "${DIRS[@]}"; do
        if ! cd "$DIR"; then
            return 1
        fi
        
        (echo "==== $DIR"; echo) >&2
        eval "$COMMAND"
        echo >&2

        if ! cd "$ORIG_DIR"; then
            return 1
        fi
    done
    
    return 0
}

function filter {
    check-args 1 varargs "filter FUNCTION_NAME [FUNCTION_ARGUMENT ...]"

    local FUNCTION_NAME="$1"
    shift    
    if [[ $# -eq 0 ]]; then
        cat | "$FUNCTION_NAME"
    else
        "$FUNCTION_NAME" "$@"
    fi
}

# # Broken somehow. Shell-quoting sucks.
# function funcalias {
#     check-args 2 2 'FUNCTION ALIAS' "$@" || return 1
#     local FN="$1"
#     local ALIAS="$2"
# 
#     eval "function $ALIAS { $FN \"$@\" ;}"
# }

function exalias {
    check-args 2 2 'FUNCTION EXECUTABLE' "$@" || return 1
    local FN="$1"
    local EXE="$2"
    eval "function $FUNCTION_NAME { command $EXE \"\$@\" ;}"
}

function source-if-exists {
    check-args 1 1 'FILE' "$@"
    local FILE="$1"
    if [[ -f "$FILE" ]]; then
        my-source "$FILE"
    else
        return 2
    fi
}

function my-source {
    check-args 1 1 "my-source FILE" "$@"
    local FILE="$1"

    if [[ "$MY_DEBUG_BASH" ]]; then
        SECONDS=0
    fi

    source "$FILE" || return 1

    if [[ "$MY_DEBUG_BASH" ]]; then
        echo "sourced '$FILE' in $SECONDS seconds"
    fi
}


# USE_GREP_TO_PRINT_BASH_DEPS="yeah"
function deps-bash {
    local ORIG_DIR="$(pwd)"
    cd "$QBASHD" || return 2

    local -a FILES
    if [[ $# -eq 0 ]]; then
        FILES=(*)
    else
        FILES=("$@")
    fi

    function _deps_search_for_regex_in_file {
        local FILENAME="$1"
        if ! installed "rg" || [[ "$USE_GREP_TO_PRINT_BASH_DEPS"  ]]; then
            echo "$FILE"

            # FIXME: doesn't take effect, even with --color=always: GREP_COLORS="mt=black"
            command grep \
                --color=never \
                '^######## ' \
                "$FILE" \
                && echo
        else
            rg \
                --with-filename \
                --no-line-number \
                --colors path:fg:blue \
                --colors match:fg:black \
                '^######## ' \
                "$FILE" \
              && echo
        fi
    }

    for FILE in "${FILES[@]}"; do
        _deps_search_for_regex_in_file "$FILE"
    done

    cd "$ORIG_DIR"
}
