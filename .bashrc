umask 0022
export CLICOLOR=1

#### Bash debugging settings

# Uncomment this to enable various hackish debugging statements in this file.
# export MY_DEBUG=true

# Run this function interactively to enable tracing *without*
# the many prompt-related commands that automatically run every time I enter a
# command. This makes the trace much more uncluttered and readable.
function setx {
    unset PROMPT_COMMAND
    export PS1="\u@\h:\w\\$ "
    set -x
}

if [[ -n "$MY_DEBUG" ]]; then
    setx
fi

#### Bash metaprogramming library "settings"

## Helper functions

function carp {
    local CALLER="${FUNCNAME[1]}"
    if [[ -z "$CALLER" ]]; then
        CALLER="bash"
    fi
    
    echo "$CALLER:" "$@" >&2
}

function complain {
    for STRING in "$@"; do
        echo "$STRING" >&2
    done
}

if [[ "$MY_DEBUG" ]]; then
    function debug {
        echo "Debug: $@" >&2
    }
else
    function debug {
        : # no-op
    }
fi

## Implementations of simple Unix commands in bash, to save fork(1) overhead

function which { # like Unix which(1)
    type -P "$@"
}

function base { # like Unix basename(1)
    # Remove trailing slashes
    local path="${1%/}"    
    # Extract the filename
    echo "${path##*/}"
}

function dir { # like Unix dirname(1)
    # Remove trailing slashes
    local path="${1%/}"
    # Extract the directory path
    echo "${path%/*}"
}

function check-args {
    local FUNCTION="${FUNCNAME[1]}"
    
    if [[ $# -lt 3 ]]; then
        # Wrong number of args to check-args itself.
        # Print an error and return.

        local ARGUMENTS_INFLECTED
        if [[ $# -eq 1 ]]; then
            ARGUMENTS_INFLECTED="arguments"
        else
            ARGUMENTS_INFLECTED="argument"
        fi
        complain "check-args: error: $FUNCTION passed the wrong number of arguments to check-args itself" \
                 "($FUNCTION called check-args with $# $ARGUMENTS_INFLECTED, but it requires" \
                 "at least 3.)" \
                 "usage: checks_args MIN_NUM_ARGS  MAX_NUM_ARGS  USAGE_STRING"
        return 250
    else
        # The caller passed the correct number of args to check-args itself.
        # Now check whether the caller's caller called the caller properly.
        
        local MIN_ARGS="$1"; shift
        local MAX_ARGS="$1"; shift
        local USAGE="$1"; shift
        local ARGC=$#
        if [[ $ARGC -lt $MIN_ARGS ]]; then
            local VARAGS_CLAUSE
            if [[ "$MAX_ARGS" = "varargs" ]]; then
                VARAGS_CLAUSE=" or more"
            else
                VARAGS_CLAUSE=""
            fi
            complain "$FUNCTION: error: too few arguments ($ARGC given, ${MIN_ARGS}${VARAGS_CLAUSE} required)." \
                     "usage: $USAGE"
            return 1
        elif [[ "$MAX_ARGS" != "varargs" ]] && ((ARGC > MAX_ARGS)); then
            complain "$FUNCTION: error: too many arguments ($ARGC given, $MAX_ARGS allowed)." \
                     "usage: $USAGE"
            return 2
        else
            return 0
        fi
    fi
}

## Array helper functions

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
    function stderr {
        echo "$@" >&2
    }

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
        
        stderr "==== $DIR"; stderr
        eval "$COMMAND"
        stderr

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

function funcalias {
    local FUNCTION_NAME  EXECUTABLE_PATH

    if [[ $# -ne 2 ]]; then
        complain "funcalias: error: wrong number of args. Usage: funcalias FUNCTION_NAME EXECUTABLE_PATH"
    else
        FUNCTION_NAME="$1"
        EXECUTABLE_PATH="$2"
        eval "function $FUNCTION_NAME { command $EXECUTABLE_PATH \"\$@\" ;}"
    fi
}

function installed {
    check-args 1 1 "installed COMMAND_NAME" "$@"
    local COMMAND="$1"
    if command -p "$COMMAND" > /dev/null 2>&1 || [[ -x "$HOMEBREW_BIN/$COMMAND" ]]
    then
        return 0
    else
        return 1
    fi
}

#### OS-specific settings

MY_OS="$(uname -s)"

function on-linux {
    local LINUX_REGEX="Linux"
    [[ "$MY_OS" =~ $LINUX_REGEX ]]
}

function on-macos {
    [[ "$MY_OS" = "Darwin" ]]
}

if on-macos; then
    # On Linux these are set automatically
    set -a
    XDG_CONFIG_HOME="$HOME/.config"
    XDG_CACHE_HOME="$HOME/.cache"
    XDG_DATA_HOME="$HOME/.local/share"
    XDG_STATE_HOME="$HOME/.local/state"
    set +a
fi

## Clipboard settings

if on-macos; then
    function pbc {
        local FILENAME="$1"
        if [[ "$FILENAME" ]]; then
            pbcopy < "$FILENAME"
        else
            echo "pbc: error: missing argument. Usage: pbc FILENAME"
        fi
    }
fi

#### Color settings

function color-printf {
  local COLOR_NAME="$1"
  shift

  local COLOR BRIGHT
  case "$COLOR_NAME" in
      black) COLOR=30; BRIGHT=0;;
      red) COLOR=31; BRIGHT=0;;
      green) COLOR=32; BRIGHT=0;;
      yellow) COLOR=33; BRIGHT=0;;
      blue) COLOR=34; BRIGHT=0;;
      magenta) COLOR=35; BRIGHT=0;;
      cyan) COLOR=36; BRIGHT=0;;
      white) COLOR=37; BRIGHT=0;;
      bright-black) COLOR=90; BRIGHT=1;;
      bright-red) COLOR=91;  BRIGHT=1;;
      bright-green) COLOR=92; BRIGHT=1;;
      bright-yellow) COLOR=93; BRIGHT=1;;
      bright-blue) COLOR=94; BRIGHT=1;;
      bright-magenta) COLOR=95; BRIGHT=1;;
      bright-cyan) COLOR=96; BRIGHT=1;;
      bright-white) COLOR=97; BRIGHT=1;;
      *) echo "Invalid color name '$COLOR_NAME'" >/dev/stderr; return 1;;
  esac

  # # No
  # echo "${START}${BRIGHT}$@${COLOR}${RESET}"
  
  printf '\033[%d;%dm%s\033[0m\n' \
         "$BRIGHT" \
         "$COLOR" \
         "$@"
}

# function word_wrap {
#     local STRING
#     if [[ $# -eq 1 ]]; then
#         STRING="$1"
#         ...

function okl {
    local RGB="$1"
    if [[ -n "$RGB" ]]; then
        complain "Sorry, RGB -> Oklch conversion isn't implemented yet. Try 'okl'"
    else
        open -u "https://oklch.com/"
    fi
}

## ANSI SGR 4-bit color codes

 ANSI='\033['
 CYAN='36m'
RESET='\033[0m'

BRIGHT_CYAN='96m'

function cyan {
    check-args 0 0 "cyan"
    echo -e "$ANSI$CYAN=cyan=$RESET"
}

function color {
    # check-args 2 2 "color STRING_TO_DISPLAY COLOR_NAME_IN_ALL_CAPS"
    local STRING="$1"
    local COLORNAME="$2"
    local COLORCODE
    
    case "$COLORNAME" in
        black)
            COLORCODE="30m";;
        red)
            COLORCODE="31m";;
        green)
            COLORCODE="32m";;
        yellow)
            COLORCODE="33m";;
        blue)
            COLORCODE="34m";;
        magenta)
            COLORCODE="35m";;
        cyan)
            COLORCODE="36m";;
        white)
            COLORCODE="37m";;
        brightblack|bblack|Bblack)
            COLORCODE="90?m";;
        brightred|bred|Bred)
            COLORCODE="91m";;
        brightgreen|bgreen|Bgreen)
            COLORCODE="92m";;
        brightyellow|byellow|Byellow)
            COLORCODE="93m";;
        brightblue|bblue|Bblue)
            COLORCODE="94m";;
        brightmagenta|bmagenta|Bmagenta)
            COLORCODE="95m";;
        brightcyan|bcyan|Bcyan)
            COLORCODE="96m";;
        brightwhite|bwhite|Bwhite)
            COLORCODE="97m";;
        *)
            complain "Unsupport color name $COLORNAME"
            return 1;;
    esac
        
    echo -e "${ANSI}${COLORCODE}${STRING}${RESET}"
}

# bnw (Black 'n' White): remove non-printable chars,
# such as ANSI color codes
# function bnw {
#     for ARG in <<< "$@"; do
#         echo "$ARG" | tr -d '[:cntrl:]'
#     done
# }

function bnw {
    while IFS= read -r LINE; do

        # Strip ANSI color codes
        CLEAN_LINE=$(echo "$LINE" | sed -r 's/\x1B\[[0-9;]*[a-zA-Z]//g')

        # Strip control characters
        CLEAN_LINE=$(echo "$CLEAN_LINE" | tr -d '[:cntrl:]')
        
        echo "$CLEAN_LINE"
    done
}

# Take a hex triplet, and print it in the indicated color.
# Requires a terminal that supports (ANSI SGR) 24-bit color,
# a.k.a. true color
function hex {
    if [[ $# -ne 1 ]]; then
        complain "Wrong number of arguments. Usage: hex RRGGBB"
        return 1
    else
        # Extract the RGB values from the hex color code
        local hex=${1#"#"}
        local r=$(printf '0x%0.2s' "$hex")
        local g=$(printf '0x%0.2s' "${hex#??}")
        local b=$(printf '0x%0.2s' "${hex#????}")
        
        # Convert the hex values to decimal
        local R=$(printf '%d' "$r")
        local G=$(printf '%d' "$g")
        local B=$(printf '%d' "$b")
    
        # Print the string in the specified color
        printf "\e[38;2;%d;%d;%dm%s\e[0m\n" "$R" "$G" "$B" "$hex"
    fi
}

#### Filename/path/dir/cd-related functions

function dashify-path {
    if ! check-args 0 1 '[PATH] # defaults to current working directory]' "$@"
    then
        return 1
    elif [[ $# -eq 0 ]]; then
        local ABSOLUTE="$(realpath)"
    elif [[ $# -eq 1 ]]; then
        local ABSOLUTE="$(realpath "$1")"
    fi
    
    local DASHIFIED=$(echo "$ABSOLUTE" | sed 's|^/|| ; s|[/]|---|g')
    echo "$DASHIFIED"
}

function cds { cd ~/src ;}
function cdq { cd ~/src/1/qdw-dotfiles ;}

# cdd - cd to the enclosing dir of a file/dir.
# If the argument is a symlink, resolve it and then cd to the enclosing dir
# of the file that it point to.
function cdd {
    if [[ $# -ne 1 ]]; then
        echo "cdd: wrong number of arguments. Usage: cdd PATH"
    else
        cd "$(dirname "$(realpath "$1")")"
    fi
}

#### Homebrew settings

if [[ ! "$PATHS_SET_AT" ]]; then
    for HDIR in "/opt/homebrew" "/usr/local"; do
        if [[ -x "$HDIR/bin/brew" ]]; then
            set -a    
            HOMEBREW_ROOT="$HDIR"
            HOMEBREW_BIN="$HDIR/bin"
            
            PATH="$HOMEBREW_BIN:$HDIR/sbin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin:$PATH"

            # man pages for brew(1) itself
            MY_MANPATH="$HDIR/manpages"           
            
            # man(1) and info(1) pages for software installed using brew
            MY_MANPATH="$HDIR/share/man:$MY_MANPATH"
            INFOPATH="$HDIR/share/info"

            PATH=$(echo $PATH | sed -e 's/[:]\{2,\}/:/g' -e 's/[:]$//')

            # Fix a problem with the nmap formula
            PYTHONPATH="${PYTHONPATH}:${HDIR}/lib/python3.9/site-packages"
            set +a

            break
        fi
    done
fi

# Prompt before overwriting files,
if [[ "$MY_OS" = "Darwin" ]] && [[ -f "$HOMEBREW_BIN/trash" ]]; then
    function rm {
        trash "$@"
    }
fi

## If on macOS, use the Homebrew versions of common command-line utilities
## instead of the bundled macOS versions, as the Homebrewed versions
## are more frequently updated.

for GNU_COMMAND in cut date dircolors find head roff sed sort stat tac tail; do
    BREWED_GNU="${HOMEBREW_BIN}/g${GNU_COMMAND}"
    if [[ -f "$BREWED_GNU" ]]; then
        funcalias "$GNU_COMMAND" "$BREWED_GNU"
    fi
done

## Show all ignore patterns in effect for the cwd

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

function fdd {
    command fd \
        --exclude .git \
        --exclude .github \
        --exclude node_modules \
        --full-path \
        --hidden \
        --no-ignore-vcs \
        --follow \
        "$@"
}

function fd {
    fdd --color=always "$@" | $PAGER
}

function gfd {
    local GFD_REGEX USAGE
    USAGE="Usage: gfd GFD_REGEX"
    
    if [[ $# -eq 0 ]]; then
	echo "gfd: error: missing argument. $USAGE"
    elif [[ $# -eq 1 ]]; then
	GFD_REGEX="$1"
	(fd "$GFD_REGEX"; g "$GFD_REGEX";) | $PAGER
    else
        echo "gfd: error: wrong number of arguments. $USAGE"
    fi
}

function fdgrep {
    # check-args 1 varargs "fdgrep FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
    local FILENAME_REGEX="$1"; shift    
    (fdd --type f "$FILENAME_REGEX" -x $MY_GREP -I --color=always $GREP_EXCLUDE_FLAGS "$@" \;)
}

# Recursively search the current working directory for files that don't
# end in a single newline, and print their names
function newlineless {
    fd -t f -x bash -c 'tail -c 1 "$1" | ggrep -qvE "^$" && echo "$1"' _ {} \;
}

# Grep TypeScript files only
function tsgrep { fdgrep "[.]ts$" "$@" ;}

function fdoccur {
    # check-args 2 varargs "fdoccur FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
    local FILENAME_REGEX="$1"; shift
    fdgrep "$FILENAME_REGEX" -l -m 1 "$@"
}

function file-types {
    fdd -t f | sed 's/.*\([.].*\)/\1/' | sort \
        | uniq \
        | grep '|(rb|kt|kts|py|jar|java|sql|ts|js|json)|yaml|yml'
}

function file-type-counts {
    fdd -t f | sed 's/.*\([.].*\)/\1/' | sort \
        | uniq -c \
        | sort --reverse --general-numeric-sort | grep '|(rb|kt|kts|py|jar|java|sql|ts|js|json)|yaml|yml'
}

#### grep settings

set -a

# Prefer ripgrep if installed.
if installed "rg"; then
    MY_GREP="rg"
    MY_GREP_OPTIONS="--color=always" # --no-line-number"
    # MY_GREP_OPTIONS="$MY_GREP_OPTIONS -H --column -n"
    GREP_RECURSE="--no-heading --follow --no-one-file-system --no-line-number"
    GREP_EXCLUDE="" # rg uses a .ignore file, not command-line flags
    GREP_COLOR_FLAGS="--colors path:fg:blue --colors line:fg:yellow --colors column:fg:cyan --colors match:fg:red"

# If ripgrep is not installed, assume GNU grep.
else
    if on-macos && installed "ggrep"; then
        MY_GREP="ggrep"
    else
        MY_GREP="grep"
    fi
    
    MY_GREP_OPTIONS="-n -I --color=always"
    GREP_RECURSE="-R"
    GREP_EXCLUDE="--exclude-dir=.git --exclude-dir=.github --exclude-dir=cdk.out --exclude-dir=node_modules --exclude=.npmignore --exclude=package-lock.json --exclude-dir=__snapshots__ --exclude '*.js'"
    
    GREEN="32"
    YELLOW="33"
    local FILENAME_COLOR="fn=${GREEN}"
    local LINE_NUMBER_COLOR="ln=${YELLOW}"
    export GREP_COLOR_FLAGS=""
    export GREP_COLORS="${FILENAME_COLOR}:${LINE_NUMBER_COLOR}"
fi
set +a

function grep {
    # When using rg as my grep implementation, I often type -R out of habit,
    # but rg has no -R flag (and it's recursive by default).
    # So, if I try to pass that flag to rg, just remove it.
    local ARGS=()
    if [[ "$MY_GREP" = "rg" ]]; then
        for ARG in "$@"; do
            # FIXME: rewrite this properly, using Bash's builtin getopts
            if [[ "$ARG" != "-R" ]]; then
                ARGS+=("$ARG")
            fi
        done
    else
        ARGS=$@
    fi

    $MY_GREP \
        $MY_GREP_OPTIONS \
        $GREP_COLOR_FLAGS \
        $GREP_EXCLUDE_FLAGS "${ARGS[@]}" | $PAGER
}

function g { grep $GREP_RECURSE "$@" ;}
function gbr { g "$@" ~/.bashr* ~/.*profile* ~/.site.bash* ;}
function ge { g "$@" ~/.emacs.d ;}

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

# fip (Filter In Place) - filter lines matching regex from a file,
# modifying the file in-place to remove those lines. If you specify
# multiple files, it does this to all of them.
function fip {
    # check-args 2 varargs
    
    local FIP_REGEX="$1"
    shift
    
    # if [[ "$1" = "-"  ]]; then
    #     local LINE
    #     while true; do
    #         LINE="$(read)"
    #         if [[ "$LINE" = "" ]]; then
    
    for FILE in "$@"; do
        command grep -vE "$FIP_REGEX" "$FILE" | sponge "$FILE"
    done
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

#### Bash settings

function source-if-exists {
    check-args 1 1 "source-if-exists FILE" "$@"
    local FILE="$1"
    if [[ -f "$FILE" ]]; then
        my-source "$FILE"
    fi
}

function my-source {
    check-args 1 1 "my-source FILE" "$@"
    local FILE="$1"

    if [[ $MY_DEBUG ]]; then
        SECONDS=0
    fi

    source "$FILE"
    
    if [[ $MY_DEBUG ]]; then
        echo "sourced '$FILE' in $SECONDS seconds"
    fi
}

# grbash - grep my Bash config files
function grbash {
    if [[ $# -ne 1 ]]; then
        echo "grbash: error: wrong number of arguments. Usage: grbash GRBASH_REGEX [GREP_OPTION ...]"
    else
        grep -R "$@" ~/.bashrc ~/.bashrc.d ~/.bash_profile ~/.bash_login ~/.bash_logout ~/.site.bash.d
    fi
}

#### Bash history settings

set -a

# Add a timestamp to history(1) output, using strftime(3) formatting.
HISTTIMEFORMAT='  %Y-%m-%d %H:%M:%S %z    '

# Keep all history forever
HISTFILESIZE=-1
HISTSIZE=-1

set +a

#### Bash settings that shouldn't be checked into git
#### (either because they contain secrets, or because of license issues)

source-if-exists ~/.bashrc.d/daemons.bash
source-if-exists ~/.site.bash.d/preamble.bash
source-if-exists ~/.secrets.bash

## Bash completion settings
## Dependencies:
##     - Homebrew itself
##     - Homebrewed bash >= 5 (set as your login shell)
##     - The following additional Homebrew formulas:
##         - bash-completion@2
##         - bash-git-prompt
##         - gh
##         - docker
##         - rclone
##         - openshift


set -a
GIT_PROMPT_THEME="Custom"
GIT_PROMPT_ONLY_IN_REPO=1
set +a


if [[ -r "$HOMEBREW_ROOT/etc/profile.d/bash_completion.sh" ]]; then
    export BASH_COMPLETION_DIR="$HOMEBREW_ROOT/etc/bash_completion.d"

    # comp - activate shell completion
    function comp {
        function sc {
            for FILENAME in "$@"; do
                my-source "$BASH_COMPLETION_DIR/${FILENAME}"
            done
        }

        case "$1" in
            aws*)
                sc "aws_bash_completer"
                ;;
                
            brew)
                my-source "$HOMEBREW_ROOT/completions/bash/brew"
                ;;
            
            docker)
                sc docker \
                   docker-compose \
                   docker-machine-prompt.bash \
                   docker-machine-wrapper.bash \
                   docker-machine.bash
                ;;
        
            gh|github)
                sc "gh"
                ;;
        
            git)
                my-source "$HOMEBREW_ROOT/share/gitprompt.sh"    
                sc "gh" \
                   "git-completion.bash"
                ;;
            
            openshift|oc|kubernetes|kube|k8s)
                sc "oc" \
                   "kubecfg" \
                   "kubectl" \
                   "minikube"
                ;;

            *)
                sc "$1"
                ;;
        esac
    }

    function ag {
        comp "git"
        export GIT_COMPLETION_ACTIVATED=1
    }
    
    # AWS command completion - pretty fast,
    # so it's OK to run this at shell startup time if you want
    function aw {
        comp "aws_bash_completer"
    }
    
    function my_prompt_function {
        REALPATH="$(realpath "$PWD")"
        
        if [[ -d "$REALPATH/.git" \
              && ( ! "$REGEX_FOR_REPOS_TO_EXCLUDE_FROM_GIT_COMPLETION" \
                       || \
                   ! "$REALPATH" =~ $REGEX_FOR_REPOS_TO_EXCLUDE_FROM_GIT_COMPLETION ) \
              && ( ! "$GIT_COMPLETION_ACTIVATED" \
                       || \
                     "$INSIDE_EMACS" ) ]]; then
            my-source "$HOMEBREW_ROOT/share/gitprompt.sh"
            my-source "$BASH_COMPLETION_DIR/gh"
            my-source "$BASH_COMPLETION_DIR/git-completion.bash"
            export GIT_COMPLETION_ACTIVATED=1
        fi
    }
    
    MY_PROMPT_FUNCTION_DEFINITION=$(command -V my_prompt_function | tail -n +2)
    ## not for upstart_web export PROMPT_COMMAND="$MY_PROMPT_FUNCTION_DEFINITION; my_prompt_function"
fi

#### Scheme Shell settings

if [[ -d "/opt/scsh" ]]; then
    SCSH_PATH="/opt/scsh/bin"
    LATEST_SCSH_VERSION="$(ls /opt/scsh/lib | sort --general-numeric-sort | tail -1)"
    SCSH_LDFLAGS="-L/opt/scsh/lib/scsh-${LATEST_SCSH_VERSION}"
fi

#### Unicode-related settings

function zero-width-joiner {
    echo -en '\u200D' | pbcopy
}
function zwj {
    zero-width-joiner "$@"
}
function nbsp {
    zero-width-joiner "$@"
}

#### Pager settings

set -a

LESS="--RAW-CONTROL-CHARS --quit-if-one-screen --no-init --mouse"

if [[ "$INSIDE_EMACS" ]]; then
    # Emacs shell-mode is a dumb terminal, so use special pager settings there.
    export PAGER="cat"
    export AWS_PAGER="$PAGER"
    export GIT_PAGER="$PAGER"
    export PSQL_PAGER="$PAGER"
    function fd {
        fdd "$@"
    }
else
    # If we're a non-dumb terminal, then page fancily,
    # with the color and pagination options I like.
    if command -v jless > /dev/null; then
        # export AWS_PAGER="jless"
        export AWS_PAGER="bat --language json --style=rule,snip"
    else
        export AWS_PAGER="$PAGER"
    fi

    export PAGER="less"
    
    # Show an extra line before each match - useful when running 'git diff'
    export GIT_PAGER="$PAGER -j 2"
    
    export PSQL_PAGER="less --no-init --mouse"
fi

ACK_PAGER="$PAGER"
BAT_PAGER="$PAGER"
MANPAGER="$PAGER"

# PAGER="bat"
# BAT_PAGER="batless"

set +a

#### Markdown settings

# export GLAMOUR_STYLE="Dark"
function readme {
    gh repo view
}

## #FIXME rewrite 'function bbat {...}' to use 'gh' and GLAMOUR_STYLE, which should make the output look nicer

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub)
function bbat {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
}

function markdown2html {
    pandoc "$1" -t html
}

#### Linker settings

function ldd-app {
    local APP_NAME EXECUTABLE_PATH

    APP_NAME="$1"

    for APP_DIR in /Applications ~/Applications; do
        EXECUTABLE_PATH="${APP_DIR}/${APP_NAME}.app/Contents/MacOS/${APP_NAME}"
        if [[ -f "$EXECUTABLE_PATH" ]]; then
            otool -L "$EXECUTABLE_PATH"
        fi
    done
}

function ldd {
    if on-macos; then
        otool -L "$@" | $PAGER
    else
        command ldd "$@" | $PAGER
    fi
}

# # Tell man to display pages in color (it uses the deprecated Termcap system)
# set -a
# # Section headings? (bold orange)
# LESS_TERMCAP_mb=$'\e[1;2;130m'
# # Program name? (bold purple)
# LESS_TERMCAP_md=$'\e[1;4;20m'
# # Option names (dull blue)
# LESS_TERMCAP_us=$'\e[0;9;25m'
# set +a

#### AWS settings

### Generic AWS settings

function aws-account {
    echo "$(AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text)"
}

### AWS IAM settings

function iam {
    if [[ $# -eq 0 ]]; then
        echo "USAGE:"
        echo
        echo 'iam --hostname "$host" --port "$port" --region "$region" --username "$user" --profile "$profile"'
    else
        aws rds generate-db-auth-token "$@" | tee >(pbcopy)
    fi
}

# Print a list of IAM instances in the current account (each double-quoted)
function instances {
    aws rds describe-db-instances \
        | jq '.DBInstances[] | .DBInstanceIdentifier'
}

# tags - show CDK tags for a project
function tags {
    local ORIG_DIR
    ORIG_DIR="$(pwd)"

    local GIT_WORKING_COPY_ROOT_DIR
    GIT_WORKING_COPY_ROOT_DIR="$(git rev-parse --show-toplevel)"
    if [[ $? -eq 0 ]]; then
        if cd "$GIT_WORKING_COPY_ROOT_DIR/cdk.out"; then
            CDK_OUTPUT_FILES="$(fdd -t f '.json$')"
            if [[ -n "$CDK)OUTPUT_FILES" ]]; then
                for FILENAME in $CDK_OUTPUT_FILES; do
                    gron "$FILENAME" | grep tags | awk "{print \"$FILENAME: \" \$0}"
                done
            fi
        fi
    fi
    
    cd "$ORIG_DIR"
}

function wait_for_proxy_to_come_up {
    local PROXY_NAME  STATUS
    
    if [[ $# -eq 1 ]]; then
        PROXY_NAME="$1"
        while true; do
 
            # Broken
            STATUS=`aws rds describe-db-proxies | jq ".DBProxies | select(.DBProxyName==\"$PROXY_NAME\") | .Status"`
 
 
            if [[ $STATUS = '"creating"' ]]; then
                sleep 60
            else
                break
            fi
        done
        echo "$STATUS at $(date)"
    else
        echo "wait_for_proxy_to_come_up: wrong number of arguments. Usage: wait_for_proxy_to_come_up PROXY_NAME"
    fi
}
            

function rds {
    function usage {
        echo "Usage: rds [ls | list]"
    }

    if [[ $# -eq 0 ]]; then
        echo -n "Missing argument."; usage
    elif [[ $# -gt 1 ]]; then
        echo "Too many arguments."; usage
    elif [[ "$1" = "ls" ]] || [[ "$1" = "list" ]]; then
        shift
        rds-ls "$@"
    else
        echo "No such subcommand '$1'."; usage
    fi
}

# rds-ls ("RDS LiSt") - list RDS instances (highlighting the hostname portio nof the FQDN)
function rds-ls {
    if [[ -n "$1" ]]; then
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*' \
            | grep "$1"
        
    else
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*'
    fi
}

function list-rds-instances { rds-ls "$@" ;}

function list-secrets { aws secretsmanager list-secrets ;}
function list-secret-ids { list-secrets | jq --raw-output '.SecretList[].Name' | sort ;}

### AWS SSM (a.k.a. Secrets Manager) settings

function get-secret {
    aws secretsmanager get-secret-value --secret-id "$1" \
        | jq '.SecretString | fromjson'
}
function secret { get-secret "$@" ;}

### AWS S3 setttings

function in-dev {
    # (AWS_DEV_ACCOUNT_NUMBER must be defined in preamble.bash)
    if ! [[ "$AWS_ACCESS_KEY_ID" && "$AWS_SECRET_ACCESS_KEY" && "$AWS_SESSION_TOKEN" ]]; then
        carp "AWS_ACCOUNT is defined, but not the auth-related AWS_* env vars"
        return 1
    else
        local ACCOUNT_NUMBER="$(AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text)"
        if [[ "$ACCOUNT_NUMBER" != "$AWS_DEV_ACCOUNT_NUMBER" ]]; then
            carp "You're logged into '$ACCOUNT_NUMBER', but the dev account is '$AWS_DEV_ACCOUNT_NUMBER'"
            return 1
        else
            return 0
        fi
    fi
}

function ls-buckets {
    check-args 0 1 'ls-buckets [MATCHING_PREFIX]' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        aws s3 ls
    elif [[ $# -eq 1 ]]; then
        aws s3 ls --bucket-name-prefix "$1"
    fi
}

function rm-bucket {
    check-args 1 1 'rm-bucket BUCKET_NAME' "$@" || return
    if ! in-dev 2>/dev/null; then
        complain "Not in dev; refusing to delete data!"
        return 1
    else
        local BUCKET_NAME="$1"
        
        # Tell the bucket to stop writing a log message every time
        # someone accesses it. This is necessary in the case that
        # these access logs are being stored in the bucket itself;
        # in that case, we won't be able to empty the bucket,
        # because new logs will keep streaming in while we delete
        # the old ones. And S3 refuses to delete buckets with files
        # in them.
        #
        # Of course, other AWS stuff may be writing to the bucket
        # as well. It's the caller's responsibility to stop
        # that stuff from writing, since we can't know what it is.
        aws s3api put-bucket-logging --bucket "$BUCKET_NAME" \
                                     --bucket-logging-status '{}' \
            || return 1
        
        set -o pipefail
        aws s3 rm --recursive "s3://${BUCKET_NAME}" >/dev/null \
            && aws s3api delete-bucket --bucket "$BUCKET_NAME"
        set +o pipefail
    fi
}

#### Editor settings: Emacs Emacs Emacs Emacs Emacs (and more Emacs)

set -a
EMACS_BINARY="$HOMEBREW_BIN/emacs"
EMACSCLIENT_BINARY="$HOMEBREW_BIN/emacsclient"
set +a

### Emacs daemon settings

function emacs-daemon-pid {
    pgrep -U "$USER" -fi "$@" "Emacs --bg-daemon"
}

function pe {
    emacs-daemon-pid -l
}

function the-emacs-daemon-is-running {
    emacs-daemon-pid > /dev/null
    return $?
}

# ke (Kill Emacs-daemon): kill the Emacs daemon, if running.
function ke {
    local PID="$(emacs-daemon-pid)"
    if [[ -n "$PID" ]]; then
        kill "$PID"
    else
        complain "It doesn't look like the emacs daemon is running"
        return 1
    fi
}

# If we're in a terminal session, start an emacsclient session inside that terminal
# (first starting the Emacs server, if it's not already running).
#
# This looks just like a normal terminal emacs session, but starts faster
# (and can see buffers from other emacsclient sessions, but whatever).
function emacs {
    # If, instead of running a command like crontab(1) that implicitly runs $VISUAL,
    # I directly type "emacs my-file", then...
    if [[ "$1" = "-c" && shift ]]   ||   [[ "$INSIDE_EMACS" ]]; then
        # If I'm inside an Emacs interactive-shell-mode buffer,
        # then edit my-file in the same same Emacs window.
        if [[ $# -gt 0 ]]; then
            "$EMACSCLIENT_BINARY" --no-wait "$@"
        else
            echo "You're already in emacs, genius" >&2
        fi
    else
        local DAEMON_STATUS
        DAEMON_STATUS=$(the-emacs-daemon-is-running)
        if the-emacs-daemon-is-running; then
            "$EMACSCLIENT_BINARY" --tty "$@"
        else
            se "$@"
        fi
    fi
}

# se (Start Emacs-daemon): start the Emacs daemon.
function se {
    if the-emacs-daemon-is-running; then
        complain "Looks like the emacs daemon is already running"
        return 1
    else
        echo "Starting the Emacs daemon..."
        command emacs --bg-daemon >/dev/null 2>&1
        
	# Give it time to start up and load my ~/.emacs.d/init.el
        sleep 4
        emacs "$@"
    fi
}

# re (Restart Emacs-daemon): stop it, then start it.
function re {
    ke
    se
}

MY_EDITOR="$HOME/bin/emacs-function"

set -a
EDITOR="$MY_EDITOR"
# GIT_EDITOR="$MY_EDITOR"
# HOMEBREW_EDITOR="$HOME/bin/emacs-function"
# PSQL_EDITOR="$MY_EDITOR"
VISUAL="$MY_EDITOR"
set +a

## FIXME: find a more elegant way
# (This function depends on the third-party newtab() function;
# see ~/.bashrc.d/third-party.bash)
# 
# te ("TErminal window") - create a new Terminal.app window in the same directory I'm in when I run te.
# function te {
#     newtab bash # -c "cd $(pwd); $@" #FIXME: get this -c thing to work
# }
# function nt { newtab "$@" ;}

# Required for the corner case where you
# want to run emacs M-x shell in Terminal.app
# without getting error messages in Emacs.
# Not needed for other terminal emulators.
function update_terminal_cwd {
    : # no-op
}

# Required to enable the Terminal.app feature
# where you can click on a directory name
# to open it in Finder... but I don't like
# that feature, so I'm commenting it out for now.
#
# function update_terminal_cwd {
#     # Identify the directory using a "file:" scheme URL,
#     # including the host name to disambiguate local vs.
#     # remote connections. Percent-escape spaces.
#     local SEARCH=' '
#     local REPLACE='%20'
#     local PWD_URL="file://$HOSTNAME${PWD//$SEARCH/$REPLACE}"
#     printf '\e]7;%s\a' "$PWD_URL" > /dev/null
# }

#### Terminal emulator settings

export TERM=xterm-256color # used by tmux

function strip_control_characters_from {
    local STRING
    if [[ $# -eq 1 ]]; then
        echo "$(tr -cd  "[:print:]"  "$STRING")"
    else
        echo "strip_control_characters_from: error: wrong number of arguments. Usage: strip_control_characters_from STRING" >&2
    fi
}

# title STRING: set the window title of an xterm/iTerm2/rxvt window to STRING.
# This works by using a special ANSI-like escape code that not all terminal
# emulators support.
function title { echo -ne "\033]2;" "$1" "\007" ;}

## iTerm2 settings

# See https://iterm2.com/documentation-captured-output.html
function cl { printf "\e]1337;ClearCapturedOutput\e\\" ;}

function activate-iTerm2-integration {
    # Enable "iTerm2 integration," that is, a bunch of fancy features
    # specific to the iTerm2 terminal emulator (example: click-to-download
    # over ssh plus lots more).
    #
    # It would slow down shell startup considerably if I automatically
    # sourced this code, so I've separated it into a function that you
    # can call explicitly when you want it.
    my-source ~/.iterm2_shell_integration.bash
}

## Bash prompt settings

# If this is my work laptop, just put "laptop" as the hostname,
# not the rather-verbose hostname assigned by DHCP.
#
# Actually, I'm disabling this; now that I'm all DevOps-y,
# I never get to ssh anywhere.
PS1_LOGO=""
if [[ -n "$LAPTOP_HOSTNAME" ]] && [[ $HOSTNAME = "$LAPTOP_HOSTNAME" ]]; then
    PS1_HOSTNAME="laptop"
else
    PS1_HOSTNAME="\\h"
fi

# Unicode logos as visual cues for different hosts/apps
if [[ "$INSIDE_EMACS" ]]; then
    PS1_LOGO=$'\u2130' # Cursive uppercase E
elif [[ -n "$LAPTOP_HOSTNAME" ]] && [[ $HOSTNAME = "$LAPTOP_HOSTNAME" ]]; then
    PS1_LOGO=$'\uF8FF' # Apple logo
fi

#export PS1="\\[\\e[33;1m\\]${PS1_LOGO}\\u@${PS1_HOSTNAME}:\\w\\$\\[\\e[0m\\] "
export PS1="\\[\\e[33;1m\\]\\w\\$\\[\\e[0m\\] "
export DEFAULT_PS1="$PS1"

# macOS: suppress nag message about Bash being deprecated in favor of zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

#### Homebrew settings

set -a
HOMEBREW_BAT=1
HOMEBREW_BAT_CONFIG_PATH=~/.config/bat/homebrew.config

HOMEBREW_NO_ANALYTICS=1
HOMEBREW_NO_ENV_HINTS=1
set +a

function brew {
    if [[ "$1" = 'help' ]] || [[ ${!#} = '--help' ]]; then
        command brew "$@" | $PAGER
    else
        command brew "$@"
    fi
}

function brew-deps-all {
    brew deps --tree --install | $PAGER
}

function brew-all-deps { brew-deps-all "$@" ;}

function brew-leaves {
    brew-deps-all | command ggrep -E '^[[:alpha:]]' | sort | uniq | $PAGER
}

function brew-leaf {
    local LEAF_NUMBER
    if [[ $# -eq 0 ]]; then
        LEAF_NUMBER=1
    elif [[ $# -eq 1 ]]; then
        LEAF_NUMBER="$1";
    else
        echo "brew-leaf: error: wrong number of arguments."
        echo "Usage: brew-leaf [NUMBER] # 1-based"
        return
    fi

    brew-leaves | head "-${LEAF_NUMBER}" | tail -1
}

function brew-next-leaf {
    local LEAF_FILE=~/.brew-next-leaf
    
    local LEAF_NUM
    if [[ -f "$LEAF_FILE" ]]; then
        LEAF_NUM=$(cat "$LEAF_FILE")
    else
        LEAF_NUM=1
    fi

        
    if brew-leaf "$LEAF_NUM"; then
        (( LEAF_NUM++ ))
        echo "$LEAF_NUM" > "$LEAF_FILE"
    fi
}
function bnl()  { brew-next-leaf "$@" ;}
function leaf { brew-next-leaf "$@" ;}


function bi { brew install "$@" ;}
function bo { brew update; brew outdated ;}
# function bo { brew doctor; brew update && brew outdated ;}
function bs { brew search "$@" ;}
function bser { brew services "$@" ;}
function bu {
    brew update
    
    if [[ "$I_HAVE_ADMIN_POWERS" ]]; then
        brew upgrade
    else
        brew upgrade --formula \
            && brew outdated
    fi
}

function buc {
    (brew update && brew upgrade --formula) \
        && (brew cleanup; brew doctor)
}

function brew-lock {
    local HOMEBREW_LOCK_DIR USAGE
    HOMEBREW_LOCK_DIR="$HOMEBREW_ROOT/var/homebrew/locks"
    USAGE="Usage: brew-lock [--remove-all]"

    if [[ $# -eq 0 ]]; then
        ls "$HOMEBREW_LOCK_DIR"
    elif [[ $# -eq 1 ]]; then
        if [[ $1 = "--remove-all" ]]; then
            brew cleanup
        else
            echo "brew-lock: error: unrecognized argument '$1'. $USAGE"
        fi
    else
        echo "brew-lock: error: too many arguments. $USAGE"        
    fi
}

# brew-n-deps PACKAGE_NAME - print the number of packages
# that depend on PACKAGE_NAME
function _brew-n-deps {
    PACKAGE_NAME="$1"
    if ! brew ls --formula "$PACKAGE_NAME" >/dev/null 2>&1; then
        echo -1
    else
        RAW_COUNT=$(brew deps --tree --installed | command grep -c "$PACKAGE_NAME")
        N_DEPENDERS=$(( RAW_COUNT - 1 ))
        echo $N_DEPENDERS
    fi
}

# Just a verbose wrapper to _brew-n-deps, which see
function brew-n-deps {
    PACKAGE_NAME="$1"
    N_DEPENDERS=$(_brew-n-deps "$PACKAGE_NAME")
    if [[ "$N_DEPENDERS" = -1 ]]; then
        echo "Package '$PACKAGE_NAME' is installed"
    elif [[ "$N_DEPENDERS" = 0 ]]; then
        echo "Package '$PACKAGE_NAME' has no dependencies"
    elif [[ "$N_DEPENDERS" = 1 ]]; then
        echo "Package '$PACKAGE_NAME' has $N_DEPENDERS dependency"
    else
        echo "Package '$PACKAGE_NAME' has $N_DEPENDERS dependencies"
    fi
}

## Homebrewed alternatives to bundled system utilities
MY_LS_BASE_OPTIONS="--color=always -1F"
if test -e "$HOMEBREW_BIN/gls"; then
    export MY_LS=gls
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS --group-directories-first"
else
    export MY_LS=ls
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS"
fi

#### Other macOS-specific settings

function mac-globalize-fonts {
    local TERMINAL_APP_DIR
    if [[ -d "/System/Applications/Utilities/Terminal.app" ]]; then
        TERMINAL_APP_DIR="/System/Applications/Utilities/Terminal.app"
    else
        TERMINAL_APP_DIR="/Applications/Utilities/Terminal.app"
    fi

    sudo cp -R \
        "$TERMINAL_APP_DIR/Contents/Resources/Fonts" \
        "/Library/Fonts/"
}

function mac-disable-font-smoothing {
    defaults write -g AppleFontSmoothing -int 0
    defaults write CGFontRenderingFontSmoothingDisabled -INT 1
}

function mac-do-not-switch-to--desktop-when-switching-to-app { 
    sudo defaults write com.apple.dock workspaces-auto-swoosh -bool NO
    sudo killall Dock
}

function mac-enable-cmd-ctrl-click-to-drag-windows {
    defaults write -g NSWindowShouldDragOnGesture -bool true
}

#### Settings for bat, a colorizing cat(1) that understands many file formats

export BAT_THEME='base16'
function bat {
    command bat \
        --color always \
        --decorations='auto' \
        --style=rule,snip \
        --wrap=auto \
        "$@" \
      | $PAGER
}

# func - show the source code of a bash function, with syntax highlighting
function func {
    # check-args 1 1 "type SOME_FUNCTION_NAME" "$@"
    local FUNCTION_NAME="$1"
    local FIRST_LINE="$(command type "$FUNCTION_NAME" | head -1)"
    if [[ "$FIRST_LINE" = "$FUNCTION_NAME is a function" ]]; then
        command type "$FUNCTION_NAME" | tail -n +2 | bat --language bash
    else
        echo "$FUNCTION_NAME is not defined as a function" >&2
        return 1
    fi
}

# bat-head FILENAME NUMBER: use color bat to show a file's first n lines
function bat-head {
    local FILENAME="$1"
    local NUMBER="$2"
    
    bat --line-range ":${NUMBER}" "$FILENAME"
}

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub)
function bbat {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
}

# cdr ("CD Realname") - cd to the directory that a symlink points to
function cdr {
    # check-args 1 1
    local TARGET="$1"
    local DEPTH=0
    local MAX_DEPTH=9
    while true; do
        if [[ $DEPTH -ge $((MAX_DEPTH - 1)) ]]; then
            complain "Giving up after following $MAX_DEPTH levels of symlinks"
            complain "(do you have a circular reference?)"
            return 1
        elif [[ -L "$TARGET" ]]; then
            TARGET="$(realpath "$TARGET")"
            REALPATH_EXIT_STATUS=$?
            if [[ $REALPATH_EXIT_STATUS -ne 0 ]]; then
                return $REALPATH_EXIT_STATUS
            else
                DEPTH=$((DEPTH + 1))
                continue
            fi
        else
            cd "$TARGET"
            return
        fi
    done
}

# Report symlinks as symlinks
function file { command file --no-dereference "$@" ;}

# path - given a relative path, print its relative path. Does not dereference symlinks. Does not work on absolute paths; hence does not work for tilde expansion.
function path {
    local REL_PATH ABS_PATH
    REL_PATH="$1"
    ABS_PATH="$(pwd)/$REL_PATH"

    if [[ ! -e "$ABS_PATH" ]]; then
        echo "Error: file '$ABS_PATH' does not exist"
    else
        echo "$ABS_PATH" | tee >(pbcopy)
    fi
}

# qp ("qdw-dotfiles Push")
function qp { cdq && git add . && git commit && git push && cd - ;}

#### ls settings

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;91;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=39;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:'

function ls {
    if [[ "$PWD" = "$HOME" ]] && [[ $# -eq 0 ]]; then
        # By default, don't show dotfiles in my home dir...
        command $MY_LS $MY_LS_OPTIONS "$@" | $PAGER
    else
        # ... but do show them everywhere else.
        command $MY_LS $MY_LS_OPTIONS -A "$@" | $PAGER
    fi
}

function lsd {
    ls ~/d/
}

function recent {
    ls -t "$@" | head -5
}

#### diff settings

function dcc { colordiff -u "$@" | $PAGER ;}

HOMEBREW_DIFF="$HOMEBREW_ROOT/opt/diffutils/bin/diff"
if [[ -f "$HOMEBREW_DIFF" ]]; then
    WE_HAVE_GNU_DIFF=0
    export PATH="/opt/homebrew/opt/diffutils/bin:$PATH"
    # 
    # Use custom colors, which are supported by GNU grep but not by macOS grep:
    # 
    # ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
    # de - deleted lines (31 is SGR red, which shows as orange in my terminal)
    # hd - header        (35 is SGR magenta, which shows as magenta in my term)
    # ln - line numbers  (34 is SGR blue, which shows as blue in my term)
    # rs - undocumented, but the default seems fine
    # 
    MY_GNU_DIFF_OPTIONS='--palette="ad=36:de=31:hd=35:ln=34:rs=0"'
    function gdiff { /opt/homebrew/opt/diffutils/bin/diff "$@" ;}
    function diff-gnu { gdiff "$@" ;}
else
    WE_HAVE_GNU_DIFF=1
fi


function diff-vanilla {
    local COMMON_DIFF_OPTIONS="-u --color=always"
    if $WE_HAVE_GNU_DIFF; then
        command diff $MY_GNU_DIFF_OPTIONS $COMMON_DIFF_OPTIONS "$@" | $PAGER
    else
        command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
    fi
}


function diff-cdk-code {
    diff-vanilla -Nur \
        --exclude 'node_modules' \
        --exclude 'package*.json' \
        --exclude 'cdk.json' \
        --exclude 'cdk.out' \
        --exclude 'CODEOWNERS' \
        --exclude 'initialize-repo.sh' \
        --exclude '.gitignore' \
        --exclude '.husky' \
        --exclude '.eslintrc.yml' \
        --exclude '.mergify.yml' \
        --exclude '.npmignore' \
        --exclude '.tool-versions' \
        --exclude 'README.md' \
        --exclude 'tsconfig.json' \
        --exclude '*example*' \
     "$@"
}


function diff-git {
    ####### Git diff ##############################
    git diff --no-index "$@"
    
    ####### Other diff tools ######################
    # local COMMON_DIFF_OPTIONS="-u --color=always"
    # 
    ####### GNU diff ##############################
    # 
    # Color legend:
    # ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
    # de - deleted lines (31 is SGR red, which shows as orange in my terminal)
    # hd - header        (35 is SGR magenta, which shows as magenta in my term)
    # ln - line numbers  (34 is SGR blue, which shows as blue in my term)
    # rs - undocumented, but the default seems fine
    # 
    # command diff $COMMON_DIFF_OPTIONS --palette="ad=36:de=31:hd=35:ln=34:rs=0" "$@" | $PAGER
    # 
    ####### macOS diff ############################
    #
    # command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
    #
    ###############################################
}

GIT_MERGE_HELP_URL="https://howchoo.com/git/git-merge-conflicts-rebase-ours-theirs"

function ours {
    open -u "$GIT_MERGE_HELP_URL"
}

function theirs {
    ours "$@"
}

function diff {
    diff-git "$@"
}

#### FTP settings

set -a
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=""
set +a

#### Git setttings

function git-latest-commits {
    (
        for DIR in "$@"; do
            cd "$DIR" >/dev/null
            echo "$(git log -n 1 --format=%cd --date=iso)  $DIR"
            cd - >/dev/null
        done
    ) \
    | sort --reverse
}

function git-show-all-commits-of {
    check-args 1 1 'show-all-commits FILE' "$@" || return $?    
    local FILE="$1"
    
    for COMMIT in $(git rev-list HEAD); do
        color-printf "red" "Commit $COMMIT"
        GIT_PAGER="cat" git show "$COMMIT:$FILE"
    done
}

function for-all-commits {
    check-args 1 1 'for-all commits COMMAND_STRING' "$@" || return 1
    CMD="$1"

    for COMMIT in $(git rev-list HEAD); do
        color-printf "red" "Commit $COMMIT"
        eval "$CMD"
    done
}

function git-show-name-only-diffs {
    for-all-commits 'git diff --name-only --color=always $COMMIT HEAD'
}

## General Git settings (not specific to GitHub)

BREW_GIT_D="/opt/homebrew/bin"
if [[ -d "$BREW_GIT_D" ]]; then
    export PATH="$BREW_GIT_D:$PATH"
fi

export GIT_SHELL_PATH="$HOMEBREW_BIN/bash"

function git-toplevel {
    if ! check-args 0 0 'git-toplevel' "$@"; then
        return 1
    elif ! git rev-parse --is-inside-work-tree >/dev/null; then
        return 2
    else
        git rev-parse --show-toplevel
    fi
}

function branch-name {
    local PHRASE_STRING
    if [[ $# -eq 1 ]]; then
        PHRASE="$1"
        kebab-case "$(strip-punctuation "$PHRASE")"
    else
        complain "branch-name: error: wrong number of arguments. Usage: branch-name 'Some (explanatory) phase'"
    fi
}

function main {
    ~/.gitconfig-helper--get-main-branch-name "$@"
}

function popb {
    command popb --quiet "$@"
}

function pushb {    
    if [[ $# -eq 0 || ($# -eq 1 && ("$1" = "-l" || "$1" = "--list")) ]]; then
        command popb --list
    else
        command pushb --quiet "$@"
    fi
}

function strip-punctuation {
    local INPUT_STRING

    if [[ $# -eq 1 ]]; then
        INPUT_STRING="$1"
        echo "$INPUT_STRING" | tr -d '[:punct:]'
    else
        complain "strip-punctuation: error: wrong number of arguments. Usage: strip-punctuation 'my string'"
    fi
}

# kebab-case convert "A Phrase with mixed Case" to "a-phrase-with-mixed-case"
# (useful for generating git branch names)
function kebab-case {
    local INPUT_STRING

    if [[ $# -eq 1 ]]; then
        INPUT_STRING="$1"
        echo "$INPUT_STRING" | tr '[:upper:]' '[:lower:]' | sed 's/ /-/g'
    else
        complain "kebab-case: error: wrong number of arguments. Usage: kebab-case 'My input String'"
    fi   
}

function lisp-case { kebab-case "$@" ;}

# gcd ("Git CD") - cd to a directory, relative to the root dir of this repo
# For example, if you're in directory ~/src/my-project/javascript then
# 'gcd python' will change the cwd to ~/src/my-project/python
function gcd {
    check-args 0 1 'gcd [RELATIVE_DIR]' "$@" || return 1
    local DIR="$(git-toplevel)"
    if [[ $? -ne 0 ]]; then
        return "$?"
    else
        echo "$DIR"
    fi
}

# function git-delete-this-branch {
#     OLD_BRANCH=$(git branch --show-current)
#     # delete_it {
#     #     git branch -d "$OLD_BRANCH"
#     # }
    
#     if [[ "$OLD_BRANCH" = "main" ]] || [[ "$OLD_BRANCH" = "master" ]]; then
#         echo "Can't delete ${OLD_BRANCH}!"
#     else
#         git main
#         git ff
#         if [[ "$1" = "-y" ]] || [[ "$1" = "--yes"]]; then
#             delete_it
#         else
#             echo "Delete ${OLD_BRANCH}? (y/N)"
#             read REALLY_DELETE
#             if [[ $REALLY_DELETE = 'y' ]] || [[ $REALLY_DELETE = 'Y' ]]; then
#                 delete_it
#             fi
#         fi
#     fi
# }

# gr (git repo) - print repo name (if your cwd is in a git working copy)
function gr {
    basename "$(git rev-parse --show-toplevel)"
}

function git-backup-helper {
    local REPO_NAME ORIG_DIR EXIT_STATUS
    if [[ $# -ne 1 ]]; then
        echo "git-backup: error: missing argument. Usage: git-backup REPO_NAME"
    else
        REPO_NAME="$1"
        ORIG_DIR=$(pwd)
        cd ~/src \
        && rsync -qa \
            "$REPO_NAME" \
            "$HOME/git-backups/${REPO_NAME}-$(date +%Y-%m-%d-%H%M%S%z)"

        cd "$ORIG_DIR"
    fi
}


function git-backup {
    check-args 0 1  'git-backup [REPO_NAME]'  "$@" || return 1
    if [[ $# -eq 1 ]]; then
        git-backup-helper "$1"
    elif [[ $# -eq 0 ]]; then
        local GIT_URL="$(gurl)"
        if [[ "$GIT_URL" ]]; then
            local REPO="${GIT_URL##*/}"
            git-backup-helper "$REPO"
        else
            carp "couldn't determine which repo to operate on"
            return 2
        fi
    fi
}


function get-main-branch-name {
    local MAIN_OR_MASTER
    
    MAIN_OR_MASTER="$(command git branch --remotes | command ggrep --perl-regexp --only-matching '  origin/HEAD -> origin/(?=master|main)\K.*$' | sort | head -1)"
    echo "$MAIN_OR_MASTER"
}

# gbs (Git Branches) - list (local) git branches
function gbs { git branch ;}

# For any subdirs of . that are git working copies,
# show my local branches.
function gbss {
    local ORIG_DIR="$(pwd)"
    for DIR in $(fdd --max-depth 1 --type d); do
        if [[ -d "${DIR}.git" ]] && cd "$DIR" >/dev/null; then
             echo "$(color "$DIR" "blue")"
             gbs
             echo
        fi
        cd "$ORIG_DIR" >/dev/null    
    done
}

# gu (Git URL) - run this in any dir inside a Git working copy to show the URL
function gurl {
    local ORIG_DIR="$(pwd)"
    command grep url "$(git-toplevel)/.git/config" | sed -e 's/^[ \t]*url[ ][=][ ]//'
}

# branchify - munge a branch description into a branch name
# (by translating spaces to dashes and uppercase to lowercase)
function branchify {
    if [[ $# -ne 1 ]]; then
        echo "branchify: error: wrong number of arguments. Usage: branchify DESCRIPTIVE_STRING"
    else
        echo "$1" | tr '[:upper:]' '[:lower:]' | tr ' ' '-'
    fi
}

## GitHub-specific Git settings

function gclone {
    check-args 1 1 'URL' "$@" || return 1
    local URL="$1"
    
    local SSH_REGEX="^git[@]"
    local HTTPS_REGEX="^https://"
    
    if [[ "$URL" =~ $SSH_REGEX ]]; then
        echo "$URL"
    elif [[ "$URL" =~ $HTTPS_REGEX ]]; then
        git-https-to-ssh "$URL"
    else
        carp "Unrecognized URL format in '$URL'"
        return 2
    fi
}

function git-https-to-ssh {
    check-args 1 1 'HTTPS_URL' "$@" || return 1
    local HTTPS_URL="$1"
    echo "$HTTPS_URL" | sed -e s'|^https://|git@github.com:|'
}

# branch BRANCH_NAME - display the GitHub web page for BRANCH_NAME
function all_gists {
    gists --limit -1
}

function branch {
    local BRANCH_NAME
    
    if [[ $# -eq 0 ]]; then
        BRANCH_NAME="$("gb")"
    elif [[ $# -eq 1 ]]; then
        BRANCH_NAME="$1"
    else
        echo "branch: error: too many arguments. Usage: branch [BRANCH_NAME]" >&2
    fi
    
    gh browse --branch "$BRANCH_NAME"
}

function gists {
    gh git list "$@"
}

function team {
    # check-args 1 1
    local TEAM_NAME="$1"
    open -u "https://github.com/orgs/$GITHUB_ORG/teams/$TEAM_NAME"
}

function owner_echo {
    local OWNER="$(head -1 CODEOWNERS | sed 's/^[*] [@]//')"
    echo "$OWNER"
}

function owner {
    local TEAM_NAME="$(owner_echo | sed s"|^$GITHUB_ORG[/]||")"
    team "$TEAM_NAME"
}

# gb (Git Branch) - create a new branch based on the main one, and check it out
function gb {
    local NEW_BRANCH_NAME MAIN

    if [[ $# -eq 0 ]]; then
        git branch --show-current
    elif [[ $# -eq 1 ]]; then
        NEW_BRANCH_NAME="$1"
        MAIN=$(get-main-branch-name) \
            && git co --quiet "$MAIN" \
            && git pull --ff-only --quiet \
            && git checkout -b "$NEW_BRANCH_NAME" "$MAIN" \
                | command grep -vE 'remote:' #\
            #&& git push --set-upstream origin # --quiet
    else
        echo "gb: error: invalid number of arguments '$#'. Usage: gb | gb NEW_BRANCH_NAME"
    fi
}

# Generate a 
function gbb {
    local SHORTNAME

    echo "First create a JIRA ticket and copy its shortname (e.g., 'MYPROJ-1')"
    jira-create-ticket

    echo "Now paste in the shortname:"
    read SHORTNAME
    
    echo "Now paste in a brief description in English"
    read DESCRIPTION
    DESC="$(branchify "$DESCRIPTION")"
    
    NEW_BRANCH_NAME="$GITHUB_USERNAME/$SHORTNAME/$DESC"
    
    echo "New branch is '$NEW_BRANCH_NAME'"    
    # gb "$NEW_BRANCH_NAME"
}   

# gb (Branch Create) - alias for gb, mnemonic because "b c" is the Magit
# command to create and switch to a branch.
# Shadows the crappy Unix built-in calculator bc(1).
function bc {
    gb "$@"
}

# gist - view gists or create a gist
function gist {
    local DESCRIPTION
    local FILENAME="$(mktemp "$HOME/tmp")"

    if [[ $# -gt 2 ]]; then
        complain "gist: error: wrong number of arguments."
        complain "Usage:
                     gist DESCRIPTION # invokes GIT_EDITOR
                     gist ???"
        return
    elif [[ $# -eq 0 ]]; then
        local GIST_EDITOR
        for EDITOR_VAR in GIT_EDITOR VISUAL EDITOR; do
            if [[ -n "$EDITOR_VAR" ]]; then
                GIST_EDITOR="$EDITOR_VAR"
                break
            fi
        done
        if [[ -z "$EDITOR_VAR" ]]; then
            GIST_EDITOR=vi
        fi
        "$GIST_EDITOR" "$FILENAME"        
    elif [[ $# -eq 1 ]]; then
        # Assume we were invoked like this:
        #     some_command | gist DESCRIPTION
        DESCRIPTION="$1"
        local PIPED_INPUT="$(cat)"
        echo "$PIPED_INPUT" > "$FILENAME"
    elif [[ $# -eq 2 ]]; then
        DESCRIPTION="$1"
        FILENAME="$2"
    fi
    
    local DESCRIPTION_CLAUSE
    if [[ -n "$DESCRIPTION" ]]; then
        DESCRIPTION_CLAUSE="--desc '$DESCRIPTION'"
    fi
    
    gh gist create --web --filename "$FILENAME" "$DESCRIPTION_CLAUSE"
}


# gtr (Git Team and Repo) - run this in any dir inside a working copy of a GitHub repo to show the team name plus repo name in GitHub notation, e.g., myorg/myrepo
function gtr {
    local REPO_URL RETVAL
    REPO_URL="$(gu)"
    if [[ "$REPO_URL" =~ ^git[@]github.com[:](.*[/].*) ]]; then
        RETVAL=${BASH_REMATCH[1]}
        echo "$RETVAL"
    else
        echo "gtr: error: couldn't parse URL '$REPO_URL'" 2> /dev/null
    fi
}   

# gline (Git Line) - open a file in GitHub's web UI, at a given line
function gline {
    local FILENAME_AND_LINE  USAGE  BRANCH
    USAGE="Usage: gline FILENAME:LINENO"
    BRANCH="$(gb)"
    
    if [[ $# -eq 1 ]]; then
        FILENAME_AND_LINE="$1"
    elif [[ $# -eq 2 ]]; then
        FILENAME_AND_LINE="${1}:${2}"
    else
        echo "gline: error: wrong number of arguments ($#). $USAGE"
    fi
    
    if [[ -n "$FILENAME_AND_LINE" ]]; then
        gh browse --branch "$BRANCH" "$FILENAME_AND_LINE"
    fi
}

function lst {
    local LAST_ARG_IS_THE_FILENAME

    if [[ $# -eq 0 ]]; then
        git diff HEAD^ HEAD
    else
        local LAST_ARG_IS_THE_FILENAME

        for ARG in "$@"; do
            LAST_ARG_IS_THE_FILENAME="$ARG"
        done

        # Set $@ to a an array slice
        # that includies all the args
        # except the last one ($# - 1)
        set -- "${@:1:${#-1}}"

        git diff "$@" HEAD^ HEAD -- "$LAST_ARG_IS_THE_FILENAME"
    fi
}

function readme {
    gh repo view
}

# gcl (Git CLone) - quickly clone a repo belonging to my organization
function gcl {
    REPO="$1"
    cd ~/src >/dev/null \
        && git clone "git@github.com:${GITHUB_ORG}/${REPO}" "$@"
    cd - >/dev/null
}

function clone { gcl "$@" ;}
function gdo { git diff origin HEAD ;}
function gn { git diff --name-only "$@" ;}
function gs { git status "$@" ;}

# function mma {
#     git add . \
#         && git commit --allow-empty-message -m "" \
#         && git push \
#         && prl
# }

# gi ("git init") - go through the multi-command dance to create a local git repo
function gi {
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "gi: error: $(pwd) is already under git control"
    else
        git init \
            && git add . \
            && git commit -a -m 'Initial commit' \
            && git branch -m master main 2>/dev/null
    fi
}

function gl {
    local N_COMMITS

    if [[ $# -eq 0 ]]; then
        N_COMMITS=3
    elif [[ $# -eq 1 ]]; then
        N_COMMITS="$1"
        shift 1
    elif [[ $# -eq 2 ]] && [[ $1 = "-n" ]]; then
        N_COMMITS="$2"
        shift 2
    fi

    git log -n "$N_COMMITS" "$@"
}


# repos - multitool for listing/searching GitHub repositories 
function repos {
    local PATTERN VERBOSE

    if [[ ! "$GITHUB_ORG" ]]; then
        complain "repos: error: env var GITHUB_ORG is not set. Set it to your team name"
        return
    fi
        
    if [[ $# -eq 0 ]]; then
        PATTERN="." # match anything, i.e. print all repos in $GITHUB_ORG
    else
        PATTERN="$1"
        VERBOSE="$2" # may be the empty string
    fi
    
    function verbose-print {
        if [[ "$VERBOSE" ]]; then
            echo "$@" >&2
        fi
    }
    
    # Search in various places for repos matching PATTERN,
    # stopping at the first place where we find a match.
    set -o pipefail
    {
        verbose-print "searching $GITHUB_ORG repos"
        gh repo list --limit 2000 "$GITHUB_ORG" | command grep -E "$PATTERN"
    } \
    || \
    {
        verbose-print "searching personal repos"
        gh repo list --limit 2000 | command grep -E "$PATTERN"
    } \
    || \
    {
        verbose-print "opening a browser and searching all repos on github.com"
        open "https://github.com/search?type=repositories&q=${PATTERN}"
    }
}

# repo - open a named GitHub named repo in a web browser
function repo {
    REPO_NAME="$1"
    if [[ -z "$REPO_NAME" ]]; then
       gh repo view --web
    elif [[ "$REPO_NAME" = "list" ]]; then
        repos "."
    else
        # Try opening a repo of this exact name,
        # first in my org and then among repos owned by by GitHub username.
        #
        # If that fails, then do some fuzzy searching.
        if     ! gh repo view --web "$GITHUB_ORG/$REPO_NAME" >/dev/null 2>&1 \
            && ! gh repo view --web "$REPO_NAME" >/dev/null 2>&1; then
            
            echo "repo: couldn't find a $GITHUB_ORG repo named '$REPO_NAME'; searching for similarly-named repos" >&2
            repos "$REPO_NAME" true 1>&2 
        fi
    fi
}

function actions {
    local REPO_URL="$(gurl)"
    REPO_FOUND=$?
    if [[ $REPO_FOUND -ne 0 ]]; then
        return $REPO_FOUND
    else
        HTTPS_REPO_URL="$(echo "$REPO_URL" | sed -e 's|:|/|' -e 's|git[@]|https://|' -e 's|[.]git$||')"
        open -u "$HTTPS_REPO_URL/actions"
    fi
}

function act {
    actions "$@"
}

# PR (Pull Request) - manipulate GitHub pull requests
# Depends on the official GitHub CLI tool, 'gh'.
# Usage:
#
#     pr        # Show my pull requests (for the repo whose directory I'm in).
#     pr v      # Same thing.
#     pr o      # Same thing.
#     pr view   # Same thing.
#
#     pr cr     # Create a PR, pushing first if necessary.
#     pr create # Same thing.
#
#     pr cro    # Create a PR as above, and open it in a browser
#               #   so that I can watch its deploy scripts run.
#     pr crv    # Same thing.
#
# PR (Pull Request) - manipulate GitHub pull requests

# prl ("Pull Request: Latest"): open my most recent PR from this repo, in a web browser
function prl {    
    local URL="$(gh pr list --author "$GITHUB_USERNAME" --json url --jq '.[0].url')"
    if [[ -z "$URL" ]]; then
        complain "Looks like you don't have any PRs open in this repo"
    else
        open -u "$URL"
    fi
}

function pr {
    function list_my_prs {
        gh pr list --author "$GITHUB_USERNAME" "$@"
    }

    function push_it {
        # Push to a remote branch (creating if necessary)
        local BRANCH
        BRANCH="$(git branch --show-current)"
        git push --set-upstream origin "$BRANCH" --quiet
    }

    function create_pr {
        local GIT_FETCH_STDOUTERR=$(git fetch 2>&1)
        if [[ "$GIT_FETCH_STDOUTERR" ]]; then
            # If there's stuff to merge, cowardly bail
            # and make the user merge it manually
            echo "$GIT_FETCH_STDOUTERR"
        else
            push_it
            
            local URL=""
            if [[ -n "$PR_TITLE" ]]; then
                # URL=$(gh pr create --title "$PR_TITLE" 2>&1 | tail -1)
                gh pr create --title "$PR_TITLE"
            else
                # URL=$(gh pr create --fill 2>&1 | tail -1)
                gh pr create --fill
            fi
            list_my_prs
        fi
    }
    
    function main {
        local PR_NUMBER_REGEX='^[[:digit:]]+$'
        
        if [[ "$1" = "--list" ]]; then
            shift
            list_my_prs
        elif [[ "$1" =~ $PR_NUMBER_REGEX ]]; then
            gh pr view --web "$1"
        else
            URL="$(create_pr "$@")"
            # open "$URL"
        fi
    }

    main "$@"
}

function prs {
    pr --list
}

# purl ("Pull request URL") - print the URL of the latest pull request on this branch.
# Also copy it to the clipboard.
function purl {
    gh pr view | command grep --color=never --only-matching 'https://.*' | tee >(pbcopy)
}

function pulls {
    open "https://github.com/pulls"
}

#### Kubernetes (and OpenShift) settings

function oc {
    if [[ "$1" = "login" ]] && [[ $# -eq 1 ]]; then
        local AUTH_URL
        AUTH_URL=$(command oc login | sed s'/.*\(https.*\)/\1/')
        if [[ $? -eq 0 ]]; then
            open "$AUTH_URL"
        fi
    else
        command oc "$@" | $PAGER
    fi
}

# Show all images in use in the current cluster; code ganked from https://kubernetes.io/docs/tasks/access-application-cluster/list-all-running-container-images/
function kube-ls-images { 
    kubectl get pods --all-namespaces \
                      -o jsonpath="{.items[*].spec.containers[*].image}" \
        | tr -s '[:space:]' '\n' | sort | uniq -c
}

# If I'm going to make the same typo all the time, I might as well autocorrect it
function lube-aws { kube-aws "$@" ;}
function lube-ls-images { kube-ls-images "$@" ;}
function lubectl { kubectl "$@" ;}
function lubecfg { kubecfg "$@" ;}
function minilube { minikube "$@" ;}

#### Perl settings

# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
export PERL_MM_USE_DEFAULT=1

# pb: because typing 'perlbrew' is too much work.
function pb {
    # Don't put this in PATH, because that would interfere with the system Perl.
    ~/perl5/perlbrew/bin/perlbrew "$@"
}

#### Playdate (handheld indie console) game dev settings

function pd_encode {
    local INPUT OUTPUT

    if ! [[ $# -eq 1 ]] || [[ $# -eq 2 ]]; then
        echo "Wrong number of arguments. Usage: pd_encode INPUT_FILENAME [OUTPUT_FILENAME"
    else
        INPUT="$1"
        OUTPUT=${2:-$INPUT.pd.wa}
        set -x
        ffmpeg -i "$INPUT" -acodec adpcm_ima_wav "$OUTPUT"
        set +x
    fi
}

#### Data risotto spatulae
function csview { command csview "$@" | $PAGER ;}

#### PostgreSQL settings
####
#### (for additional PostgreSQL settings, see the following files):
####     ~/.psqlrc*
####     ~/.pg_service
####     ~/.pgpass

function pg {
    local  VERSION  PROGRAM_NAME  BINARY_PATH  COMMAND
    
    if [[ "$1" ]]; then
        VERSION="$1"
        shift
    else
        echo "pg: error: missing argument. Usage: pg VERSION [COMMAND] [ARG ...]" >&2
        return
    fi

    if [[ "$1" ]]; then
        PROGRAM_NAME="$1"
        shift
    else
        PROGRAM_NAME="psql"
    fi
    
    BINARY_PATH=$(echo $HOMEBREW_ROOT/opt/postgresql@${VERSION}/bin/$PROGRAM_NAME)
    
    # subshell
    (
        export PGSERVICE="pg${VERSION}"
        "$BINARY_PATH" "$@"
    )
}

declare -Ax pgpasses

function pql {
    local SERVICE  PASSWORD
    if [[ $# -ne 1 ]]; then
        complain "pql: error: missing argument. Usage: pql pg_service.conf-stanza-name | psql --clear-cache"
        return 1
    else
        if [[ "$1" = "--clear-cache" ]]; then
            unset pgpasses
            return
        else
            SERVICE="$1";
            if [[ -n ${pgpasses["$SERVICE"]} ]]; then
                PASSWORD=${pgpasses["$SERVICE"]}
            else
                PASSWORD="$("$MY_VIRTUALENV_DIR/qql/bin/python" "$HOME/bin/site/pql" "$SERVICE")";
                if [[ $? -ne 0 ]]; then
                    complain "pql couldn't retrieve the AWS secret for $SERVICE" 1>&2;
                    return 2
                fi
            fi

            pgpasses["$SERVICE"]="$PASSWORD"
            echo "$PASSWORD"
        fi
    fi
}

function qql {
    local SERVICE PASSWORD
    
    if [[ $# -eq 0 ]]; then
        echo "qql: error: missing argument. Usage: qql pg_service.conf-stanza-name [PSQL_OPTION ...]" \
             >&2
    else
        SERVICE="$1"; shift
        
        if [[ -n ${pgpasses["$SERVICE"]} ]]; then
            PASSWORD=${pgpasses["$SERVICE"]}
        else
            PASSWORD="$("$MY_VIRTUALENV_DIR//qql/bin/python" "$HOME/bin/site/pql" "$SERVICE")"
            if [[ $? -ne 0 ]]; then
                echo "pql couldn't retrieve the AWS secret for $SERVICE" >&2
                return
            else
                pgpasses["$SERVICE"]="$PASSWORD"
            fi
        fi

        PGSERVICE="$SERVICE" PGPASSWORD="$PASSWORD" psql "$@"
    fi
}

# replication-role -  say whether this is a primary or a replica (suitable for inclusion in your shell and/or psql prompt)
function replication_role {
    if ! which psql; then
        echo ''
    else
        INR=$(psql -tc 'select pg_is_in_recovery()')
        if [[ $INR == ' f' ]]; then
            echo '*** PRIMARY ***'
        elif [[ $INR == ' t' ]]; then
            echo 'replica'
        else
            echo '[ERROR: COULD NOT DETERMINE REPLICATION ROLE]'
        fi
    fi
}

# View or edit my ~/.pg_service.conf
# Usage:
#
#     # To open the file in a text editor:
#     ser
#
#     # To show a [section] in the file that matches a regex:
#     ser monolith
#     # ^ prints all sections with section headings matching '[.*monolith.*]'
# 
# ser - display sections of my ~/.pg_service.conf, or the whole thing
function ser {
    local COLORIZE="bat --language ini"

    if [[ $# -eq 0 ]]; then
        $COLORIZE ~/.pg_service.conf
    else
        local SERVICE_NAME_SUBSTRING="$1"
        para "$SERVICE_NAME_SUBSTRING" ~/.pg_service.conf | $COLORIZE
    fi
}

#### IRL settings

## PurpleAir air quality monitor settings

function air {
    set -uo pipefail
    local RESPONSE_FILE
    RESPONSE_FILE=$(mktemp "$HOME/tmp/air.XXXX")

    curl -L 'https://www.purpleair.com/json?show=119745&key=NPLQPTWKC54229MU' \
         > "$RESPONSE_FILE" \
         2> /dev/null
         
    if [[ $? -ne 0 ]]; then    
        echo "Problem contacting the PurpleAir API server; maybe rate-limiting?"
        echo "Try waiting ten minutes and then re-running."
    else
        local PM25 STATS TSLM NUMBER_OF_MINUTES INFLECTED

        PM25=$(jq --raw-output '.results[0]."PM2_5Value"' < "$RESPONSE_FILE")
        STATS=$(jq --raw-output '.results[0].Stats' < "$RESPONSE_FILE")
        TSLM=$(echo "$STATS" | jq --raw-output '.timeSinceModified')

        # Convert milliseconds to minutes
        NUMBER_OF_MINUTES=$(( TSLM / 60 ))
        
        if [[ "$NUMBER_OF_MINUTES" -eq 1 ]]; then
            INFLECTED="minute"
        else
            INFLECTED="minutes"
        fi

        echo "PM 2.5 is ${PM25}ppm (as of $NUMBER_OF_MINUTES $INFLECTED ago)"
    fi
    
    set +uo pipefail
}

#### Convenience functions for working with local files

function buy {
    # check-args 0 1 "buy [PRODUCT_NAME]"
    local URL
    local URL_DIR="$HOME/.site.bash.d/buy"
    function list-things-you-can-buy {
        local OUTPUT_DEVICE="$1"
        echo "Things you can buy:" > "$OUTPUT_DEVICE"
        echo > "$OUTPUT_DEVICE"
        command ls -1 --color=never "$URL_DIR" > "$OUTPUT_DEVICE"
    }
    
    if [[ $# -eq 0 ]]; then
        list-things-you-can-buy /dev/stdout
    elif [[ $# -eq 1 ]]; then
        local PRODUCT_NAME="$1"
        local URL_FILE="$HOME/.site.bash.d/buy/$PRODUCT_NAME"
        if [[ ! -e "$URL_FILE" ]]; then
            complain "Don't know how to buy '$PRODUCT_NAME'"
            complain "(no such file '$URL_FILE')"
            list-things-you-can-buy /dev/stderr
            return 1
        else
            local URL="$(cat "$URL_FILE")"
            CAT_EXIT_STATUS=$?
            if [[ $CAT_EXIT_STATUS -ne 0 ]]; then
                return $CAT_EXIT_STATUS
            elif [[ -z "$URL" ]]; then
                complain "'$URL_FILE' is empty"
                return 254
            else
                open -a Firefox "$URL"
            fi
        fi
    fi
}

function gtd {
    echo '* TODO' "$@" >> ~/.site.bash.d/gtd/gtd.org
}

function inbox {
    gtd "$@"
}

function todo {
    gtd "$@"
}

function rtfm {
    check-args 0 1 'rtfm [TOPIC' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        command ls --color=never "$HOME/.site.bash.d/rtfm"
    elif [[ $# -eq 1 ]]; then
        cat "$HOME/.site.bash.d/rtfm/$1"
    fi
}

function wish () { 
    echo '* TODO' "$@" >> ~/.site.bash.d/wishlist/wishlist.org
}

#### Python settings

MY_VIRTUALENV_DIR=~/.virtualenvs--python3

# Homebrew stable Python version

set -a
PYTHON_INTERPRETER_PATH="/usr/local/bin"
PYTHON_PIPX_PATH=~/.local/bin
set +a

function python { python3 "$@" ;}
function pip { pip3 "$@" ;}

function va {
    local VENV_NAME
    if [[ $# -ne 1 ]]; then
        echo "va: error: wrong number of arguments. Usage: va VIRTUALENV_TO_ACTIVATE"
    else
        VENV_NAME="$1"
        my-source "$MY_VIRTUALENV_DIR/$VENV_NAME/bin/activate"
        pip install --upgrade pip
    fi
}

function mkvirtualenv {
    mkdir -p "$MY_VIRTUALENV_DIR"
    
    NEW_VIRTUALENV_NAME="$1"
    shift
    if [[ ! $NEW_VIRTUALENV_NAME ]]; then
        echo "Missing argument. Usage: mkvirtualenv [OPTIONS] NEW_VIRTUALENV_NAME"
    else
        NEW_VIRTUALENV_PATH="$MY_VIRTUALENV_DIR/$NEW_VIRTUALENV_NAME"
        python3 -m venv "$NEW_VIRTUALENV_PATH" \
            && va "$NEW_VIRTUALENV_NAME" \
            && pip install --upgrade pip
    fi
}
function ve { mkvirtualenv "$@" ;}

function venvs {
    ls "$MY_VIRTUALENV_DIR/"
}

#### Aranet4 air quality monitoring settings

function co2  {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read "$MY_ARANET4" 2> /dev/null | jq ".co2"
}

function co2-verbose {
    echo "CO2 was $(co2) ppm at $(date '+%I:%M %P') on $(d)"
}

#### Tor settings

function tor-curl { curl --socks4a 127.0.0.1:9050 "$@" ;}
function torcurl { tor-curl "$@" ;}

#### TypeScript settings/Deno settings

# (You can't do this with a shebang, hence the need for a function):
# dr ("Deno Run") - run some code via the Deno runtime.
function dr { deno run --quiet "$@" ;}

# dl ("Deno with Local permissions") - run some code in the Deno runtime, and allow the code to access the network and read the local filesystem, without prompting me for permission.
function dl { deno run --quiet --allow-net --allow-read "$@" ;}

function dc { deno eval --check "$@" ;}

# dcr - run Deno, but first do type-checking (no longer the default as of Deno v1.23)
function dcr { deno run --check "$@" ;}

#### Rust settings

export RUST_PATH=~/.cargo/bin

#### Ruby settings

# Use homebrewed Ruby if installed
if [[ -d "$HOMEBREW_ROOT/opt/ruby" ]]; then
    set -a
    RUBYOPT=rubygems
    RUBY_PATH=/usr/local/opt/ruby/bin
    RUBY_LDFLAGS='-L/usr/local/opt/ruby/lib'
    RUBY_CPPFLAGS='-I/usr/local/opt/ruby/include'
    set +a
fi

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
function gemi {
    sudo gem install --remote \
                     --rdoc \
                     --ri \
                     "$@"
}

#### Miscellaneous shell functions

# Change this terminal's background to the hex triplet passed in.
function bghex {
    # Example: 'bghex ff0000' to use a red background.
    printf '\e]11;%s\a' "#${1}"
}

function ips { /sbin/ifconfig -a | command grep 'inet ' | command grep -v 'inet6 ' | grep 'inet' ;}
function ipv6 { /sbin/ifconfig -a | grep 'inet6 ' ;}

# flast - print the first and last lines of a file
function flast {
    local FILENAME
    FILENAME="$1"

    head -1 "$FILENAME" | awk '{ print $1, $2; }'
    tail -1 "$FILENAME" | awk '{ print $1, $2; }'
}

# line N FILE - show line N of file FILE
function line {
    local LINE_NUMBER FILENAME
    LINE_NUMBER="$1"
    FILENAME="$2"

    head "-${LINE_NUMBER}" "$FILENAME" | tail -1
}

function mcd {
    local DIRNAME
    DIRNAME="$1"
    mkdir "$DIRNAME" \
        && cd "$DIRNAME"
}

# mhost ("Map to Host(s)") - repeatedly execute a command on a list of hostnames (passing in each hostname as the command's last arg)
function mhost {
    COMMAND="$1"; shift
    for NODE in "$@"; do
        echo "$NODE"
        echo ==
        echo
        "$NODE" "$COMMAND"
        echo
    done
}

# mshost ("Map Ssh [to] Host(s)") - execute an ssh command on a list of hosts, with paragraphized output
function mshost {
    REMOTE_COMMAND="$1"; shift
    COMMAND="ssh $REMOTE_COMMAND"
    mhost "$COMMAND" "$@"
}

#### Process settings

# Display all processes, in a style that I like. If an argument is
# given, interpret it as a grep regex, and display only those that
# match that regex. Yeah, I know I could do this with pgrep, but it
# wouldn't give me all the output-format flexibility that I get from
# using ps itself.
# function p {
#     local HEADER
#     local PROCS
#     local PS_COMMAND="ps auxwwwww"
    
#     if [[ $# -eq 0 ]]; then
#         $PS_COMMAND | $PAGER
#     else
#         HEADER="$($PS_COMMAND | head -1)"

#         set -o pipefail
#         PROCS="$($PS_COMMAND | command grep -v grep | command grep --color=always "$@")" | $PAGER
#         RC=$?
#         set +o pipefail
        
#         if [[ -n "$PROCS" ]]; then
#             echo "$HEADER"
#             echo "$PROCS"
#         fi

#         return $RC
#     fi
# }

function pstree {
    command pstree -g 3 -u $USER "$@" | $PAGER
}

# Display only processes that belong to me.
function pu { p "$USER" ;}

#### vim settings

# vl ("Vim Less") - use vim as a colorizing pager
function vl { "$HOMEBREW_ROOT/share/vim/vim91/macros/less.sh" "$@" ;}

function _mvim_helper {
    local ERROR_MESSAGE="$1"; shift
    
    THE_MVIM_IN_PATH="$(type -P mvim)"
    if [[ "$THE_MVIM_IN_PATH" ]]; then
        "$THE_MVIM_IN_PATH" "$@"
    elif [[ -f "$HOMEBREW_ROOT/opt/macvim/bin/mvim" ]]; then
        "$HOMEBREW_ROOT/opt/macvim/bin/mvim" "$@"
    else
        complain "$ERROR_MESSAGE"
    fi
}

function gvim {
    if which gvim >/dev/null; then
        command gvim "$@"
    else
        _mvim_helper "Looks like neither gvim nor mvim (MacVim) is installed" "$@"
    fi
}

function mvim {
    _mvim_helper "Looks like mvim (MacVim) isn't installed" "$@"
}

#### Set operations

## All these operators take a list of two or more files,
## where each file consists of one or more lines.
## Each file is treated as a set,
## and each line is treated as an element of that set.

## Totally broken: gives wrong results and fails to match OP

# function set-op {
#     local OP="$1"
#     shift
    
#     if [[ $# -ne 2 ]]; then
#         echo "set-op: error: expected 2 arguments but got $#"
#     else
#         case OP in
#             union | +)
#                 sort -u "$1" "$2" ;;
#             intersection | intersect | isect | ^)
#                 comm -12 <(sort "$1") <(sort "$2") ;;
#             difference | diff | subtraction | subtract | minus | -)
#                 comm -23 <(sort "$1") <(sort "$2") ;;
#             membership | in | E)
#                 grep -Fxq "$1" "$2" ;;
#             *)
#                 complain "unsupported set operation '$OP'"
#                 return 255 ;;
#         esac
#     fi
# }            

# function set-intersection {
#     if [[ $# -eq 2 ]]; then
#         comm -12 "$1" "$2"
#     else
#         complain "set-intersection: error: wrong number of arguments (got $#, expected 2)"
#     fi
# }


# function set-difference {
#     if [[ $# -eq 2 ]]; then
#         comm -23 "$1" "$2"
#     else
#         complain "set-difference: error: wrong number of arguments (got $#, expected 2)"
#     fi
# }

# function set-disjunctive-union { set-difference "$@" ;}


# function set-union {
#     if [[ $# -eq 2 ]]; then
#         cat "$1" "$2" | sort | uniq
#     else
#         complain "set-union: error: wrong number of arguments (got $#, expected 2)"
#     fi
# }

# function set-equals {

#### Slack settings

function slack-channel {
    # check-args 1 1 "slack-channel CHANNEL_NAME # (with or without the leading '#'; doesn't matter)"

    # Remove leading '#' if present
    local CHANNEL_NAME="${1/##/}"
    
    if on-macos; then
        macos-open-slack-channel.bash "$CHANNEL_NAME"
    else
        open "https://${SLACK_TEAM}.slack.com/channels/${CHANNEL_NAME}"
    fi
}
    

#### SSH settings

function ssh-keygen { command ssh-keygen -t ed25519 "$@" ;}

function ssh-add-all {
    for KEY in $(find ~/.ssh -maxdepth 1 -type f -name '*_ed25519'); do
        ssh-add "$KEY"
    done
}

# [FIXME: needs work] When I first ssh to a host, configure it by installing my public ssh keys, my favorite dotfiles, my favorite yum packages, et cetera.
function quinnify_host {
    local DOTFILES NODE PACKAGES

    # set -x

    if [[ ! $1 ]]; then
        echo "Error: missing argument. Usage: quinnify_host $HOSTNAME_OR_SSH_HOSTALIAS"
        return
    fi

    NODE="$1"

    DOTFILES='
        .bashrc.qweaver.bash
        .emacs
        .emacs.d
        .gitconfig
        .hushlogin
        .inputrc
        .psqlrc
        .screenrc
        .tmux.qweaver.conf
        .vimrc.qweaver
        .vim
    '

    PACKAGES='
        ack
        colordiff
        emacs-nox
        htop
        nc
        nmap
        pgcenter
        pv
        tmux
        wemux
    '

    ssh-copy-id -i ~/.ssh/qweaver_switchfly_3_id_ed25519 "$NODE" > /dev/null 2>&1

    rsync -rptgoDL ~/...sms-me "${NODE}:~/"

    #### Dotfiles.

    cd ~/dotfiles \
       && rsync -rptgoDL $DOTFILES ${NODE}:~/
    cd - > /dev/null

    ssh "$NODE" touch .quinnified

    # set +x
}

function my_ssh {
    NODE=$1

    if (! ssh "$NODE" test -e ~/.quinnified > /dev/null 2>&1); then
        quinnify_host "$NODE"
    fi

    ssh "$NODE"
}

# aa graceful|start|... -  because typing 'sudo apache2ctl' is too much work.
function aa { sudo apache2ctl "$@" ;}

function atom { newsboat "$@" ;}
function news { cast-text "$@" ;}

# cel FILE.el ("compile LISP") - byte-compile an emacs LISP file.
function cel { "$EMACS_BINARY" -nw -q -batch -f batch-byte-compile "$@" ;}

# d ("date") - give today's date in YYYY-MM-DD format.
function d { date +%Y-%m-%d ;}

function epoch2human {
    # check-args 1 1 "epoch2human EPOCH_SECONDS"
    local EPOCH_SECONDS="$1"
    date -d "@${EPOCH_SECONDS}"
}

function dos2unix { perl -pe 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream - run the Electric Sheep fractal generation daemon
function dream { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# dfr ("df real") - show all mounted devices that are actually disks (see also mr).
function dfr {
    df -h | head -1

    if [[ "$MY_OS" = "Darwin" ]]; then
        df -h | command grep -E '/System/Volumes/Data$'
    else
        df -h | command grep ^/
    fi
}

# ff [PROFILE_NAME] - run Firefox with a profile. No args? Open the dialog box to list/create profiles.
function ff {
    FIREFOX_EXECUTABLE="/Applications/Firefox.app/Contents/MacOS/firefox-bin"
    if [[ $# -eq 1 ]]; then
        # Run with a specific profile, named in $1.
        "$FIREFOX_EXECUTABLE" -ProfileManager "$1" & exit
    else
        # Run the Profile Manager (which allows you to view all profiles and
        # add a new one).
        "$FIREFOX_EXECUTABLE" -p & exit
    fi
}

function ho { command grep -E "Host .*${1}" ~/.ssh/*conf* ;}

# mr ("mount real") - show all mounted devices that are actually disks (see also dfr).
function mr {
    mount | head -1
    mount | command grep ^/
}

# lw Perl::Module, lw Perl/Module.pm ("library which") - show the path to Perl module in PERL5LIB.
function lw { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which") - find a perl module in PER5LIB and open it in VISUAL. #FIXME: test behavior.
# elw { $VISUAL $(lw "$@") ;}

# (See http://wiki.github.com/bard/mozrepl/)
# mz: Use mozrepl to connect to Firefox for some interactive debugging.
function mz { socat READLINE TCP4:localhost:4242 ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder. Fails if the directory contains anything besides .DS_Store.
function rmds {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
}

function rot13 {
    echo "$1" | tr 'A-Za-z' 'N-ZA-Mn-za-m'
}        

# Invoke strings(1) safely, because http://www.jwz.org/blog/2014/10/we-live-in-a-magical-future-where-strings-is-exploitable/
function strings { command strings -a "$@"  | $PAGER ;}

# para: print any paragraph contaning the desired string. A paragraph is defined as a collection of lines followed by an all-blank line (i.e., a newline).
function para {
    STRING="$1"; shift
    awk -v ORS="\n\n" -v RS='' "/$STRING/" "$@"
}

# Same thing, with line numbers, and a line with only whitespace also
# marks a paragraph boundary
function paraln {
    # check args 1 varargs
    local PARALN_REGEX="$1"; shift
    # awk "/$PARALN_REGEX/ {print NR \": \" \$0; f=1} f && NF {print NR \": \" \$0} /^$/ {f=0}" \
    awk "/$PARALN_REGEX/ {if (!f) {print NR \": \" \$0}; f=1} f && NF && !/$PARALN_REGEX/ {print NR \": \" \$0} /^$/ {f=0}"   "$@"
}

function sshconf { para "$1" ~/.ssh/*.config | command grep -e '^' -e "$1" ;}

# pi ("Perl @INC") - print @INC dirs, one per line, preceded by the path to Perl.
function pi {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

# shn ("SSH hostname") - print the canonical hostname for a ~/.ssh/config alias.
function shn {
    command grep -A 1 "$1" ~/.ssh/*config* | tail -1 | awk '{ print $3; }'
}

## tmux

function _tmux_is_new {
    local LEADING_DIGIT_OF_VERSION_NUMBER
    LEADING_DIGIT_OF_VERSION_NUMBER="$(tmux -V | sed 's/^tmux[ ]\([0123456789]\).*/\1/')"

    if [[ "$LEADING_DIGIT_OF_VERSION_NUMBER" -ge 3 ]]; then
        return 0
    elif [[ "$LEADING_DIGIT_OF_VERSION_NUMBER" -lt 3 ]]; then
        return 1
    else
        echo "_tmux_is_new: error: version number starts with invalid character '$LEADING_DIGIT_OF_VERSION_NUMBER'"
        return 2
    fi
}

# ta $SESSION_NAME ("Tmux Attach") - attach to named session, creating if needed
function ta {
    local SESSION_NAME TMUX_CONF
    
    if [[ $# -ne 1 ]]; then
        echo "ta: error: wrong number of arguments. Usage: ta SESSION_NAME"
    else
        SESSION_NAME="$1"

        if _tmux_is_new; then
            local TMUX_CONF=~/.tmux.new-versions.conf
        else
            local TMUX_CONF=~/.tmux.old-versions.conf
        fi

        if (! tmux has-session -t "$SESSION_NAME" 2> /dev/null); then
	    tn "$SESSION_NAME"
        else
            tmux -f "$TMUX_CONF" -2 attach-session -t "$SESSION_NAME"
        fi
    fi
}

# tn ("Tmux New") - create a new tmux session
function tn {
    local SESSION_NAME TMUX_CONF
    
    if [[ $# -ne 1 ]]; then
        echo "tn: error: wrong number of arguments. Usage: tn SESSION_NAME"
    else
        SESSION_NAME="$1"

        if _tmux_is_new; then
            TMUX_CONF=~/.tmux.new-versions.conf
        else
            TMUX_CONF=~/.tmux.old-versions.conf
        fi

        tmux -f "$TMUX_CONF" new-session -s "$SESSION_NAME"
    fi
}

# tl ("Tmux List") - list running tmux sessions.
function tl {
    if _tmux_is_new; then
        local TMUX_CONF=~/.tmux.new-versions.conf
    else
        local TMUX_CONF=~/.tmux.old-versions.conf
    fi

    tmux -f "$TMUX_CONF" list-sessions 2> /dev/null
}

function timestamp { date +%Y%m%dT%H%M%S%z ;}
function ts { timestamp "$@" ;}

# tor_wget URL1 [DEST_PATH]: get the contents of a URL, using Tor for anonymity.
function tor_wget {
    HTTP_PROXY=http://127.0.0.1:8118/ \
    http_proxy=$HTTP_PROXY \
      wget --mirror --convert-links --html-extension --domains "$1" "$2"
}

# tor_on: start proxying all this shell's HTTP traffic through Tor.
function tor_on { export HTTP_PROXY="http://127.0.0.1:8118/"; export http_proxy="$HTTP_PROXY" ;}

# tor_off: stop proxying this shell's HTTP traffic through Tor.
function tor_off { unset HTTP_PROXY http_proxy ;}

# unix2dos FILE1 ...: translate-in-place newlines to the DOS convention (CRLF).
function unix2dos { /usr/bin/perl -pi -e ' s{$}{\r}x; ' "$@" ;}

# unrar_plural FILE1 ...: because unraring files one by one is tiresome.
function unrar_plural { for f in *rar; do unrar x -o+ "$f" .; done ;}

# xb ("xbindkeys") - reload custom (xbindkeys) X11 keybindings.
function xb { killall --user "$USER" xbindkeys && xbindkeys ;}

# Make it so Ctrl-S and Ctrl-Q aren't interpreted as software flow control
# characters, messing up your terminal (see http://catern.com/posts/terminal_quirks.html).
# This also frees up Ctrl-Q for use as the tmux prefix key.
if (shopt -q login_shell   &&  tty > /dev/null 2>&1); then
    stty -ixon -ixoff > /dev/null 2>&1
fi

#### Perlbrew settings (this must come after the part in this file where PATH is set):

function activate-perlbrew { my-source ~/perl5/perlbrew/etc/bashrc ;}
function perlbrew-activate { activate-perlbrew ;}

function psql-tips { links ~/src/psql-tips/html/psql_tips_all.html ;}
function tips { psql-tips "$@" ;}
function mutt { neomutt "$@" ;}

#### Game development settings

function sd2lcompile {
    gcc -o "$@"  `sdl2-config --cflags --libs`
}
function sdd { sdl2compile "$@" ;}

#### Web settings

function brow { defaultbrowser "$@" ;}
function brc { brow "chrome" ;}
function brs { brow "safari" ;}

# Adapted from a function posted by user Chubler_XL, in a comment on
# https://www.unix.com/shell-programming-and-scripting/277083-url-encoding-string-using-sed.html

function urlencode {
    # URL encode string and optionally store in shell variable
    #
    # usage: url_var <string> [var]
    local LC_ALL=C
    local encoded=""
    local i c

    for (( i=0 ; i<${#1} ; i++ )); do
        c=${1:$i:1}
        case "$c" in
            [a-zA-Z0-9/_.~-] ) encoded="${encoded}$c" ;;
            * ) printf -v encoded "%s%%%02x" "$encoded" "'${c}" ;;
        esac
    done
    
    if [[ $# -gt 1 ]]; then
        printf -v "$2" "%s" "${encoded}"
    else
        printf "%s\n" "${encoded}"
    fi
}

function gmail-search {
    local QUERY  BASE_URL
    BASE_URL="https://mail.google.com/mail/u/0/#search"
    
    if [[ $# -ne 1 ]]; then
        echo "gmail-search: error: wrong number of arguments. Usage: gmail-search QUERY_STRING"
    else
        QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$QUERY")
        open -u "$BASE_URL/$URLENCODED_QUERY"
    fi
}

function gmail {
    if [[ $# -eq 0 ]]; then
        open -u "https://mail.google.com/"
    elif [[ $# -eq 1 ]]; then
        gmail-search "$1"
    else
        echo "gmail: error: wrong number of arguments. Usage: gmail [SEARCH_STRING]"
    fi
}

function google {
    local BASE_URL="http://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q="

    if [[ $# -ne 1 ]]; then
        echo "google: error: wrong number of arguments. Usage: google QUERY"
    else
        local INPUT_QUERY MASSAGED_QUERY
        INPUT_QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$INPUT_QUERY")
        open -u "${BASE_URL}${URLENCODED_QUERY}"
    fi
}

function goog {
    google "$@"
}

#### Java settings

export JPATH="/usr/local/opt/openjdk/bin"

#### JavaScript settings

export NODE_PATH="/opt/node/bin"
export NODE_MANPATH="/opt/node/share/man:/opt/node/lib/node_modules/npm/man"
MY_MANPATH="$NODE_MANPATH:$MY_MANPATH"

#### Jenkins settings

function jenkins {
    local  REPO  BRANCH  JENKINS_OUTPUT_FILENAME  JENKINS_OUTPUT_URL  DOCKER_IMAGE_URL  SHORT_MESSAGE  LOGFILE
    
    if [[ "$1" = "--no-sms" ]] || [[ "$JENKINS_NO_SMS" ]]; then
        function sms {
            : # no-op
        }
    else
        function sms {
            ~/...sms-me "$@"
        }
    fi

    REPO="$(jenkins-repo)"
    BRANCH="$(jenkins-branch)"
    
    if [[ -z "$JENKINS_BASE_URL" ]]; then
        echo 'jenkins: error: first export JENKINS_BASE_URL=[your URL]'
    elif [[ -z "$REPO" ]]; then
        echo 'jenkins: error: first call jenkins-repo $REPO_NAME'
    elif [[ -z "$BRANCH" ]]; then
        echo 'jenkins: error: first call jenkins-branch $BRANCH_NAME'
    else
        JENKINS_OUTPUT_URL="$JENKINS_BASE_URL/job/$REPO/job/$BRANCH/lastBuild/consoleText"
        
        JENKINS_OUTPUT_FILENAME="$(mktemp "$HOME/tmp/jenkins-log--$REPO--$BRANCH.XXXXXX")"
        osascript ~/bin/jenkins-fetch-console-output.osascript "$JENKINS_OUTPUT_URL" \
            | tee "$JENKINS_OUTPUT_FILENAME" \
            | tee >(command grep -m 1 -o "docker://.*" | sed 's/docker:[/][/]//' | tee pbcopy) \
            2>&1

        sms "Jenkins finished the build"
        # # This logfile-parsing code is brittle, so disable it for now
        # if command grep --quiet SUCCESS "$LOGFILE" >/dev/null; then
        #     SHORT_MESSAGE="Successfully built repo '$REPO', branch '$BRANCH'"
        #     DOCKER_IMAGE_URL="$(command grep --color=never --only-matching 'harbor.*:git-.*$' "$LOGFILE" | command uniq)"
        #     echo "$SHORT_MESSAGE"
        #     echo "Docker image URL copied to clipboard:"            
        #     echo "$DOCKER_IMAGE_URL" | tee >(pbcopy)
        # elif command grep --quiet 'Log in with OpenShift' "$LOGFILE" >/dev/null; then
        #     SHORT_MESSAGE="Jenkins login needed"
        #     echo "First log in at $JENKINS_BASE_URL"; open -u "$JENKINS_BASE_URL"
        # elif command grep --quiet 'FAILURE' "$LOGFILE" >/dev/null; then
        #     SHORT_MESSAGE="Failed to build repo '$REPO', branch '$BRANCH'"
        #     cat "$LOGFILE"
        #     echo
        #     echo "$SHORT_MESSAGE"
        # else
        #     SHORT_MESSAGE="Unrecognized Jenkins output for repo '$REPO', branch '$BRANCH'"
        #     cat "$LOGFILE"
        #     echo
        #     echo "^ $SHORT_MESSAGE"
        # fi
            
        # if [[ -n "$JENKINS_SMS_ME" ]]; then
        #     sms "$SHORT_MESSAGE"
        # fi
    fi
}

#### JSON settings

function json-key { 
    check-args 1 varargs 'json-key KEY_NAME [FILE1 [FILE2...]]' "$@"
    local KEY_NAME="$1"
    shift
    if [[ $# -eq 0 ]]; then
        grep -i -R "\"$KEY_NAME\":"  "."
    else
        grep -i "\"$KEY_NAME\":"  "$@"
    fi
}

#### Jenkins settings

function jthis {
    local REPO  BRANCH
    REPO="$(gr)"
    if [[ $? -eq 0 ]]; then
        BRANCH="$(gb)"
        if [[ $? -eq 0 ]]; then
            jenkins-repo "$REPO"
            jenkins-branch "$BRANCH"
        fi
    fi
    j
}

function jenkins-repo {
    local REPOFILE
    REPOFILE="/tmp/jenkins-repo"

    if [[ $# -eq 0 ]]; then
        cat "$REPOFILE"
    elif [[ $# -eq 1 ]]; then
        echo "$1" > "$REPOFILE"
    else
        echo "jenkins-repo: error: too many arguments" >&2
    fi
}

function jenkins-branch {
    local BRANCHFILE
    BRANCHFILE="/tmp/jenkins-branch"

    if [[ $# -eq 0 ]]; then
        cat "$BRANCHFILE"
    elif [[ $# -eq 1 ]]; then
        echo "$1" > "$BRANCHFILE"
    else
        echo "jenkins-branch: error: too many arguments" >&2
    fi
}

#### Jira settings, regrettably

my-source ~/.bashrc.d/jira.bash

#### Music-related settings

function music {
    am "$@"
}

function am {
    ~/src-3/music-cli/src/am.sh
}

# zophar - search for video game music (BGM)
function zophar {
    local URL
    local NES_URL="https://mail.zophar.net/music/nintendo-nes-nsf"
    
    if [[ $# -eq 0 ]]; then
        # default: NES
        URL="$NES_URL"
    else
        local CONSOLE="$1"
        case "$CONSOLE" in
            NES|nes)
                URL="$NES_URL";;
            SNES|snes)
                URL="https://www.zophar.net/music/nintendo-snes-spc";;
            PS2|PlayStation)
                URL="https://www.zophar.net/music/playstation2-psf2";;
            PSP)
                URL="https://www.zophar.net/music/playstation-portable-psp";;
            *)
                complain "Console '$1' is unsupported"; return 1;;
        esac
        
        open -u "$URL"
    fi
}

#### YAML and JSON settings

function yv {
    if [[ $# -ne 1 ]]; then
        echo "yv: error: wrong number of arguments. Usage: yv YAML_KEY_NAME"
    else
        KEY="$1"
        shift
        
        #FIXME: figure out how to glob for *.yaml case-insensitively. Maybe use fd?
        # command grep --no-filename "${KEY}:[ ]*" *.yaml | sort | uniq | sed "s/^[ ]*${KEY}:[ ]*//"
        fdd "$@" .yaml -X ggrep "${KEY}:"
    fi
}

function yvu {
    yv "$@" | sed "s/.*${KEY}:[ ]*//" | sort | uniq
}

# ys ("YAML Sort")" - take a file and sort its YAML keys alphabetically.
function ys {
    local FILE="$1"
    yq -P "sort_keys(..)" "$FILE"
}

# yd ("YAML Diff") - diff YAML files, sorting their keys first. Does not change the original files.
function yd {
    for FILE in "$1" "$2"; do
        ys "$FILE"
    done
    
    diff "$1" "$2"
}

function json2yaml {
    if [[ $# -eq 0 ]]; then
        echo "json2yaml: error: missing filename argument"
    else
        local JSON_FILENAME="$1"
        shift
        yq eval -p json -o yaml "$JSON_FILENAME" "$@"
    fi
}
    
function yaml2json {
    if [[ $# -eq 0 ]]; then
        echo "yaml2json: error: missing filename argument"
    else
        local YAML_FILENAME="$1"
        shift
        yq eval -p yaml -o json "$YAML_FILENAME" "$@"
    fi
}

#### path settings (global)

if [[ -z "$PATHS_SET_AT" ]]; then
    set -a
    PATHS_SET_AT="$(date)"

    ## executable PATH
    PATH="$HOME/.umt:/usr/local/opt/ccache/libexec:$JAVA_PATH:$NODE_PATH:$PATH:$RUBY_PATH:$PYTHON_INTERPRETER_PATH:$PYTHON_PIPX_PATH:$RUST_PATH:$PGPATH:/usr/local/bin:/opt/tmux/bin:/usr/local/sbin:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/color:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:/usr/local/opt/ncurses/bin:$HOME/bin/site:$HOME/bin/site/upsa:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources:$SCSH_PATH"
    
    ## C settings/dynamic library path settings
    CPPFLAGS="$RUBY_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$RUBY_LDFLAGS $SCSH_LDFLAGS $LDFLAGS"
    PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}:${PKG_CONFIG_PATH}"
    export SDKROOT="`xcrun --show-sdk-path`"
    
    # If some of those variables that I interpolated in my PATH are undefined,
    # then I will wind up with '::' in my PATH. That's equivalent to having
    # '.' (the current working directory) in one's path, which is a security
    # risk, because there's a silly trick that hackers can use where they
    # put a malicious binary named ls or grep or something inside a directory
    # and a privileged user cd's there and accidentally runs it.
    # So remove any instances of two (or more) consecutive colons.
    # Same goes for a single trailing colon.
    PATH=$(echo $PATH | sed -e 's/[:]\{2,\}/:/g' -e 's/[:]$//')

    ## Places that man(1) will search for man pages:
    MANPATH="$MY_MANPATH:$MANPATH"

    ## Dynamic library path settings
    LDFLAGS="$RUBY_LDFLAGS $SCSH_LDFLAGS $LDFLAGS"
    CPPFLAGS="$RUBY_CPPFLAGS $CPPFLAGS"
    PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}:${PKG_CONFIG_PATH}"

    ## golang path
    GOPATH="$HOME/go"

    set +a
fi

source-if-exists ~/.site.bash.d/postamble.bash
