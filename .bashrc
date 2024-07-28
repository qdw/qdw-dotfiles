umask 0022
export CLICOLOR=1

#### Bash debugging settings

## Uncomment the following line to run shellcheck, a mediocre and pedantic static analysis tool, on my Bash config files.
# shellcheck shell=bash

## Uncomment the following lines if you want to time each command in this file.
## You will also need to add 'time ' before every command in this file,
## because Bash doesn't have a good builtin time-tracing function.
# export TIMEFORMAT='    real %3R'; export PS4=''
# set -xT

# Uncomment if you want to *stop* timing how long each command in this
# file takes to run.
# set +xT

## Also useful for timing:

# TIMEFORMAT='%3R'
# function time_source {
#     echo -n "$1:  "
#     time source "$1"
#     echo
# }

# setx - run this function interactively to turn off the many
# prompt-related commands that automatically run every time I enter a
# command, then turn on tracing. This makes the trace much more
# uncluttered.

function setx {
    unset PROMPT_COMMAND
    export PS1="\u@\h:\w\\$ "
    set -x
}

#### Settings related to my little Bash metaprogramming library

function complain {
    for STRING in "$@"; do
        echo "$STRING" >&2
    done
}

function check_args {
    local FUNCTION="${FUNCNAME[1]}"
    
    if [[ $# -lt 3 ]]; then
        # Wrong number of args to check_args itself. Print an informative error message.

        local ARGUMENTS_INFLECTED
        if [[ $# -eq 1 ]]; then
            ARGUMENTS_INFLECTED="arguments"
        else
            ARGUMENTS_INFLECTED="argument"
        fi
        
        complain "check_args: error: $FUNCTION passed the wrong number of arguments to check_args itself" \
                 "($FUNCTION called check_args with $# $ARGUMENTS_INFLECTED, but it requires" \
                 "at least 3.)" \
                 "Usage: " \
                 "checks_args MIN_NUM_ARGS  MAX_NUM_ARGS  USAGE_STRING [ACTUAL_ARGUMENT_PASSED ...]"
        return 1
    else
        # Happy path: the caller passed the correct number of args to check_args itself.
        # Check whether the caller's caller called the caller properly.
        local MIN_ARGS="$1"; shift
        local MAX_ARGS="$1"; shift
        local USAGE="$1"; shift
        local ARGC=$#
        if ((ARGC < MIN_ARGS)); then
            
            local VARCLAUSE
            if [[ "$MAX_ARGS" = "varargs" ]]; then
                VARCLAUSE=" or more"
            else
                VARCLAUSE=""
            fi
            
            complain "$FUNCTION: error: too few arguments ($ARGC given, ${MIN_ARGS}${VARCLAUSE} required). Usage:" \
                     "$USAGE"
            return 2
        elif [[ "$MAX_ARGS" != "varargs" ]] && ((ARGC > MAX_ARGS)); then
            complain "$FUNCTION: error: too many arguments ($ARGC given, $MAX_ARGS allowed). Usage:" \
                     "$USAGE"
            return 3
        else
            return 0
        fi
    fi
}

function filter {
    check_args 1 varargs "filter FUNCTION_NAME [FUNCTION_ARGUMENT ...]"

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

MY_OS="$(uname -s)"
function on_macos {
    if [[ "$MY_OS" = "Darwin" ]]; then
        return 0
    else
        return 1
    fi
}

#### Clipboard settings

if on_macos; then
    function pbc { 
        local FILENAME
        FILENAME="$1"
        if [[ $FILENAME ]]; then
            pbcopy < "$FILENAME"
        else
            echo "pbc: error: missing argument. Usage: pbc FILENAME"
        fi
    }
fi

#### Color settings

# function word_wrap {
#     local STRING
#     if [[ $# -eq 1 ]]; then
#         STRING="$1"
#         ...

# bw (Black and White): remove ANSI color (and other) sequences
function bw {
    local  INPUT_STRING

    function sed_it {
        gsed 's/\x1b\[[0-9;]*m//g'
    }

    if [[ $# -eq 0 ]]; then
        cat | tr -d '[:punct:]'
    elif [[ $# -eq 1 ]]; then
        echo "$1" | tr -d '[:punct:]'
    else
        complain "baw: error: too many arguments. Usage:
    echo "some string" | baw
    cat some_file | baw
    baw FILENAME"
    fi
}

# Take a hex triplet, and print it in the indicated color.
# Requires a terminal that supports (ANSI SGR) 24-bit color,
# a.k.a. true color
function hex() {
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

#### cd settings

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
            HOMEBREW_EDITOR="command emacs -nw"
            HOMEBREW_ROOT="$HDIR"
            HOMEBREW_BIN="$HDIR/bin"            
            
            PATH="$HOMEBREW_BIN:$HDIR/sbin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin:$PATH"

            # man pages for brew(1) itself
            MY_MANPATH="$HDIR/manpages"            
            
            # man pages for software installed using brew
            MY_MANPATH="$HDIR/share/man"            
            PATH=$(echo $PATH | sed -e 's/[:]\{2,\}/:/g' -e 's/[:]$//')

            # Fix a problem with the nmap formula
            PYTHONPATH="${PYTHONPATH}:${HDIR}/lib/python3.9/site-packages"
            set +a

            break
        fi
    done
fi

# Prompt before overwriting files.
function mv {
    command mv -i "$@"
}

if [[ "$MY_OS" = "Darwin" ]] && [[ -f "$HOMEBREW_BIN/trash" ]]; then
    function rm {
        trash "$@"
    }
fi

## If on macOS, use the Homebrew versions of basic command-line utilities
## instead of the bundled macOS versions, as the Homebrewed versions
## are more frequently updated.

for GNU_COMMAND in cut date dircolors find head roff sed sort stat tac tail; do
    BREWED_GNU="${HOMEBREW_BIN}/g${GNU_COMMAND}"
    if [[ -f "$BREWED_GNU" ]]; then
        funcalias "$GNU_COMMAND" "$BREWED_GNU"
    fi
done

#### fd settings (fd is a faster and better replacement for find(1)

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

function fdgrep {
    # check_args 1 varargs "fdgrep FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
    local FILENAME_REGEX="$1"; shift    
    ( fdd --type f "$FILENAME_REGEX" -x $MY_GREP -I -H --color=always $GREP_EXCLUDE_FLAGS "$@" \; ) \
        | $PAGER
}

# Grep TypeScript files only
function tsgrep { fdgrep "[.]ts$" "$@" ;}

function fdoccur {
    # check_args 2 varargs "fdoccur FILENAME_REGEX [GREP_OPTION ...] GREP_REGEX"
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

if [[ -e "$HOMEBREW_BIN/ggrep" ]]; then
    export MY_GREP="ggrep -E" MY_FGREP="ggrep -F" MY_PGREP="ggrep -P"
else
    export MY_GREP="egrep" MY_FGREP="fgrep" MY_PGREP="echo 'Perl-compatible grep syntax is not available on this platform' >&2"
fi

export MY_GREP="ggrep -E" MY_FGREP="ggrep -F" MY_PGREP="ggrep -P"

## Skip binary files.

GREP_OPTIONS="$GREP_OPTIONS -I"

## Color my grep output.
## This choice of colors matches Emacs compilation-mode's colors.

GREP_OPTIONS="$GREP_OPTIONS --color=always"

GREEN="32"
YELLOW="33"

FILENAME_COLOR="fn=${GREEN}"
LINE_NUMBER_COLOR="ln=${YELLOW}"
export GREP_COLORS="${FILENAME_COLOR}:${LINE_NUMBER_COLOR}"

## Skip metadata files, Emacs droppings, et cetera

GREP_EXCLUDE_FLAGS="--exclude-dir=.git --exclude-dir=.github --exclude-dir=cdk.out --exclude-dir=node_modules --exclude=.gitignore --exclude=.npmignore --exclude=package-lock.json --exclude-dir=__snapshots__"

function grep {  $MY_GREP    $GREP_OPTIONS $GREP_EXCLUDE_FLAGS "$@" | $PAGER ;}
function fgrep { $MY_FGREP   $GREP_OPTIONS $GREP_EXCLUDE_FLAGS "$@" | $PAGER ;}
function g {     $MY_GREP -R $GREP_OPTIONS $GREP_EXCLUDE_FLAGS --exclude '*.js' "$@" | $PAGER ;}
function gbr {   $MY_GREP -R $GREP_OPTIONS $GREP_EXCLUDE_FLAGS "$@" ~/.bashr* ~/.*profile* ~/.site.bash* ;}
function ge {    $MY_GREP -R $GREP_OPTIONS $GREP_EXCLUDE_FLAGS "$@" ~/.emacs.d ;}

function gfd {
    local REGEX USAGE
    USAGE="Usage: gfd REGEX"
    
    if [[ $# -eq 0 ]]; then
	echo "gfd: error: missing argument. $USAGE"
    elif [[ $# -eq 1 ]]; then
	REGEX="$1"
	(g "$REGEX"; fd "$REGEX")
    else
        echo "gfd: error: wrong number of arguments. $USAGE"
    fi
}

function grep-settings {
    # check_args 2 varargs
    local REGEX="$1"
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

    ggrep -i $GREP_EXCLUDE_FLAGS \
          "^${COMMENT_PREFIX}.*(${REGEX}.*settings|settings.*${REGEX})" \
        | $PAGER
}

# fip (Filter In Place) - filter lines matching regex from a file,
# modifying the file in-place to remove those lines. If you specify
# multiple files, it does this to all of them.
function fip {
    # check_args 2 varargs
    
    local REGEX="$1"
    shift
    
    # if [[ "$1" = "-"  ]]; then
    #     local LINE
    #     while true; do
    #         LINE="$(read)"
    #         if [[ "$LINE" = "" ]]; then
    
    for FILE in "$@"; do
        grep -vE "$REGEX" "$FILE" | sponge "$FILE"
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
    $MY_GREP -R --files-with-matches --max-count=1 $GREP_EXCLUDE_FLAGS "$@" | $PAGER
}

#### Bash settings

function source-if-exists {
    if [[ $# -eq 1 ]] && [[ -f "$1" ]]; then
        source "$1"
    fi
}

# grbash - grep my Bash config files
function grbash {
    if [[ $# -ne 1 ]]; then
        echo "grbash: error: wrong number of arguments. Usage: grbash REGEX"
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
                source "$BASH_COMPLETION_DIR/${FILENAME}"
            done
        }

        case "$1" in
            aws*)
                sc "aws_bash_completer"
                ;;
                
            brew)
                source "$HOMEBREW_ROOT/completions/bash/brew"
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
                source "$HOMEBREW_ROOT/share/gitprompt.sh"    
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
            source "$HOMEBREW_ROOT/share/gitprompt.sh"
            source "$BASH_COMPLETION_DIR/gh"
            source "$BASH_COMPLETION_DIR/git-completion.bash"
            export GIT_COMPLETION_ACTIVATED=1
        fi
    }
    
    MY_PROMPT_FUNCTION_DEFINITION=$(command -V my_prompt_function | tail -n +2)
    ## not for upstart_web export PROMPT_COMMAND="$MY_PROMPT_FUNCTION_DEFINITION; my_prompt_function"
fi

#### Scheme Shell settings (keywords: Lisp, scsh)

if [[ -d "/opt/scsh" ]]; then
    SCSH_PATH="/opt/scsh/bin"
    LATEST_SCSH_VERSION="$(ls /opt/scsh/lib | sort --general-numeric-sort | tail -1)"
    SCSH_LDFLAGS="-L/opt/scsh/lib/scsh-${LATEST_SCSH_VERSION}"
fi    

#### Pager settings

set -a
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
        export AWS_PAGER="jless"
    else
        export AWS_PAGER="$PAGER"
    fi

    export PAGER="command less --RAW-CONTROL-CHARS --quit-if-one-screen --no-init --mouse"
    
    export GIT_PAGER="$PAGER -j 2" # Show an extra line before match - useful with 'git diff'
    
    export PSQL_PAGER="command less --no-init"
fi

ACK_PAGER="$PAGER"
BAT_PAGER="$PAGER"
MANPAGER="$PAGER"
set +a

function less { $PAGER "$@" ;}

#### Markdown settings

# export GLAMOUR_STYLE="Dark"
function readme {
    gh repo view
}

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
function md2html { markdown2html "$@" ;}

#### #FIXME settings: rewrite 'function bbat {...}' to use 'gh' and GLAMOUR_STYLE, which should make the output look nicer

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
    if [[ "$MY_OS" = "Darwin" ]]; then
        if [[ "$1" =~ ^[[:upper:]] ]] || [[ "$1" =~ [.]app$ ]]; then
            ldd-app "$@" | $PAGER
        else
            otool -L "$@" | $PAGER
        fi
    else
        command ldd "$@"
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

function iam {
    if [[ $# -eq 0 ]]; then
        echo "USAGE:"
        echo
        echo 'iam --hostname "$host" --port "$port" --region "$region" --username "$user" --profile "$profile"'
    else
        aws rds generate-db-auth-token "$@"
    fi
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
        rds_ls "$@"
    else
        echo "No such subcommand '$1'."; usage
    fi
}

# rds_ls ("RDS LiSt") - list RDS instances (highlighting the hostname portio nof the FQDN)
function rds_ls {
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

function list-rds-instances { rds_ls "$@" ;}

function list-secrets { aws secretsmanager list-secrets ;}
function list-secret-ids { list-secrets | jq --raw-output '.SecretList[].Name' | sort ;}


function get-secret {
    aws secretsmanager get-secret-value --secret-id "$1" \
        | jq '.SecretString | fromjson'
}
function secret { get-secret "$@" ;}

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
        command emacs --bg-daemon
        
	# Give it time to start up and load my ~/.emacs.d/init.el
        sleep 4
        emacs "$@"
    fi
}

# re (Restart Emacs-daemon): stop it, then start it.
function re { ke && se ;}

set -a
if [[ "$INSIDE_EMACS" ]]; then
    EDITOR_FUNCTION="emacs"
else
    EDITOR_FUNCTION="command emacs -nw"
fi
EDITOR="$VISUAL"
GIT_EDITOR="$EDITOR_FUNCTION"
PSQL_EDITOR="$EDITOR_FUNCTION"
VISUAL="$EMACS_BINARY"
set +a

# (This function depends on the third-party newtab() function;
# see ~/.bashrc.d/third-party.bash)
# 
# te ("TErminal window") - create a new Terminal.app window in the same directory I'm in when I run te.
function te {
    newtab bash # -c "cd $(pwd); $@" #FIXME: get this -c thing to work
}

function nt { newtab "$@" ;}

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
# that feature, so I'm comment it out for now.
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
    source ~/.iterm2_shell_integration.bash
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

# Color the shell prompt ANSI blue (to distinguish commands from output).

#export PS1="\\[\\e[34;1m\\]${PS1_LOGO}\\u@${PS1_HOSTNAME}:\\w\\$\\[\\e[0m\\] "
export PS1="\\[\\e[34;1m\\]\\w\\$\\[\\e[0m\\] "
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
    brew-deps-all | command ggrep -E '^[:alpha]' | sort | uniq | $PAGER
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
        RAW_COUNT=$(brew deps --tree --installed | grep -c "$PACKAGE_NAME")
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

# bat-head FILENAME NUMBER: use color bat to show a file's first n lines
function bat-head {
    local FILENAME="$1"
    local NUMBER="$2"
    
    bat --line-range ":${NUMBER}" "$FILENAME"
}

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub)
function bbat  {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
}

# cdy ("CD Realname") - cd to the directory that a symlink points to (or, if it points to a non-directory file, cd to that file's parent dir).
function cdr {
    local TARGET
    TARGET=$(readlink -- "$1")

    if [[ -d "$TARGET" ]]; then
        cd "$TARGET"
    else
        PARENT=$(echo "$TARGET" | sed 's/\(.*\)[/].*/\1/')
        cd "$PARENT"
    fi
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

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=39;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:'

function ls {
    if [[ "$PWD" = "$HOME" ]] && [[ $# -eq 0 ]]; then
        # By default, don't show dotfiles in my home dir...
        command $MY_LS $MY_LS_OPTIONS "$@" | $PAGER
    else
        # ... but do show them everywhere else.
        command $MY_LS $MY_LS_OPTIONS -A "$@" | $PAGER
    fi
}

function recent {
    ls -t "$@" | head -5
}

function lsd {
    command $MY_LS $MY_LS_OPTIONS "$@" ~/d/ | $PAGER
}

#### diff settings

function dcc { colordiff -u "$@" | $PAGER ;}

export PATH="/opt/homebrew/opt/diffutils/bin:$PATH"
# function diff {
#     local COMMON_DIFF_OPTIONS="-u --color=always"
#     if [[ -f "/opt/homebrew/opt/diffutils/bin/diff" ]]; then
#         # Color legend:
#         # ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
#         # de - deleted lines (31 is SGR red, which shows as orange in my terminal)
#         # hd - header        (35 is SGR magenta, which shows as magenta in my term)
#         # ln - line numbers  (34 is SGR blue, which shows as light pink in my term)
#         # rs - undocumented, but the default seems fine
#         command diff $COMMON_DIFF_OPTIONS --palette="ad=36:de=31:hd=35:ln=34:rs=0" "$@" | $PAGER
#     else
#         command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
#     fi
# }

function diff {
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
    # ln - line numbers  (34 is SGR blue, which shows as light pink in my term)
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

#### FTP settings

set -a
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=""
set +a

#### Git setttings

## General Git settings (not specific to GitHub)

export GIT_SHELL_PATH="$HOMEBREW_BIN/bash"

function branch-name {
    local PHRASE_STRING
    if [[ $# -eq 1 ]]; then
        PHRASE="$1"
        kebab-case "$(strip-punctuation "$PHRASE")"
    else
        complain "branch-name: error: wrong number of arguments. Usage: branch-name 'Some (explanatory) phase'"
    fi
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
    check_args 0 1 "gcd [RELATIVE_DIR]"
    cd "$(git rev-parse --show-toplevel)/$1"
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
    basename $(git rev-parse --show-toplevel)
}

function git-backup {
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


function get-main-branch-name {
    local MAIN_OR_MASTER
    
    MAIN_OR_MASTER="$(command git branch --remotes | command ggrep --perl-regexp --only-matching '  origin/HEAD -> origin/(?=master|main)\K.*$' | sort | head -1)"
    echo "$MAIN_OR_MASTER"
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
                | command grep -v -E 'remote:' #\
            #&& git push --set-upstream origin # --quiet
    else
        echo "gb: error: invalid number of arguments '$#'. Usage: gb | gb NEW_BRANCH_NAME"
    fi
}

# Generate a 
function gbb {
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

# gbs (Git Branches) - list (local) git branches
function gbs { git branch ;}

# gu (Git URL) - run this in any dir inside a Git working copy to show the URL
function gurl {
    local ORIG_DIR
    ORIG_DIR=$(pwd)
    gcd \
        && command grep url .git/config | sed -e 's/^[ \t]*url[ ][=][ ]//'
    cd "$ORIG_DIR"
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

# branch BRANCH_NAME - display the GitHub web page for BRANCH_NAME
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

function all_gists {
    gists --limit -1
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
        $GIST_EDITOR "$FILENAME"        
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
function gcm { git commit -m "$@" ;}
function gdo { git diff origin HEAD ;}
function gn { git diff --name-only "$@" ;}
function gs { git status "$@" ;}


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
            PASSWORD="$(pql "$SERVICE")"
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
    local REGEX
    
    if [[ $# -eq 0 ]]; then
        echo "ser: missing argument. Usage: ser [GREP_FLAG ...] REGEX" >&2
    else
        REGEX="$1"
        shift

        command grep -E $@ "^[[].*$REGEX.*[]]" ~/.pg_service.conf
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

## Ballet-slippers-ordering settings
function ballet-flats {
    open -u "https://www.discountdance.com/dancewear/Split-Sole-Vesta-Canvas-Ballet-Shoe/p/VESTA"
}

## Coffee-bean-ordering settings

function coffee {
    local URL="https://store.awakencafe.com/s/order?location=11e93a189ebfc0bcb7910a580a1c0333#6"
    # local URL="https://store.awakencafe.com/s/customer-accounts"
    open -a Firefox "$URL"
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
        source "$MY_VIRTUALENV_DIR/$VENV_NAME/bin/activate"
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

function ips { /sbin/ifconfig -a | grep 'inet ' | grep -v 'inet6 ' ;}
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
function p {
    local HEADER PROCS
    local PS_COMMAND="ps auxwww"
    
    if [[ $# -eq 0 ]]; then
        $PS_COMMAND | $PAGER
    else
        HEADER="$($PS_COMMAND | head -1)"

        set -o pipefail
        PROCS="$($PS_COMMAND | command grep -v grep | grep "$@")"
        RC=$?
        set +o pipefail
        
        if [[ -n "$PROCS" ]]; then
            echo "$HEADER"
            echo "$PROCS"
        fi

        return $RC
    fi
}

function pstree {
    command pstree -g 3 -u $USER "$@" | $PAGER
}

# Display only processes that belong to me.
function pu { p "$USER" ;}

#### vim settings

# vl ("Vim Less") - use vim as a colorizing pager
function vl { "$HOMEBREW_ROOT/share/vim/vim91/macros/less.sh" "$@" ;}

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

function atom { newsbeuter "$@" ;}

# cel FILE.el ("compile LISP") - byte-compile an emacs LISP file.
function cel { "$EMACS_BINARY" -nw -q -batch -f batch-byte-compile "$@" ;}

# d ("date") - give today's date in YYYY-MM-DD format.
function d { date +%Y-%m-%d ;}

function epoch2human {
    # check_args 1 1 "epoch2human EPOCH_SECONDS"
    local EPOCH_SECONDS="$1"
    date -d "@${EPOCH_SECONDS}"
}

function dos2unix { perl -pe 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream - run the Electric Sheep fractal generation daemon
function dream { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# dfr ("df real") - show all mounted devices that are actually disks (see also mr).
function dfr {
    df -h | grep ^/
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

function ho { grep "Host .*${1}" ~/.ssh/*conf* ;}

# mr ("mount real") - show all mounted devices that are actually disks (see also dfr).
function mr { mount | grep ^/ ;}

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
    STRING="$1"
    shift
    awk -v ORS="\n\n" -v RS='' "/$STRING/" "$@"
}

function conf { para "$1" ~/.ssh/*.config | grep -e '^' -e "$1" ;}

# pi ("Perl @INC") - print @INC dirs, one per line, preceded by the path to Perl.
function pi {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

# shn ("SSH hostname") - print the canonical hostname for a ~/.ssh/config alias.
function shn {
    grep -A 1 "$1" ~/.ssh/*config* | tail -1 | awk '{ print $3; }'
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

function activate-perlbrew { source ~/perl5/perlbrew/etc/bashrc ;}
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
export NODE_MANPATH="/opt/node/share/man:/opt/node/lib/node_modules/npm/man/"
MY_MANPATH="$NODE_MANPATH:$MY_MANPATH"

#### Jenkins settings

function j {
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

source ~/.bashrc.d/jira.bash

#### Music-related settings

function music {
    am "$@"
}

function am {
    ~/src-3/music-cli/src/am.sh
}

# zophar - search for NES music
function zophar {
    open https://mail.zophar.net/music/nintendo-nes-nsf
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

if [[ ! "$PATHS_SET_AT" ]]; then
    set -a
    PATHS_SET_AT="$(date)"

    ## executable PATH
    PATH="$HOME/.umt:/usr/local/opt/ccache/libexec:$JAVA_PATH:$NODE_PATH:$PATH:$RUBY_PATH:$PYTHON_INTERPRETER_PATH:$PYTHON_PIPX_PATH:$RUST_PATH:$PGPATH:/usr/local/bin:/opt/tmux/bin:/usr/local/sbin:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/color:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:/usr/local/opt/ncurses/bin:$HOME/bin/site:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources:$SCSH_PATH"
    
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

    ## man page path settings
    MANPATH="$MY_MANPATH:$MANPATH"

    ## dynamic library path settings
    LDFLAGS="$RUBY_LDFLAGS $SCSH_LDFLAGS $LDFLAGS"
    CPPFLAGS="$RUBY_CPPFLAGS $CPPFLAGS"
    PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}:${PKG_CONFIG_PATH}"

    ## golang path
    GOPATH="$HOME/go"
    
    set +a
fi

source-if-exists ~/.site.bash.d/postamble.bash
