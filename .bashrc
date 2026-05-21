umask 0022

set -o pipefail

QSITED="$HOME/.site.bash.d" # not checked into git; specific to this machine

QBASHD="$HOME/.bashrc.d" # checked into git
function source-section {
    source "$QBASHD/$1"
}

#### My Bash metaprogramming library

source-section "00-bash-metaprogramming.bash" ### Bash helper funcs, arg checking, functional programming"

export MY_DEBUG=""
export MY_VERBOSE_DEBUG_BASH=""
source "$QBASHD/01-bash-debugging.bash"

#### The following files shouldn't be checked into git
  ## (either because they contain secrets, or because they contain
  ## third-party code with its own license)

source-if-exists "$QSITED/preamble.bash"
source-if-exists "$HOME/.secrets.bash"

#### Time and date #sec4#

function gnu-mtime-seconds {
    check-args 1 1 'FILENAME' "$@" || return 1
    local FILENAME="$1"
    stat -c %Y "$FILENAME"
}

function bsd-mtime-seconds {
    check-args 1 1 'FILENAME' "$@" || return 1
    local FILENAME="$1"
    stat -f %m "$FILENAME"
}

function mtime-in-epoch-seconds {
    check-args 1 1 'FILENAME' "$@" || return 1
    local FILENAME="$1"

    if on-linux || [[ "$(command -V stat | head -1)" = "stat is a function" ]]; then
        gnu-mtime-seconds "$FILENAME"
    elif on-macos; then
        bsd-mtime-seconds "$FILENAME"
    else
        carp "warning: unrecognized platform; guessing that stat(1) is GNU stat"
        gnu-mtime-seconds "$FILENAME"
    fi
}

# Timestamp with time zone, no special chars, OK to interpolate into filenames
function ts {
    check-args 0 0 '[no-arguments]' "$@" || return 1
    date '+%Y-%m-%dT%H%M%S%z' "$@"
}

function pg-ts {
    psql --no-psqlrc -tc 'select now()' | sed -e 's/[ ]*//'
}

# Timestamp in the format used by YAML
function ts-yaml {
    local TIMEZONE_SHORTCODE="$(date +%Z "$@")"
    local TS_WITH_NANOSECONDS="$(date +%Y-%m-%dT%H:%M:%S.%N "$@")"
    local TS_WITH_DECISECONDS=${TS_WITH_NANOSECONDS:0:21}
    local RETVAL="${TS_WITH_DECISECONDS}${TIMEZONE_SHORTCODE}"
    echo "$RETVAL"
}

#### OS-specific settings #sec 5#

MY_OS="$(uname -s)"

function on-macos {
    [[ "$MY_OS" = "Darwin" ]]
}

function on-linux {
    [[ "$MY_OS" =~ Linux ]]
}

## OS-specific linker settings

function ldd {
    check-args 1 1 'EXECUTABLE | Some.app' "$@" || return 1
    local PROG="$1"
    
    if on-macos; then
        local APP_PREDICATE_REGEX=".+[.]app"
        local PATH_PARSING_REGEX="(.*[/](.*)[.]app$)"
        if [[ "$PROG" =~ $APP_PREDICATE_REGEX ]]; then
            local APP_FULL_PATH
            APP_FULL_PATH="$(which "$PROG")"
            local WHICH_EXIT_STATUS=$?
            if [[ $WHICH_EXIT_STATUS -ne 0 ]]; then
                return $WHICH_EXIT_STATUS
            elif [[ "$APP_FULL_PATH" =~ $PATH_PARSING_REGEX ]]; then
                local APP_PATH="${BASH_REMATCH[1]}"
                local APP_NAME="${BASH_REMATCH[2]}"
                local EXE_PATH="$APP_PATH/Contents/MacOS/$APP_NAME"
                otool -L "$EXE_PATH" | $PAGER
            else
                err "$PROG didn't match the path-parsing regex"
                return 3
            fi
        else
            otool -L "$PROG" | $PAGER
        fi
    else
        command ldd "$PROG" | $PAGER
    fi
}

# If some of the variables that I interpolated in my PATH are undefined,
# then I will wind up with '::' in my PATH. That's equivalent to having
# '.' (the current working directory) in one's path, which is a security
# risk, because there's a silly trick that hackers can use where they
# put a malicious binary named ls or grep or something inside a directory
# and a privileged user cd's there and accidentally runs it.
# Plus it looks bad. So remove any instances of two (or more)
# consecutive colons. Same goes for a single leading or trailing colon.

function decolonize {
    check-args 1 1 'STRING # like PATH or MANPATH' "$@" || return 1
    STRING="$1"
    echo $STRING | sed -e 's/^[:]//' -e 's/[:]\{2,\}/:/g' -e 's/[:]$//'
}

#### Text editor #sec 6#

function install-emacs-plus {
    check-args 1 1 'EMACS_VERSION_NUMBER # e.g., 29' "$@" || return 1
    local VERSION_NUMBER="$1"
    
    (
        set -euo pipefail
        brew install --formula \
                     --build-from-source \
                     "emacs-plus@${VERSION_NUMBER}" \
                     --with-mailutils \
                     --without-cocoa
    )
}

# Alas, emacs-plus's compile options are no longer adequate to my needs.
# Build Emacs from the original source, but apply emacs-plus's favored
# patches first.

function install-emacs {
    check-args 1 1 'EMACS_VERSION_NUMBER # e.g., 30' "$@" || return 1
    local VERSION_NUMBER="$1"
    
    local INSTALL_DIR="/opt/emacs/$VERSION_NUMBER"
    local SOURCE_DIR="$HOME/src-3/emacs"
    local PATCH_DIR="$HOMEBREW_ROOT/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-$VERSION"
    
    local DESIRED_PATCHES=""
    
    function dir-is-okay {
        if [[ -d "$DIR" ]] && \
           [[ -r "$DIR" ]] && \
           [[ -w "$DIR" ]] && \
           [[ -x "$DIR" ]] && \
           [[ -z "$(command ls -A "$DIR")" ]]
        then
            return 0
        else
            err "cannot access directory '$DIR'"
            return 2
        fi
    }
    
    brew install $(brew deps "emacs-plus@${VERSION_NUMBER}") || return    
    
    for DIRECTORY in "$INSTALL_DIR" "$SOURCE_DIR" "$PATCH_DIR"; do
        dir-is-okay "$DIRECTORY" || return
    done
        
    cd "$SOURCE_DIR" || return
    command git checkout "emacs-${VERSION_NUMBER}" || return
    
    ./autogen.sh || return
    ./configure \
        --prefix "$INSTALL_DIR" \
        ...
        
    for PATCH in $DESIRED_PATCHES; do
        apply_patch "$PATCH" || return
    done
    
}

function emacs-package-clean {
    cd ~/.emacs.d \
        && fd --no-ignore '.el[cn]$' -x rm
}

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
    check-args 0 1 '[SIGNAL]' "$@" || return 1
    local ARG=""

    if [[ $# -eq 1 ]]; then
        case "$1" in
            "-9" | "-KILL") :;;
            *) err "Invalid signal '$1'"; return 1;;
        esac
    fi

    local PID="$(emacs-daemon-pid)"
    if [[ -n "$PID" ]]; then
        kill $ARG "$PID"
    else
        err "It doesn't look like the emacs daemon is running"
        return 2
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
            command emacsclient --no-wait "$@"
        else
            echo "You're already in emacs, genius" >&2
        fi
    else
        if the-emacs-daemon-is-running; then
            command emacsclient --tty "$@"
        else
            se "$@"
        fi
    fi
}

# se (Start Emacs-daemon): start the Emacs daemon.
function se {
    if the-emacs-daemon-is-running; then
        err "Looks like the emacs daemon is already running"
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

MY_EDITOR="$HOME/bin/emacs-wrapper"

set -a
# GIT_EDITOR="$MY_EDITOR"
GIT_EDITOR="subl"
EDITOR="$MY_EDITOR"
VISUAL="$MY_EDITOR"

if command-exists "subl"; then 
    PSQL_EDITOR="subl -w"
    HOMEBREW_EDITOR="subl"
else
    PSQL_EDITOR="$MY_EDITOR"
fi
set +a

#### Homebrew #sec 7#

set -a
HOMEBREW_BAT=1
HOMEBREW_BAT_CONFIG_PATH="$HOME/.config/bat/config"
HOMEBREW_NO_ANALYTICS=1
HOMEBREW_NO_ENV_HINTS=1
set +a

## Homebrewed alternatives to bundled system utilities

MY_LS_BASE_OPTIONS="--color=always -1F"
MY_GNU_SPECIFIC_LS_OPTIONS="--group-directories-first"
if test -e "$HOMEBREW_BIN/gls"; then
    export MY_LS="$HOMEBREW_BIN/gls"
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS $MY_GNU_SPECIFIC_LS_OPTIONS"
else
    export MY_LS="$(which ls)"
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS"
fi

for HDIR in "/opt/homebrew" "/usr/local"; do
    if [[ -x "$HDIR/bin/brew" ]]; then
        set -a
        HOMEBREW_MANPATH="$HDIR/manpages:$HDIR/share/man"
        HOMEBREW_INFOPATH="$HDIR/share/info"
        HOMEBREW_ROOT="$HDIR"
        HOMEBREW_OPT="$HDIR/opt"
        HOMEBREW_BIN="$HDIR/bin"
        MY_HOMEBREW_PATH="$HOMEBREW_OPT:$HOMEBREW_BIN:$HDIR/sbin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin"
        PATH="$(decolonize "$MY_HOMEBREW_PATH:$PATH")"
        set +a
        break
    fi
done

## Basic brew shortcuts and wrappers

function brew {
    MANNAME="run Homebrew commands, paginating docs if any"
    if [[ "$1" = 'help' ]] || [[ ${!#} = '--help' ]]; then
        command brew "$@" | $PAGER
    else
        command brew "$@"
    fi
}

function be { brew edit "$@" ;}
function bh { brew home "$@" ;}
function bi { brew install "$@" ;}
function bl { brew ls "$@" ;}
function bs { brew search "$@" ;}
function bser { brew services "$@" ;}

function brew-service-names {
    local MANNAME="list Homebrew services (names only)"
    brew services list --json | jq -r '.[].name'
}

function bls {
    MANNAME="search for package (first installed packages, then uninstalled)"
    check-args 1 1 'REGEX' "$@" || return 1
    local REGEX="$1"

    brew ls | grep "$REGEX"
    if [[ $? -ne 0 ]]; then
        brew search "/$REGEX/"
    fi
}

## Functions for inspecting Homebrew dependencies

function brew-deps-all {
    brew deps --tree --install | $PAGER
}

function brew-leaves {
    brew-deps-all | command grep -E '^[[:alpha:]]' | sort | uniq | $PAGER
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

function  bnl { brew-next-leaf "$@" ;}
function leaf { brew-next-leaf "$@" ;}

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

## Functions for running Homebrew upgrades

BREW_UPDATE_TIMESTAMP_FILE="$HOME/tmp/brew_updated_at"
BREW_UPDATE_STALENESS_THRESHOLD="600" # = 10 min. in seconds

function brew-update-timestamp {
    check-args 0 0 '' "$@" || return 1
    touch "$BREW_UPDATE_TIMESTAMP_FILE"
}

function brew-is-stale {
    check-args 0 0 '' "$@" || return 1

    if [[ ! -f "$BREW_UPDATE_TIMESTAMP_FILE" ]]; then
        _debug "file '$BREW_UPDATE_TIMESTAMP_FILE' missing, so brew is stale"
        return 0
    else
        local NOW="$(date +%s)"
        _debug "$NOW since the Epoch"
        local LAST_BREW_UPDATE="$(mtime-in-epoch-seconds "$BREW_UPDATE_TIMESTAMP_FILE")"
        _debug "$LAST_BREW_UPDATE seconds since brew"
        local SECONDS_OLD=$(( NOW - LAST_BREW_UPDATE ))
        _debug "elapsed seconds: $SECONDS_OLD"
        if [[ $SECONDS_OLD -gt $BREW_UPDATE_STALENESS_THRESHOLD ]]; then
            _debug "$SECONDS_OLD > $BREW_UPDATE_STALENESS_THRESHOLD, so the brew is stale"
            return 0
        else
            _debug "$SECONDS_OLD is less than $BREW_UPDATE_STALENESS_THRESHOLD, so the brew is still fresh"
            return 1
        fi
    fi
}

function brew-force-update {
    check-args 0 0 '' "$@" || return 1
    brew update --quiet && brew-update-timestamp
}

function brew-update-if-stale {
    check-args 0 0 '' "$@" || return 1
    if brew-is-stale; then
        brew-force-update
    fi
}

function bo {
    check-args 0 0 '' "$@" || return 1
    brew-update-if-stale
    brew outdated
}

function boser {
    local MANNAME="show outdated Homebrew services"
    brew-update-if-stale

    local -a OUTDATED_FORMULAE
    mapfile -t OUTDATED_FORMULAE < <(bo)

    local -A BREW_SERVICE_NAMES
    while IFS="" read -r NAME; do
        BREW_SERVICE_NAMES["$NAME"]=1
    done < <(brew-service-names)

    for FORMULA_NAME in ${OUTDATED_FORMULAE[@]}; do
        if [[ -v BREW_SERVICE_NAMES[$FORMULA_NAME] ]]; then
            echo "$FORMULA_NAME"
        fi
    done
}

function bu {
    local MANNAME="upgrade Homebrew packages, and restart services if needed"
    check-args 0 0 '' "$@" || return 1

    brew-update-if-stale
    local -a OUTDATED_SERVICES
    mapfile -t OUTDATED_SERVICES < <(boser)

    brew upgrade

    if [[ ${#OUTDATED_SERVICES[@]} -gt 0 ]]; then
        color-printf "blue" "==> "; color-printf "red" "Restarting services:\n"
        for SVC in ${OUTDATED_SERVICES[@]}; do
            color-printf "green" "==> "; color-printf "red" "Restarting "; color-printf "bright-green" "$SVC\n"
            brew services restart "$SVC"
        done
    fi
}

function shells {
    MANNAME="list installed Unix shells"
    check-args 0 0 '[no arguments]' "$@" || return 1
    (
        basename "$(which scsh 2>/dev/null)"

        ORIGINAL_DIR="$(pwd)" && cd /bin && printf "%s\n" *sh

        brew ls \
            | command grep -E --color=never 'sh$|nushell' \
            | command grep -vE 'ssh|trash|hash|cash'
    ) \
    | sort
}

function boi {
    local MANNAME="show outdated Homebrew packages, and prompt to upgrade them"
    brew-update-if-stale

    local -a OUTDATED_FORMULAE OUTDATED_CASKS
    mapfile -t OUTDATED_FORMULAE < <(brew outdated --formula)
    local N_FORM=${#OUTDATED_FORMULAE[@]}
    if [[ $N_FORM -gt 0 ]]; then
        color-printf "red" "==> "; color-printf "red" "Outdated formulae:\n"
        echo ${OUTDATED_FORMULAE[@]}
    fi


    mapfile -t OUTDATED_CASKS < <(brew outdated --cask | tee /dev/tty)
    local N_CASK=${#OUTDATED_CASKS[@]}
    if [[ $N_CASK -gt 0 ]]; then
        color-printf "red" "==> "; color-printf "red" "Outdated casks:\n"
        echo ${OUTDATED_CASKS[@]}
    fi


    if [[ $N_FORM -gt 0 ]] || [[ $N_CASK -gt 0 ]]; then
        color-printf "bright-red" "==> Upgrade? [y/N]> "
        local USER_REPLY
        read USER_REPLY
        if [[ "${USER_REPLY,,}" =~ y ]]; then # downcase, then match
            bu
        fi
    fi
}

#### OS-specific settings for macOS #sec 8#

if on-macos; then
    source-section "macos-apps.bash" ### Shortcuts for macOS apps, e.g. Mail, Mail.app, Finder, Finder.app

    source-section "macos-light-and-dark.bash" ### Set light/dark mode in all apps, web pages, and other contexts

    source-section "macos-app-completion.bash" ### Make 'la SUBSTRING tab' complete on macOS app names

    set -a
    XDG_CONFIG_HOME="$HOME/.config"
    XDG_CACHE_HOME="$HOME/.cache"
    XDG_DATA_HOME="$HOME/.local/share"
    XDG_STATE_HOME="$HOME/.local/state"
    set +a

    if [[ -f "$HOMEBREW_BIN/trash" ]]; then
        # function rm {
        #     "$HOMEBREW_BIN/trash" -F "$@"
        # }
        function trash {
            "$HOMEBREW_BIN/trash" -F "$@"
        }
    fi
    
    function left {
        dock "left"
    }

    function right {
        dock "right"
    }
    
    function dock {
        local VALID_ARGS="left|right|bottom"

        check-args 1 1 "[$VALID_ARGS]" "$@" || return 1
        local SIDE="$1"
        case "$SIDE" in
            left|right|bottom) :;;
             *) complain "invalid argument '$SIDE'. " \
                         "Valid arguments: $VALID_ARGS"
                         return 1;;
        esac
        defaults write com.apple.dock orientation "$SIDE" \
            && killall Dock
    }
    
    function pbc {
        local FILENAME="$1"
        if [[ "$FILENAME" ]]; then
            pbcopy < "$FILENAME"
        else
            echo "pbc: error: missing argument. Usage: pbc FILENAME"
        fi
    }

    function plist-bin-to-text {
        check-args 1 1 'SOMEFILE.plist' "$@" || return 1
        local FILE="$1"
        plutil -convert xml1 -o - "$FILE"
    }
    
    ## Safe-ish deletion settings for macOS
    
    if [[ -f "$HOMEBREW_BIN/trash" ]]; then
        function trash {
            "$HOMEBREW_BIN/trash" -F "$@"
        }
    elif [[ -x "/usr/bin/trash" ]]; then
        function trash {
            /usr/bin/trash "$@"
        }
    fi
    
    # if command-exists trash; then
        # function rm {
        #     trash "$@"
        # }
    # fi

    ## which(1) settings: extend it to make it work for macOS .app bundles
    
    function macwhich {
        local -a DIRS_TO_SEARCH=(
            "$HOME/Applications"
            "/Applications"
            "/Applications/Utilities"
            "/System/Applications"
        )
        
        check-args 1 1 'FOO.app' "$@" || return 1
        local APP_NAME="$1"
        if [[ ! "$APP_NAME" =~ [.]app$ ]]; then
            err "'$APP_NAME' does not end in '.app'.
Maybe you meant 'which \"$APP_NAME\"?'"
            return 1
        else
            for DIR in "${DIRS_TO_SEARCH[@]}"; do
                local QUALIFIED_APP_NAME="$DIR/$APP_NAME"
                if [[ -d "$QUALIFIED_APP_NAME" ]]; then
                    echo "$QUALIFIED_APP_NAME"
                    return 0
                fi
            done
            
            err "couldn't find app '$APP_NAME'. Search path:"
            printf "    %s\n" "${DIRS_TO_SEARCH[@]}" >&2
            echo >&2
            return 2
        fi
    }
    
    function which {
        check-args 1 1 'COMMAND' "$@" || return 1

        local PROGRAM_NAME="$1"
        if [[ "$PROGRAM_NAME" =~ [.]app$ ]]; then
            macwhich "$PROGRAM_NAME"
            MACWHICH_EXIT_STATUS=$?
            if [[ $MACWHICH_EXIT_STATUS -ne 0 ]]; then
                return $MACWHICH_EXIT_STATUS
            fi
        else
            type -P "$PROGRAM_NAME"
        fi
    }

    function exe {
        check-args 1 1 'My.app' "$@" || return 1
        local APP_NAME_INCLUDING_EXT="$1"
        local APP_DIR="$(which "$APP_NAME_INCLUDING_EXT")"
        if [[ ! "$APP_DIR" ]]; then
            err "couldn't find a macOS app named '$APP_NAME_INCLUDING_EXT'"
        else
            local EXE_DIR="$APP_DIR/Contents/MacOS"
            shopt -s nullglob
            local -a EXE_FILES=($EXE_DIR/*)
            shopt -u nullglob
            N_EXE=${#EXE_FILES[@]}
            if [[ "$N_EXE" -eq 0 ]]; then
                err "found no executables in '$EXE_DIR'"
            elif [[ "$N_EXE" -gt 1 ]]; then
                err "found multiple executables in '$EXE_DIR'"
            else
                echo ${EXE_FILES[0]}
            fi
        fi
    }

    function darwin-version {
        uname -r
    }

    function macos-version {
        echo "$(sw_vers -productName) $(sw_vers -productVersion)"
    }

    function macos-version-for-darwin-version {
        check-args 0 2 '[--codename-only | --number-only] DARWIN_VERSION_NUMBER # example: 24.6.5' "$@" \
            || return 1

        local DARWIN_VERSION CODENAME MACOS MACOSVER
        if [[ $# -eq 0 ]]; then
            DARWIN_VERSION="$(darwin-version)" # the currently running version
        else
            DARWIN_VERSION="${@: -1}"
        fi
        
        local DARWIN_MAJOR_VERSION="${DARWIN_VERSION%%.*}"
        case "$DARWIN_MAJOR_VERSION" in
            "1.3") CODENAME="Cheetah"
                   MACOS="Mac OS X"
                   MACOSVER="10.0"
                   ;;
            "1.4"|"1.5") CODENAME="Puma"
                         MACOS="Mac OS X"
                         MACOSVER="10.1"
                         ;;
            "6") CODENAME="Jaguar"
                 MACOS="Mac OS X"
                 MACOSVER="10.2"
                 ;;
            "7") CODENAME="Panther"
                 MACOS="Mac OS X"
                 MACOSVER="10.3"
                 ;;
            "8") CODENAME="Tiger"
                 MACOS="Mac OS X"
                 MACOSVER="10.4"
                 ;;
            "9") CODENAME="Leopard"
                 MACOS="Mac OS X"
                 MACOSVER="10.5"
                 ;;
           "10") CODENAME="Snow Leopard"
                 MACOS="Mac OS X"
                 MACOSVER="10.6"
                 ;;
           "11") CODENAME="Lion"
                 MACOS="Mac OS X"
                 MACOSVER="10.7"
                 ;;
           "12") CODENAME="Mountain Lion"
                 MACOS="Mac OS X"
                 MACOSVER="10.8"
                 ;;
           "13") CODENAME="Mavericks"
                 MACOS="Mac OS X"
                 MACOSVER="10.9"
                 ;;
           "14") CODENAME="Yosemite"
                 MACOS="Mac OS X"
                 MACOSVER="10.10"
                 ;;
           "15") CODENAME="El Capitan"
                 MACOS="Mac OS X"
                 MACOSVER="10.11"
                 ;;
           "16") CODENAME="Sierra"
                 MACOS="macOS"
                 VERSION="10.12"
                 ;;
           "17") CODENAME="High Sierra"
                 MACOS="macOS"
                 VERSION="10.13"
                 ;;
           "18") CODENAME="Mojave"
                 MACOS="macOS"
                 VERSION="10.14"
                 ;;
           "19") CODENAME="Catalina"
                 MACOS="macOS"
                 VERSION="10.15"
                 ;;
           "20") CODENAME="Big Sur"
                 MACOS="macOS"
                 VERSION="11"
                 ;;
           "21") CODENAME="Monterey"
                 MACOS="macOS"
                 VERSION="12"
                 ;;
           "22") CODENAME="Ventura"
                 MACOS="macOS"
                 VERSION="13"
                 ;;
           "23") CODENAME="Sonoma"
                 MACOS="macOS"
                 VERSION="14"
                 ;;
           "24") CODENAME="Sequoia"
                 MACOS="macOS"
                 VERSION="15"
                 ;;
           "25") CODENAME="Tahoe"
                 MACOS="macOS"
                 VERSION="26"
                 ;;
              *) err "unrecognized Darwin version '${DARWIN_MAJOR_VERSION}'; update this bash function"
                 return 2
                 ;;
        esac

        if [[ $# -eq 0 ]]; then
            MACOS="$(sw_vers --productName)"
            VERSION="$(sw_vers --productVersion)"
        fi

        _format-macos_version "$CODENAME" "$MACOS" "$MACOSVER"
    }

    function _format-macos-version {
        check-args \
            3 4 \
            '[--codename-only | --macos-only] MACOS CODENAME VERSION' "$@" \
          || return 1

        local FLAG MACOS CODENAME VERSION

        if [[ $# -eq 3 ]]; then
            FLAG="$1"
            shift
        fi

        MACOS="$1"
        CODENAME="$1"
        VERSION="$2"

        case "$FLAG" in
            "") echo "$CODENAME ($MACOS $VERSION)";;
            "--codename-only") echo "$CODENAME";;
            "--macos-only") echo "$MACOS $VERSION";;
            *) err "invalid option '$FLAG'"; return 1;;
        esac
    }
    
    function uname {
        if [[ $# -eq 1 ]] && [[ "$1" = "-x" ]]; then
            print-macos-version-codename
        else
            command uname "$@"
        fi
    }

#### OS-specific settings for Linux and other non-macOS Unix variants

else
    function which {
        type -P "$@"
    }
    if command-exists xdg-open; then
        function open {
            xdg-open "$@"
        }
    fi

    ## When on macOS, prefer Homebrewed GNU tools to their built-in
    ## equivalents, iff they're available

    for GNU_COMMAND in cut date dircolors find head roff sed sort stat tac tail
    do
        BREWED_GNU="${HOMEBREW_BIN}/g${GNU_COMMAND}"
        if [[ -e "$BREWED_GNU" ]]; then
            # FIXME: this code is not being reached, somehow
            eval "function $GNU_COMMAND { $BREWED_GNU \"$@\" ;}"
            # exalias "$GNU_COMMAND" "$BREWED_GNU"
        fi
    done
fi

#### grep

source-section "02-grep.bash" ### grep (or rg, if installed). Lots of customizations.

#### fd

source-section "03-fd.bash" ### fd, a fast, modern replacement for find(1)

#### .ignore files

source-section "dot-ignore.bash" ### helper functions for the .ignore files honored by git, fd, and rg

#### Terminal emulators

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

function title {
    echo -ne "\033]2;" "$1" "\007"
}

#### iTerm2-specific terminal settings

# See https://iterm2.com/documentation-captured-output.html
function cl {
    printf "\e]1337;ClearCapturedOutput\e\\"
}

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

#### Terminal color settings

export COLORTERM="truecolor"
export CLICOLOR=1

#### Convenience functions for printing ANSI SGR 4-bit color codes

set -a
QANSI='\033['
QRESET='\033[0m'

set +a

function color-printf {
    local HELP="COLOR_NAME [STRING ...]

For example, color-printf bright-cyan 'Color text' 'and more color text\n'.
"

    check-args 2 varargs "$HELP" "$@" || return 1
    local COLOR_NAME="$1"
    shift

    local COLORCODE BRIGHTNESS
    case "$COLOR_NAME" in
        black)    COLORCODE=30; BRIGHTNESS=0;;
        red)      COLORCODE=31; BRIGHTNESS=0;;
        green)    COLORCODE=32; BRIGHTNESS=0;;
        yellow)   COLORCODE=33; BRIGHTNESS=0;;
        blue)     COLORCODE=34; BRIGHTNESS=0;;
        magenta)  COLORCODE=35; BRIGHTNESS=0;;
        cyan)     COLORCODE=36; BRIGHTNESS=0;;
        white)    COLORCODE=37; BRIGHTNESS=0;;
  
        bright-black)    COLORCODE=90; BRIGHTNESS=1;;
        bright-red)      COLORCODE=91; BRIGHTNESS=1;;
        bright-green)    COLORCODE=92; BRIGHTNESS=1;;
        bright-yellow)   COLORCODE=93; BRIGHTNESS=1;;
        bright-blue)     COLORCODE=94; BRIGHTNESS=1;;
        bright-magenta)  COLORCODE=95; BRIGHTNESS=1;;
        bright-cyan)     COLORCODE=96; BRIGHTNESS=1;;
        bright-white)    COLORCODE=96; BRIGHTNESS=1;;
  
        *) echo "Unsupported color '$COLOR_NAME'" >&2; return 2;;
    esac

    printf '\e[%d;%dm' "$BRIGHTNESS" "$COLORCODE"
    printf "$@"
    printf '\033[0m'
}

function bash-color-test-pattern {
    for COLORNAME in black red green yellow blue magenta cyan white; do
        for PREFIX in "" "bright-"; do
            FULLCOLORNAME="${PREFIX}${COLORNAME}"
            color-printf "$FULLCOLORNAME" "$FULLCOLORNAME"
            echo
        done
    done
}

# bnw (Black 'n' White): remove non-printable chars,
# such as ANSI color codes. Works as a filter, e.g. 'git diff a b | bnw'

function bnw {
    while IFS="" read -r LINE; do

        # Strip ANSI color codes
        CLEAN_LINE="$(echo "$LINE" | sed -r 's/\x1B\[[0-9;]*[a-zA-Z]//g')"

        # Strip control characters
        CLEAN_LINE="$(echo "$CLEAN_LINE" | tr -d '[:cntrl:]')"
        
        echo "$CLEAN_LINE"
    done
}

# Take a hex triplet, and print it in the indicated color.
# Requires a terminal that supports (ANSI SGR) 24-bit color,
# a.k.a. true color

function hex {
    if [[ $# -ne 1 ]]; then
        err "Wrong number of arguments. Usage: hex RRGGBB"
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

#### Bash prompt settings

unset PROMPT_COMMAND
PS1_LOGO=""

# # Use Unicode emoji as visual cues for different hosts/apps
# if [[ "$INSIDE_EMACS" ]]; then
#     PS1_LOGO=$'\u2130' # Cursive uppercase E
# elif [[ -n "$LAPTOP_HOSTNAME" ]] && [[ $HOSTNAME = "$LAPTOP_HOSTNAME" ]]; then
#     PS1_LOGO=$'\uF8FF' # Apple logo
# fi
# export PS1="\\[\\e[33m\\]${PS1_LOGO}\\u@${PS1_HOSTNAME}:\\w\\$\\[\\e[0m\\] "

export PS1="\\[\\e[33m\\]\\u@\\h:\\w\\$\\[\\e[0m\\] "
export DEFAULT_PS1="$PS1"

# macOS: suppress nag message about Bash being deprecated in favor of zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

#### Environment variable functions

function enva {
    check-args 0 0 "[no arguments]" "$@" || return 1
    env | ggrep --color=always -P '^AWS_[^=]*' | command grep -vE '(AWS_DEV_ACCOUNT)|(AWS_PAGER)' | sort
}

function envp {
    check-args 0 0 "[no arguments]" "$@" || return 1
    env | ggrep --color=always -P '^PG_[^=]*' | sort
}

function envap {
    check-args 0 0 "[no arguments]" "$@" || return 1
    enva
    envp
}

function xkcd-automate-p {
    local XKCD_CHART="$HOME/doc/is_it_worth_the_time_2x.png"
    if [[ -f "$XKCD_CHART" ]]; then
        open "$XKCD_CHART"
    else
        echo "Missing file '$XKCD_CHART'. Go grab it from https://xkcd.com/1205/"
    fi
}

function xkcd {
    check-args 1 1 'COMIC_NUMBER' "$@" || return 1
    local COMIC_NUMBER="$1"
    open "https://xkcd.com/$COMIC_NUMBER"
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
    
    local DASHIFIED="$(echo "$ABSOLUTE" | sed 's|^/|| ; s|[/]|---|g')"
    echo "$DASHIFIED"
}

function cdd { cd ~/Desktop/$1 ;}
function cdq { cd ~/src/1/qdw-dotfiles/$1 ;}
function cd3 { cd ~/src-3/$1 ;}
function cd1 { cd ~/src/1/$1 ;}

# If the argument is a symlink, resolve it and then cd to the enclosing dir
# of the file that it point to.
#
# If the user passes no arguments, then do that to the directory portion
# of the previous argument passed ($_).
function cdl {
    local MANNAME='cd to the enclosing dir of a file (or of $_ if none passed)'
    local ARG  REALPATH
    
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

    set +x
}

#### Bash history settings

set -a

# Add a timestamp to history(1) output, using strftime(3) formatting.
HISTTIMEFORMAT='  %Y-%m-%d %H:%M:%S %z    '

# Keep all history forever
HISTFILESIZE=-1
HISTSIZE=-1

set +a

## Bash completion and prompt settings
## Dependencies:
##     - Homebrew itself
##     - Homebrewed bash >= 5 (set as your login shell)
##     - The following additional Homebrew formulas:
##         - bash-completion@2
##         - gh
##         - docker
##         - rclone
##         - openshift
##
##     - bash-git-prompt, preferably my fork: https://github.com/qdw/bash-git-prompt

set -a
GIT_PROMPT_THEME="Custom"
GIT_PROMPT_ONLY_IN_REPO=1
set +a

if [[ -r "$HOMEBREW_ROOT/etc/profile.d/bash_completion.sh" ]]; then
    function use-gitprompt {
        local OPTIONAL_WORKTREE="$1"
    
        local MY_FORKED_SCRIPT="$HOME/src/forks/bash-git-prompt/gitprompt.sh"
        local ORIGINAL_SCRIPT="$HOMEBREW_ROOT/share/gitprompt.sh"

        if [[ -f "$MY_FORKED_SCRIPT" ]]; then
            if [[ "$OPTIONAL_WORKTREE" ]]; then

                # (function get-branch-name-bounds is defined in ~/.site.bash.d/preamble.bash)
                local BOUNDS="$(get-branch-name-bounds "$OPTIONAL_WORKTREE")"
            
                if [[ "$BOUNDS" ]]; then
                    read OFFSET LIMIT <<< "$BOUNDS"
                    export GIT_PROMPT_BRANCH_OFFSET="$OFFSET"
                    export GIT_PROMPT_BRANCH_LIMIT="$LIMIT"
                fi
            fi
            source "$MY_FORKED_SCRIPT"
        else
            source "/opt/homebrew/share/gitprompt.sh"
        fi
    }

    export BASH_COMPLETION_DIR="$HOMEBREW_ROOT/etc/bash_completion.d"
    # comp - activate shell completion
    function comp {
        function sc {
            for FILENAME in "$@"; do
                source "$BASH_COMPLETION_DIR/$FILENAME"
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
                use-gitprompt "$(realpath "$PWD")"
                sc "gh" \
                   "git-completion.bash"
                ;;

            m|macos|macOS)
                sc "m"
                ;;

            ollama)
                source-if-exists "$HOME/src-3/ollama-bash-completion-plugin/plugin.sh"
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
        local REALPATH="$(realpath "$PWD")"
        
        if [[ -d "$REALPATH/.git" \
              && ( ! "$REGEX_FOR_REPOS_TO_EXCLUDE_FROM_GIT_COMPLETION" \
                       || \
                   ! "$REALPATH" =~ $REGEX_FOR_REPOS_TO_EXCLUDE_FROM_GIT_COMPLETION ) ]];
        then
            use-gitprompt "$REALPATH"
            if [[ ! "$GIT_COMPLETION_ACTIVATED" ]]; then
                my-source "$BASH_COMPLETION_DIR/gh"
                my-source "$BASH_COMPLETION_DIR/git-completion.bash"
                export GIT_COMPLETION_ACTIVATED=1
            fi
        fi
    }
    
    MY_PROMPT_FUNCTION_DEFINITION=$(command -V my_prompt_function | tail -n +2)
    export PROMPT_COMMAND="$MY_PROMPT_FUNCTION_DEFINITION; my_prompt_function"
fi

#### Scheme Shell settings

if [[ -d "/opt/scsh" ]]; then
    SCSH_BASE="/opt/scsh"
    SCSH_PATH="${SCSH_BASE}/bin"
    LATEST_SCSH_VERSION="$(ls "${SCSH_BASE}/lib" | sort --general-numeric-sort | tail -1)"
    SCSH_LDFLAGS="-L${SCSH_BASE}/lib/${LATEST_SCSH_VERSION}"
fi

#### String manipulation

source-section "04-string-manipulation.bash" ### String manipulation

#### Pager (GNU less)

source-section "05-pager.bash" ### Pager (GNU less)

#### Makefiles

function make-targets {
    gsed -e 's/^[^:]*$// ; /^$/d ; s/\(.*\):.*/\1/' Makefile | sort
}

function make-rules {
    command grep -E --color=never '[[:alpha:]]+:' Makefile
}

#### Markdown

function markdown2html {
    pandoc "$1" -t html
}

#### AI/LLMs/ML

for MODEL_DIR in "$HOME/ai/models"/*; do
    MODEL_NAME="$(basename "$MODEL_DIR")"
    if [[ $MODEL_NAME =~ [a-zA-Z]+ ]]; then
        SHORT_NAME="${BASH_REMATCH[0]}"
        LC="${SHORT_NAME,,}"
    
        OLLAMA_FUNCNAME="$LC-ollama"
        LLAMAFILE_FUNCNAME="$LC-llamafile"

        eval "
            function $OLLAMA_FUNCNAME {
                ollama run $MODEL_NAME \"\$@\"
            }

            function $LLAMAFILE_FUNCNAME {
                llamafile -m \"$HOME/ai/models/${MODEL_NAME}/${MODEL_NAME}.gguf\" \"\$@\"
            }
        "
    fi
done

function ai {
    tmux attach-session -t ai
}

function mistral {
    Mistral-7B-Instruct-v0.3.Q4_0-ollama "$@"
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

MODEL_DIR="$HOME/ai/weights"

function aws-account {
    echo "$(AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text)"
}

function clear-env {
    unset AWS_ACCOUNT \
          AWS_ACCESS_KEY_ID \
          AWS_SECRET_ACCESS_KEY \
          AWS_SESSION_TOKEN \
          PGDATABASE \
          PGHOST \
          PGPASSWORD \
          PGPORT \
          PGSERVICE \
          PGUSER
}

function gguf {
    local MANNAME="show ollama GGUF files (weights) for all installed models"
    check-args 0 0 '[no arguments]' "$@" || return 1
    
    for MODEL_NAME in $(ollama ls | gtail -n +2 | awk '{print $1}'); do
        local BLOB
        BLOB="$(ollama show --modelfile "$MODEL_NAME" \
                    | command grep '^FROM' \
                    | sed 's/^FROM[ ]//')"
        echo "$BLOB $MODEL_NAME"
    done
}

function link-ggufs {
    check-args 0 2 '[--verbose] [--force]' "$@" || return 1

    local VERBOSE FORCE
    while [[  $# -gt 0 ]]; do
        case "$1" in
            --verbose) VERBOSE="yes"; shift;;
            --force) FORCE="yes"; shift;;
            *) err "invalid option '$1'"; return 1;;
        esac
    done    

    local ORIG_DIR
    ORIG_DIR="$(pwd)" || return
    cd "$MODEL_DIR" || return

    while read -r BLOB MODEL_NAME; do
        local MODEL_NAME="$(echo $MODEL_NAME | sed -e 's/:.*//')"
        
        if [[ "$VERBOSE" ]]; then
            _stderr "$MODEL_NAME -> $BLOB"
        fi

        if [[ "$FORCE" ]]; then
            ln -sf "$BLOB" "$MODEL_NAME"
        else
            ln -s "$BLOB" "$MODEL_NAME"
        fi
    done < <(gguf)

    cd "$ORIG_DIR" >/dev/null
}

### AWS IAM settings

function iam-provision-pgpassword {
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

function cdk {
    case "$1" in
        diff | deploy) SUBCOMMAND="$1"
                       shift
                       command cdk "$SUBCOMMAND" --quiet "$@" \
                           2> >(grep -v "Hold on while" >&2)
                       stty sane;;
        *) command cdk "$@";;
    esac
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

# rds-ls ("RDS LiSt") - list RDS instances (highlighting the hostname portion of the FQDN)
function rds-ls {
    check-args 0 1 'rds-ls [HOSTNAME_REGEX]' "$@" || return 1

    if [[ $# -eq 1 ]]; then
        local REGEX="$1"
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

#### AWS SSM (a.k.a. Secrets Manager) settings

function list-secrets {
    check-args 0 varargs '[GREP_REGEX]' "$@" || return 1
    local REGEX="$1" # may be ""
    shift # remaining arguments (if any) are flags to pass to grep

    function list-all-secrets {
        aws secretsmanager list-secrets \
            | jq -r '.SecretList[].Name' \
            | sort -u
    }

    if [[ "$REGEX" ]]; then
        list-all-secrets | command grep -E "$@" "$REGEX"
    else
        list-all-secrets
    fi
}

function get-secret {
    aws secretsmanager get-secret-value --secret-id "$1" \
        | jq '.SecretString | fromjson'
}

function secret { get-secret "$@" ;}

### AWS logs/CloudWatch/LogGroup/log group settings

function rds-ls-log-groups {
    set -o pipefail

    aws logs describe-log-groups --log-group-name-prefix "/aws/rds" \
        | command grep logGroupName \
        | command grep -v "proxy" \
        | sort \
        | sed -e 's|.*/aws/rds/instance/||; s|/postgresql",$||'
    
    set +o pipefail
}

### AWS S3 setttings

function in-dev {
    # (AWS_DEV_ACCOUNT_NUMBER must be defined in preamble.bash)
    if ! [[ "$AWS_ACCESS_KEY_ID" && "$AWS_SECRET_ACCESS_KEY" && "$AWS_SESSION_TOKEN" ]]; then
        err "AWS_ACCOUNT is defined, but not the auth-related AWS_* env vars"
        return 1
    else
        local ACCOUNT_NUMBER="$(AWS_PAGER=cat aws sts get-caller-identity --query "Account" --output text)"
        if [[ "$ACCOUNT_NUMBER" != "$AWS_DEV_ACCOUNT_NUMBER" ]]; then
            err "You're logged into '$ACCOUNT_NUMBER', but the dev account is '$AWS_DEV_ACCOUNT_NUMBER'"
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
        err "not in dev; refusing to delete data!"
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
        
        aws s3 rm --recursive "s3://${BUCKET_NAME}" >/dev/null \
            && aws s3api delete-bucket --bucket "$BUCKET_NAME"
    fi
}


## FIXME: find a more elegant way
# (This function depends on the third-party newtab() function;
# see $QBASHD/third-party.bash)
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

# export BAT_THEME="tritium" # can't get 'bat cache --build' to work
export BAT_THEME="base16"
function bat {
    command bat \
        --color "always" \
        --decorations "auto" \
        --style "rule,snip" \
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

# Save-to-file a function that you defined in an interactive shell
function save-function {
    check-args 1 2 'BASH_FUNCTION_NAME [FILENAME] # default: ~/.bashrc' "$@" \
        || return 1;

    local FUNCTION_NAME="$1"
    local DEST_FILE="${2:-$HOME/.bashrc}"
    
    echo >> "$DEST_FILE"
    func "$FUNCTION_NAME" | bnw >> "$DEST_FILE"
}

# Search for functions whose name match a regex
function gfunc {
    check-args 1 varargs '[GREP_FLAGS ...] REGEX' "$@" || return 1
    local -a ARGS=("$@")
    local REGEX="${ARGS[-1]}"
    unset ARGS[-1]
    gbr ${ARGS[@]} "function.*${REGEX}.*[{]"
}

function g-funk { gfunc "$@" ;}

# bat-head FILENAME NUMBER: use color bat to show a file's first n lines
function bat-head {
    local FILENAME="$1"
    local NUMBER="$2"
    
    bat --line-range ":${NUMBER}" "$FILENAME"
}

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub).

function bbat {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
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
        ABS_PATH="$(echo "$ABS_PATH" | sed -e "s|^${HOME}/|~/|")"

        echo "$ABS_PATH" | tee >(pbcopy)
    fi
}

# qp ("qdw-dotfiles Push")
function qp { cdq && git add . && git commit && git push && cd - ;}

#### Improvements on core Unix commands

if command-exists rsync; then
    function cp {
        check-args \
            2 varargs \
            'cp FILE_1 FILE_2 | cp FILE_1 [FILE_2 [...]] DIR # uses rsync' \
            "$@" \
        || return 1
        
        rsync -aq "$@"
    }
fi

#### ls settings

# Note: LS_COLORS is specific to GNU ls (macOS's built-in BSD ls uses
# LSCOLORS instead). For the meanings of these two-letter codes, run
# dircolors --print-database

export LS_COLORS='rs=0:di=34:ln=32:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=91:mi=35:su=30;31:sg=30;43:ca=00:tw=30;42:ow=39;42:st=37;44:ex=93:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90'

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
    ls "$@" ~/Desktop
}

function recent {
    check-args 0 0 '[no arguments]'
    $MY_LS $MY_LS_BASE_OPTIONS -lt | head -5
}

#### diff settings

function dcc { colordiff -u "$@" | $PAGER ;}

HOMEBREW_DIFF="$HOMEBREW_ROOT/opt/diffutils/bin/diff"
if [[ -f "$HOMEBREW_DIFF" ]]; then
    WE_HAVE_GNU_DIFF=0
    export DIFF_PATH="/opt/homebrew/opt/diffutils/bin"

    # Use custom colors, which are supported by GNU grep but not by macOS grep:
    # 
    # ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
    # de - deleted lines (31 is SGR red, which shows as orange in my terminal)
    # hd - header        (35 is SGR magenta, which shows as magenta in my term)
    # ln - line numbers  (34 is SGR blue, which shows as blue in my term)
    # rs - undocumented, but the default seems fine
    # 
    MY_GNU_DIFF_OPTIONS='--palette="ad=36:de=31:hd=35:ln=34:rs=0"'
    function gdiff { "$DIFF_PATH/diff" "$@" ;}
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

# function alt-diff
#     # Git diff
#     git diff --no-index "$@"
    
#     ### Other available diff tools, FYI
#     ### local COMMON_DIFF_OPTIONS="-u --color=always"
#     # 
#     ## Recent GNU diff
#     # 
#     #   Color legend:
#     #   ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
#     #   de - deleted lines (31 is SGR red, which shows as orange in my terminal)
#     #   hd - header        (35 is SGR magenta, which shows as magenta in my term)
#     #   ln - line numbers  (34 is SGR blue, which shows as blue in my term)
#     #   rs - undocumented, but the default seems fine
#     # 
#     # command diff $COMMON_DIFF_OPTIONS --palette="ad=36:de=31:hd=35:ln=34:rs=0" "$@" | $PAGER
#     # 
#     ## Old GNU diff (shipped with macOS as /usr/bin/diff)
#     #
#     # command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
# }

GIT_MERGE_HELP_URL="https://howchoo.com/git/git-merge-conflicts-rebase-ours-theirs"

function ours {
    open "$GIT_MERGE_HELP_URL"
}

function theirs {
    ours "$@"
}

function diff {
    # I just like git diff's syntax coloring better.
    # --no-index allows you to use it anywhere on files,
    # without regard to their history, just like
    # regular diff.
    git diff --no-index "$@"
}

#### FTP

set -a
FTP_LOGIN=anonymous
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=""
set +a

#### Git, GitHub, gitlawb

source-section "06-git.bash" ### Git, GitHub, gitlawb

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

# source "$QBASHD/perl.bash"

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

#### PostgreSQL

# For additional PostgreSQL-realted settings, see the following files:
#    ~/.psqlrc*
#    ~/.pg_service
#    ~/.pgpass

source "$HOME/.bashrc.d/postgresql.bash"

declare -Ax pgpasses

function pql {
    check-args 1 1 '"stanza name from pg_service.conf"' "$@" || return 1
    
    local SERVICE  PASSWORD
    if [[ $# -ne 1 ]]; then
        err "missing argument. Usage: pql pg_service.conf-stanza-name | psql --clear-cache"
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
                    err "couldn't retrieve the AWS secret for $SERVICE" 1>&2;
                    return 2
                fi
            fi

            pgpasses["$SERVICE"]="$PASSWORD"
            echo "$PASSWORD"
        fi
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

function snip {
    local SNIPPET_DIR="$HOME/sql-snippets"

    if [[ ! -d "$SNIPPET_DIR" ]]; then
        err "no such directory '$SNIPPET_DIR'"
        return 2
    elif [[ $# -eq 0 ]]; then
        echo "Available snippets:"
        echo
        command ls -1 "$SNIPPET_DIR"
    elif [[ $# -eq 1 ]]; then
        if [[ "$PGSERVICE" ]]; then
            qql "$SNIPPET_DIR"
        else
            err "you must set the PGSERVICE env var"
            return 3
        fi
    fi
}

#### IRL settings

## PurpleAir air quality monitor settings

function air {
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
}

#### Convenience functions for working with local files

function buy {
    local MANNAME="list or navigate to sites for return purchases"
    check-args 0 1 '[PRODUCT_NAME]' "$@" || return 1
    
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
            err "don't know how to buy '$PRODUCT_NAME'"
            list-things-you-can-buy /dev/stderr
            return 3
        else
            local URL="$(cat "$URL_FILE")"
            CAT_EXIT_STATUS=$?
            if [[ $CAT_EXIT_STATUS -ne 0 ]]; then
                return $CAT_EXIT_STATUS
            elif [[ -z "$URL" ]]; then
                err "URL file '$URL_FILE' is empty"
                return 4
            else
                firefox "$URL"
            fi
        fi
    fi
}

function todo {
    check-args 1 varargs 'todo MY TODO TEXT [...]' "$@" || return 1

    if [[ "$GTD_INBOX_PATH" ]]; then
        echo "* INBOX" "$@" >> "$GTD_INBOX_PATH"
    else
        err "Env var GTD_INBOX_PATH is not defined"
        return 2
    fi
}

#### Python

for PREFERRED in "$HOMEBREW_BIN/python3" "/usr/bin/python3"; do
    if [[ -f "$PREFERRED" ]]; then
        PYTHON_INTERPRETER_PATH="$PREFERRED"
        break
    fi
done

function python {
    python3 "$@"
}

function defs {
    # Show Python function names
    check-args 0 varargs \
               '[GREP_ARG_1 [GREP-ARG_2 ...]] [DIRECTORY] # defaults to .' \
               "$@" \
        || return 1

    # Get the last argument, and remove it from $@
    local PTH
    if [[ $# -ne 0 ]]; then
        local PTH="${@[$# - 1]}"
        unset ${@[$# - 1]}
        echo "PTH: $PTH"
        echo "remaining args: $@"
    else
         PTH="."
    fi
    
    command fd -t f '.py$' \
        -x command grep -RiE --color=always "$@"  "^[ ]*def[ ]"  "{}" \; \
        "$PTH"
}

#### Aranet4 air quality monitoring settings

function co2  {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read "$MY_ARANET4" 2> /dev/null | jq ".co2"
}

function co2-verbose {
    echo "CO2 was $(co2) ppm at $(date '+%I:%M %P') on $(d)"
}

#### Tor

function tor-curl { curl --socks4a 127.0.0.1:9050 "$@" ;}
function torcurl { tor-curl "$@" ;}

#### TypeScript/Deno

export CDK_DISABLE_CLI_TELEMETRY=true

# (You can't do this with a shebang, hence the need for a function):
# dr ("Deno Run") - run some code via the Deno runtime.
function dr { deno run --quiet "$@" ;}

# dl ("Deno with Local permissions") - run some code in the Deno runtime, and allow the code to access the network and read the local filesystem, without prompting me for permission.
function dl { deno run --quiet --allow-net --allow-read "$@" ;}

function dc { deno eval --check "$@" ;}

# dcr - run Deno, but first do type-checking (no longer the default as of Deno v1.23)
function dcr { deno run --check "$@" ;}

#### Ring tones

# Download the audio from a YouTube video and convert it to an m4r.

function yt {
    check-args 2 3 'URL OUTPUT_FILENAME_WITHOUT_EXTENSION [OUTPUT_FORMAT]' "$@" || return 1
    local URL="$1"
    local OUTPUT_BASENAME="$2"
    local OUTPUT_FORMAT="${3:-m4a}"
    local OUTPUT_FILENAME="${OUTPUT_BASENAME}.${OUTPUT_FORMAT}"

    yt-dlp \
        --cookies "$HOME/.yt-dlp-cookies" \
        --format "bestaudio[ext=m4a]" \
        --extract-audio --audio-format "$OUTPUT_FORMAT" --audio-quality "0" \
        -o "$OUTPUT_FILENAME" \
        "$URL" \
        || return

    if [[ "$OUTPUT_FORMAT" = "m4a" ]]; then
        mv "$OUTPUT_FILENAME" "$OUTPUT_BASENAME.m4r"
    fi
}

function mp3 {
    check-args 2 2 'URL OUTPUT_FILENAME_WITHOUT_EXTENSION' "$@" || return 1
    yt "$@" "mp3"
}

# Download an MP3 from zophar.net (video game sounds) and convert it to an m4r.

function zo {
    check-args 2 2 'URL OUTPUT_FILENAME' "$@" || return 1
    local URL="$1"
    local OUTPUT_FILENAME="$2"
    if [[ ! $OUTPUT_FILENAME =~ .mp3$ ]]; then
        OUTPUT_FILENAME="${OUTPUT_FILENAME}.mp3"
    fi

    curl -L "$URL" -o "$OUTPUT_FILENAME" \
        && mp3-to-m4r "$OUTPUT_FILENAME" \
        && command trash -F "$OUTPUT_FILENAME" # the mp3
}

# Conver an MP3 to an m4r (iPhone ring tone file).

function mp3-to-m4r {
    check-args 1 1 'FILENAME' "$@" || return 1
    local FILENAME="$1"
    local BASE="$(basename -s .mp3 "$FILENAME")"
    
    ffmpeg -i "$FILENAME" \
           -c:a aac \
           -b:a 256k \
           "$BASE.m4a" \
        && mv "$BASE.m4a" "$BASE.m4r"
}

#### golang

export GOPATH="$HOME/go"

#### Lua

LUAROCKS_PATH=~/.luarocks/bin

# We need this old version in order to compile Moonscript itself
LUA51_BASE="/opt/lua-5.1.5"
LUA51_PATH="${LUA51_BASE}/bin"
LUA51_LDFLAGS="-L${LUA51_BASE}/lib"
LUA51_CPPFLAGS="-I${LUA51_BASE}/include"

#### Ruby

if [[ -d "$HOMEBREW_ROOT/opt/ruby" ]]; then
    set -a
    # RUBYOPT=rubygems
    RUBY_PATH=$HOMEBREW_ROOT/opt/ruby/bin
    RUBY_LDFLAGS='-L$HOMEBREW_ROOT/opt/ruby/lib'
    RUBY_CPPFLAGS='-I$HOMEBREW_ROOT/opt/ruby/include'
    set +a
fi

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
function gemi {
    sudo gem install --remote \
                     --rdoc \
                     --ri \
                     "$@"
}

#### Rust

export RUST_PATH=~/.cargo/bin
source "$HOME/.cargo/env"

#### Network

function ips {
    /sbin/ifconfig -a \
        | command grep --color=never 'inet ' \
        | command grep --color=never -v 'inet6 ' \
        | grep 'inet'
}

function ipv6 {
    /sbin/ifconfig -a \
        | grep 'inet6 '
}

function whois {
    carp "warning: whois is deprecated; using rdap instead"
    rdap "$@"
}

#### Miscellaneous

# Change this terminal's background to the hex triplet passed in.
function bghex {
    # Example: 'bghex ff0000' to use a red background.
    printf '\e]11;%s\a' "#${1}"
}

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

#### Processes

function pstree {
    command pstree -g 3 -u $USER "$@" | $PAGER
}

function p {
    check-args 1 varargs '[GREP_FLAGS] REGEX' "$@" || return 1
    
    ps auxwwwwww \
        | command grep -vE 'grep|rg' \
        | $MY_GREP $MY_GREP_OPTIONS $GREP_COLOR_FLAGS "$@" \
        | $PAGER
}

# Display only processes that belong to me.
function pu { p "$USER" ;}

#### vim

function vimacs {
    # Run vim with settings that make its behavior and keybindings
    # resemble those of Emacs:
    command vim -u "$XDG_CONFIG_HOME/vim/vimacsrc" "$@"
}

# vl ("Vim Less") - use vim as a colorizing pager
function vl {
    "$HOMEBREW_ROOT/share/vim/vim91/macros/less.sh" "$@"
}

#### VMs

if on-macos && installed "qemu-system-arch64" >/dev/null 2>&1; then
    function vm {
        local INSTALLATION_ISO="/Users/quinn.weaver/vm/iso/debian-13.2.0-arm64-netinst.iso"

        qemu-system-aarch64 \
            -M virt,accel=hvf \
            -cpu host \
            -drive if=pflash,format=raw,file=/opt/homebrew/share/qemu/edk2-aarch64-code.fd,readonly=on \
            -drive if=pflash,format=raw,file=efivars.fd \
            -drive file=disk.img,format=qcow2,if=virtio,cache=writeback \
            -smp 2 \
            -m 2G \
            `# initial installation only: CD-ROM/ISO for installation; omit after initial install` \
            -drive "file=${INSTALLATION_ISO},media=cdrom,if=virtio,readonly=on" \
            `# initial installation only: boot order (d=cdrom, c=disk, n=network; c is the default)` \
            -boot order=dc,menu=on \
            `# user-mode networking with SSH port forwarding; no root needed; default has no port forwarding so you can't SSH in` \
            -netdev user,id=net0,hostfwd=tcp::2244-:22 \
            `# companion to -netdev: attaches virtio NIC to the network backend defined above; without a -device the -netdev does nothing` \
            -device virtio-net-pci,netdev=net0 \
            `# make the virtio RNG feed the guest entropy pool. Without this, /dev/random in the guest can starve during boot` \
            -device virtio-rng-pci \
            `# Recommended: exposes a host directory to the guest via 9p; mount in guest with: mount -t 9p -o trans=virtio hostshare /mnt; omit if you don't need host<->guest file sharing` \
            `## -virtfs local,path=/path/to/shared,mount_tag=hostshare,security_model=mapped-xattr,id=fs0` \
            `# Recommended for headless VMs: disables graphical window and multiplexes serial+monitor onto stdio; default opens a graphical window (useless for a server VM)` \
            -nographic \
            `# Optional: redirects serial console to stdio (use with -nographic); 'mon:stdio' also gives you the QEMU monitor via Ctrl-A C` \
            -serial mon:stdio \
            `# Optional: exposes QEMU monitor on a Unix socket so you can manage the VM programmatically; without this you only get monitor access via the serial mux above` \
            -monitor unix:qemu-monitor.sock,server,nowait \
            `# Optional: writes the QEMU process PID to a file; useful for scripted stop/start; no PID file written by default` \
            -pidfile qemu.pid \
            `# Optional: sets the VM name shown in process list and window title; defaults to the qemu binary name` \
            -name my-vm \
            `# Optional: sets guest RTC to UTC; default is also utc but some guides use localtime, which causes clock drift issues on Linux guests` \
            -rtc base=utc \
            `# Optional: on guest-initiated reboot, exit QEMU instead of rebooting; useful in provisioning scripts where you want to detect reboot as a completion signal` \
            -no-reboot \
            `# Optional: forks QEMU to background after startup; default runs in foreground`
    }
fi

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
#                 err "unsupported set operation '$OP'"
#                 return 255 ;;
#         esac
#     fi
# }            

# function set-intersection {
#     check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
#     comm -12 "$1" "$2"
# }

function set-difference {
    check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
    comm -23 <(sort -u "$1") <(sort -u "$2")
}

# function set-disjunctive-union {
#     set-difference "$@"
# }

# function set-union {
#     check-args 2 2 'SET_1_FILE SET_2_FILE' "$@" || return 1
#     cat "$1" "$2" | sort | uniq
# }

#### Slack

function slack-channel {
    check-args \
        1 \
        1 \
        "slack-channel CHANNEL_NAME # (with or without the leading '#'; doesn't matter)" \
        "$@" \
     || return 1

    # Remove leading '#' if present
    local CHANNEL_NAME="${1/##/}"
    
    if on-macos; then
        macos-open-slack-channel.scpt "$CHANNEL_NAME"
    else
        open "https://${SLACK_TEAM}.slack.com/channels/${CHANNEL_NAME}"
    fi
}

function channel {
    slack-channel "$@"
}

SLACK_EXTRAS="$HOME/.site.bash.d/slack-urls.bash"
if [[ -f "$SLACK_EXTRAS" ]] && (( BASH_VERSINFO[0] >= 5 )); then
    source "$SLACK_EXTRAS"

    function msg {
        check-args 1 1 'SLACK_USERNAME' "$@" || return 1
        URL="$(slack-username2url "$1")"
        if [[ $URL ]]; then
            open "$URL"
        fi
    }

    function slack-username2url {
        check-args 1 1 'SLACK_USERNAME # e.g. alice.hacker' "$@" || return 1
        local SLACK_USERNAME="$1"

        local URL="${SLACK_USERNAME2URL["$SLACK_USERNAME"]}"
        if [[ -z "$URL" ]]; then
            error "the URL for $SLACK_USERNAME is unknown"
            error "Add it to var SLACK_USERNAME2URL in file $SLACK_EXTRAS"
            return 2
        else
            echo "$URL"
        fi
    }
fi

function nanp {
    check-args 1 1 'AREA_CODE' "$@" || return 1
    local AREA_CODE="$1"

    local CVS_URL="https://reports.nanpa.com/public/npa_report.csv"
    local CACHED_COPY="$HOME/.site.bash.d/npa_report.csv"
    if [[ ! -f "$CACHED_COPY" ]]; then
        local CURL_ES
        curl -L "$CSV_URL" > "$CACHED_COPY"
        CURL_ES=$?
        if [[ $CURL_ES -ne 0 ]]; then
            return $CURL_ES
        fi
    fi

    # Don't try to parse out location fields,
    # since they aren't consistently filled in,
    # and since the CSV file doesn't use proper double-quoting
    # Or escaping, which makes it hard to parse.

    # Column names
    local HEADER
    HEADER="$(head -2 "$CACHED_COPY" | tail -1 | grep ",")"

    local -a MATCHING_LINES
    mapfile -t MATCHING_LINES \
            < <(tail -n "+2" "$CACHED_COPY" | grep "^${AREA_CODE}")

    if [[ ${#MATCHING_LINES} -gt 0 ]]; then
        echo "$HEADER"
        echo
        echo "${MATCHING_LINES[@]}" | grep "," # highlight field separators
        return 0
    else
        return 2
    fi
}

# funcalias "nanpa" "nanp"

#### SSH settings

function ssh-keygen { command ssh-keygen -t ed25519 "$@" ;}

function ssh-add-all {
    for KEY in $(find ~/.ssh -maxdepth 1 -type f -name '*_ed25519'); do
        ssh-add "$KEY"
    done
}

function my_ssh {
    NODE=$1

    if (! ssh "$NODE" test -e ~/.quinnified > /dev/null 2>&1); then
        quinnify_host "$NODE"
    fi

    ssh "$NODE"
}

#### News of various sorts

function c-span {
    open "https://www.c-span.org/congress/"
}

function newsboat {
    command newsboat \
        --refresh-on-start \
        --efwklwkml \    
        "$@"
}

# function rss { newsboat --refresh-on-start "$@" ;}
function usenet { tin "$@" ;}
function newspapers { cast-text "$@" ;}

function news {
    err "ambiguous command. Please run one of the following instead:

    - usenet
    - newspapers"
}

#### Emacs-merge

# cel FILE.el ("compile LISP") - byte-compile an emacs LISP file.
function cel { command emacs -nw -q -batch -f batch-byte-compile "$@" ;}

#### Miscellaneous-merge

# d ("date") - give today's date in YYYY-MM-DD format.
function d {
    check-args 0 0 '[no arguments]' "$@" || return 1
    date +%Y-%m-%d
}

function epoch2human {
    # check-args 1 1 "epoch2human EPOCH_SECONDS"
    local EPOCH_SECONDS="$1"
    date -d "@${EPOCH_SECONDS}"
}

function dos2unix { perl -pe 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

#### Mac-specific settings-merge

function macos-gui {
    on-macos && test -t 1 && pgrep -xq WindowServer
}

function df {
    if on-macos; then
        dfr "$@"
    else
        df "$@"
    fi
}

function dfr {
    local MANNAME="show mounted disks only, not other devices"
    check-args 0 1 '--no-gui' "$@" || return 1
    local NO_GUI="$1"

    if ! on-macos; then
        df -h | head -1
        df -h | command grep ^/
        return
    else
        err "CLI df is broken on macOS; it doesn't grok APFS pooling"
        if [[ "$NO_GUI" ]]; then
            err "and you passed --no-gui, so I'm bailing out"
            return 2
        elif ! macos-gui; then
            err "can't find the GUI. Are you in single-user mode?"
            return 3
        else
            function open_settings_storage {
                _stderr "dfr: opening Settings.app > Storage"
                open "x-apple.systempreferences:com.apple.settings.Storage"
            }

            if [[ "$DF_USE_MACOS_GUI" ]]; then
                open_settings_storage
                return 3
            else
                local USER_REPLY
                read -rp \
                     "Open the Settings > Storage GUI? [y/N] " \
                     USER_REPLY
                case "$USER_REPLY" in
                    [yY]) open_settings_storage; return 4;;
                       *) return 5;;
                esac
            fi
        fi
    fi
}

function mac-free-space {
    open "x-apple.systempreferences:com.apple.settings.Storage"
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

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder. Fails if the directory contains anything besides .DS_Store.

function rmds {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
}

# FIXME: doesn't work
# function stanza {
#     check-args 1 1 'PATTERN' "$@" || return 1
#     local PATTERN="$1"

#     para "$PATTERN" ~/.ssh/*.config \
#         | command grep -e '^' -e "$PATTERN"
# }

# shn ("SSH hostname") - print the canonical hostname for a ~/.ssh/config alias.
function shn {
    command grep -A 1 "$1" ~/.ssh/*config* | tail -1 | awk '{ print $3; }'
}

#### tmux

# ta $SESSION_NAME ("Tmux Attach") - attach to named session, creating if needed
function ta {
    if [[ $# -ne 1 ]]; then
        echo "ta: error: wrong number of arguments. Usage: ta SESSION_NAME"
    else
        local SESSION_NAME="$1"

        if ! tmux has-session -t "$SESSION_NAME" 2> /dev/null; then
	        tmux new-session -s "$SESSION_NAME"
        else
            tmux attach-session -t "$SESSION_NAME"
        fi
    fi
}

# tl ("Tmux List") - list running tmux sessions.
function tl {
    check-args 0 0 '[no arguments]' "$@" || return 1
    tmux list-sessions 2>/dev/null
}

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

#### Perl-merge: Perlbrew settings (this must come after the part in this file where PATH is set):

function activate-perlbrew { my-source ~/perl5/perlbrew/etc/bashrc ;}
function perlbrew-activate { activate-perlbrew ;}

function psql-tips { links ~/src/psql-tips/html/psql_tips_all.html ;}
function tips { psql-tips "$@" ;}
function mutt { neomutt "$@" ;}

#### Games

function sd2lcompile {
    gcc -o "$@"  `sdl2-config --cflags --libs`
}

function sdd { sdl2compile "$@" ;}

#### Web

function brow { defaultbrowser "$@" ;}
function brc { brow "chrome" ;}
function brs { brow "safari" ;}

# Adapted from a function posted by user Chubler_XL, in a comment on https://www.unix.com/shell-programming-and-scripting/277083-url-encoding-string-using-sed.html

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

function url-file {
    check-args 1 2 'URL [FILENAME]' "$@" || return 1
    local URL="$1"

    function write-that {
        local THE_URL="$1"
        printf "[InternetShortcut]\n%s\n" "$THE_URL"
    }

    if [[ "$2" ]]; then
        local FILENAME="$2"
        write-that "$URL" > "$FILENAME"
    else
        write-that "$URL"
    fi
}

function http-status-follow-redirects {
    check-args 1 1 'URL' "$@" || return 1
    local URL="$1"
    local STATUS="$(curl -L --head \
                         --silent --output /dev/null \
                         --write-out '%{http_code}' \
                         "$URL")"
    echo "$STATUS"
}

function http-200 {
    local STATUS="$(http-status-follow-redirects "$@")"
    [[ "$STATUS" = 200 ]]
}

function gmail {
    check-args 0 1 '[SEARCH_TERMS_STRING]' "$@" || return 1
    
    if [[ $# -eq 1 ]]; then
        local SEARCH_STRING="$1"
        gmail-search "$SEARCH_STRING"
    elif [[ $# -eq 0 ]]; then
        open -u "https://mail.google.com/mail/u/0"
    fi
}

function gmail-search {
    check-args 0 1 'QUERY [BASE_URL' "$@" || return 1
    local QUERY="$1"
    local BASE_URL="${2:-https://mail.google.com/mail/u/0}"
    
    local URL="$BASE_URL/#search"
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open -u "${BASE_URL}/${URLENCODED_QUERY}"
}

function goog {
    check-args 1 1 'QUERY_STRING # no need to URL-encode' "$@" || return 1
    local QUERY="$1"

    local BASE_URL="https://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q="
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open "${BASE_URL}${URLENCODED_QUERY}"
}

function wayback {
    check-args 1 1 "DESIRED_URL" "$@" || return 1
    local DESIRED_URL="$1"

    local THIS_YEAR="$(date '+%Y')"
    local WAYBACK_VERSION="${THIS_YEAR}0000000000\*"

    open "https://web.archive.org/web/$WAYBACK_VERSION/$DESIRED_URL"
}

function ddg {
    check-args 1 1 'QUERY_STRING # no need to URL-encode' "$@" || return 1
    local QUERY="$1"

    local BASE_URL="https://duckduckgo.com/?t=h_&q="
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open "${BASE_URL}${URLENCODED_QUERY}"
}

function bang {
    local MANNAME="use a DuckDuckGo bang"
    local USAGE="BANG_CODE_WITHOUT_EXCLAMATION_POINT QUERY
EXAMPLE
    ${FUNCNAME[0]} w 'Duck-Billed Platypus'"

    check-args 2 2 "$USAGE" "$@" || return 1
    local BANGCODE="$1"
    local QUERY="$2"
    
    ddg "!${BANGCODE} ${QUERY}"
}

function wi {
    local MANNAME="search Wikipedia (via DuckDuckGo)"
    check-args 1 1 'SEARCH_QUERY' "$@" || return 1
    local SEARCH_QUERY="$1"
    bang w "$SEARCH_QUERY"
}

function sheets {
    open "https://sheets.google.com/"
}

function sheet {
    sheets "$@"
}

#### Jenkins

# function jenkins {
#     local  REPO  BRANCH  JENKINS_OUTPUT_FILENAME  JENKINS_OUTPUT_URL  DOCKER_IMAGE_URL  SHORT_MESSAGE  LOGFILE
    
#     if [[ "$1" = "--no-sms" ]] || [[ "$JENKINS_NO_SMS" ]]; then
#         function sms {
#             : # no-op
#         }
#     else
#         function sms {
#             ~/...sms-me "$@"
#         }
#     fi

#     REPO="$(jenkins-repo)"
#     BRANCH="$(jenkins-branch)"
    
#     if [[ -z "$JENKINS_BASE_URL" ]]; then
#         echo 'jenkins: error: first export JENKINS_BASE_URL=[your URL]'
#     elif [[ -z "$REPO" ]]; then
#         echo 'jenkins: error: first call jenkins-repo $REPO_NAME'
#     elif [[ -z "$BRANCH" ]]; then
#         echo 'jenkins: error: first call jenkins-branch $BRANCH_NAME'
#     else
#         JENKINS_OUTPUT_URL="$JENKINS_BASE_URL/job/$REPO/job/$BRANCH/lastBuild/consoleText"
        
#         JENKINS_OUTPUT_FILENAME="$(mktemp "$HOME/tmp/jenkins-log--$REPO--$BRANCH.XXXXXX")"
#         /usr/bin/osascript ~/bin/jenkins-fetch-console-output.osascript "$JENKINS_OUTPUT_URL" \
#             | tee "$JENKINS_OUTPUT_FILENAME" \
#             | tee >(command grep -m 1 -o "docker://.*" | sed 's/docker:[/][/]//' | tee pbcopy) \
#             2>&1

#         sms "Jenkins finished the build"
#         # # This logfile-parsing code is brittle, so disable it for now
#         # if command grep --quiet SUCCESS "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Successfully built repo '$REPO', branch '$BRANCH'"
#         #     DOCKER_IMAGE_URL="$(command grep --color=never --only-matching 'harbor.*:git-.*$' "$LOGFILE" | command uniq)"
#         #     echo "$SHORT_MESSAGE"
#         #     echo "Docker image URL copied to clipboard:"            
#         #     echo "$DOCKER_IMAGE_URL" | tee >(pbcopy)
#         # elif command grep --quiet 'Log in with OpenShift' "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Jenkins login needed"
#         #     echo "First log in at $JENKINS_BASE_URL"; open "$JENKINS_BASE_URL"
#         # elif command grep --quiet 'FAILURE' "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Failed to build repo '$REPO', branch '$BRANCH'"
#         #     cat "$LOGFILE"
#         #     echo
#         #     echo "$SHORT_MESSAGE"
#         # else
#         #     SHORT_MESSAGE="Unrecognized Jenkins output for repo '$REPO', branch '$BRANCH'"
#         #     cat "$LOGFILE"
#         #     echo
#         #     echo "^ $SHORT_MESSAGE"
#         # fi
            
#         # if [[ -n "$JENKINS_SMS_ME" ]]; then
#         #     sms "$SHORT_MESSAGE"
#         # fi
#     fi
# }

# function jthis {
#     local REPO  BRANCH
#     REPO="$(gr)"
#     if [[ $? -eq 0 ]]; then
#         BRANCH="$(gb)"
#         if [[ $? -eq 0 ]]; then
#             jenkins-repo "$REPO"
#             jenkins-branch "$BRANCH"
#         fi
#     fi
#     j
# }

# function jenkins-repo {
#     local REPOFILE
#     REPOFILE="/tmp/jenkins-repo"

#     if [[ $# -eq 0 ]]; then
#         cat "$REPOFILE"
#     elif [[ $# -eq 1 ]]; then
#         echo "$1" > "$REPOFILE"
#     else
#         echo "jenkins-repo: error: too many arguments" >&2
#     fi
# }

# function jenkins-branch {
#     local BRANCHFILE
#     BRANCHFILE="/tmp/jenkins-branch"

#     if [[ $# -eq 0 ]]; then
#         cat "$BRANCHFILE"
#     elif [[ $# -eq 1 ]]; then
#         echo "$1" > "$BRANCHFILE"
#     else
#         echo "jenkins-branch: error: too many arguments" >&2
#     fi
# }

#### JSON

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

#### Confluence

function confluence-docs-by-me {
    open "https://$JIRA_TEAM.atlassian.net/wiki/search?cql=creator%3DcurrentUser()"
}

#### Jira

function _j-check-env {
    local VALUE

    local MY_EXIT_STATUS=0
    for VARNAME in JIRA_USERNAME  \
                   JIRA_API_TOKEN \
                   JIRA_BASE_URL  \
                   JIRA_DEFAULT_PROJECT
    do
        VALUE="{!VARNAME}"
        if [[ -z "$VALUE" ]]; then
            MY_EXIT_STATUS=1
            echo "Error: environment variable $VARNAME is not set" 2>&1
        fi
    done
    
    return "$MY_EXIT_STATUS"
}

function jlabels {
    _j-check-env || return 2
    check-args 0 0 '[no arguments]' "$@" || return 1;

    local -A UNIQUE_LABELS

    function extract-unique-labels-from {
        while IFS= read -r line; do
            IFS=',' read -ra LABELS_ON_THIS_LINE <<< "$line"
            for LABEL in "${LABELS_ON_THIS_LINE[@]}"; do
                UNIQUE_LABELS["$LABEL"]="1"
            done
        done
    }

    extract-unique-labels-from < \
        <(jira issue list --plain \
                          --no-truncate \
                          --columns "LABELS" \
                          --jql "labels is not empty and (assignee = '$JIRA_USERNAME')" \
              | awk '{print $2}' | tail -n +2)
    
    for LBL in "${!UNIQUE_LABELS[@]}"; do
        echo "$LBL"
    done \
        | sort
}

function jiv { # view issue
    _j-check-env || return 2
    check-args 1 varargs 'ISSUE [JIRA_CLI_ARGS...]' "$@" || return 1
    local ISSUE="$1"
    shift
    jira issue view "$@" "$ISSUE"
}

function jiw { # open an issue in the web browser
    _j-check-env
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"
    open "$JIRA_BASE_URL/browse/$ISSUE"
}

function jmv { # change issue state (e.g. Planned -> Completed)
    _j-check-env || return 2
    check-args 2 2 'ISSUE_CODE STATE (example: DATAPLAT-01 Done)' "$@" \
        || return 1
    local ISSUE="$1"
    local STATE="$2"

    local STDOUT EXIT_STATUS
    STDOUT="$(jira issue move "$ISSUE" "$STATE")"
    EXIT_STATUS="$?"
    if [[ "$EXIT_STATUS" -ne 0 ]]; then
        echo "$STDOUT" >&2
        return "$EXIT_STATUS"
    fi
}

function jprog { # mark an issue as "in progress"
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "In Progress"
}

function jdone { # mark an issue as completed :)
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "Completed"
}

function jcancel { # mark issue Canceled (closest possible thing to deleting it)
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "Canceled"
}

function jrm { jcancel "$@" ;}

function jas { # assign an existing issue
    _j-check-env || return 2
    check-args 1 2 'ISSUE [USERNAME] # defaults to you!' "$@" || return 1
    local ISSUE="$1"
    local ASSIGNEE="${2:-$JIRA_USERNAME}"
    
    local STDOUT EXIT_STATUS
    STDOUT="$(jira issue assign "$ISSUE" "$JIRA_USERNAME")"
    EXIT_STATUS="$?"
    if [[ "$EXIT_STATUS" -ne 0 ]]; then
        echo "$STDOUT" >&2
        return "$EXIT_STATUS"
    fi
}

function jec { # create an epic
    _j-check-env || return 2
    check-args 1 varargs '[--ADDITIONAL -J -I -RA --CLI-FLAGS] SHORT_TITLE [LABELS]' "$@" || return 1
    local -a JIRA_CLI_ARGS

    # # Flags, like -f, --flag, or --flag-with-arg arg. Currently none supported.
    # while [[ $# -gt 0 ]]; do
    #     case "$1" in
    #         -*) echo "Unsupported option '$1'" >&2
    #             return 1;;
    # 
    #          *) break;; # first non-flag arg: start of positional args
    #     esac
    # done

    # Required positional argument: the issue summary
    local ONE_LINER="$1"; shift
    JIRA_CLI_ARGS+=("--name" "$ONE_LINER" "--summary" "$ONE_LINER")
    
    # Optional positional arguments: labels for the issue
    for LABEL in "$@"; do
        JIRA_CLI_ARGS+=("--label" "$LABEL")
    done
    
    local JIRA_CLI_OUTPUT  JIRA_CLI_EXIT_STATUS
    JIRA_CLI_OUTPUT="$(
        jira epic create --no-input "${JIRA_CLI_ARGS[@]}"
    )"
    JIRA_CLI_EXIT_STATUS="$?"

    if [[ "$JIRA_CLI_EXIT_STATUS" -eq 0 ]]; then
        local URL_OF_THE_NEW_EPIC="$(echo "$JIRA_CLI_OUTPUT" | tail -1)"
        local EPIC_NAME="${URL_OF_THE_NEW_EPIC##*/}" # e.g. "DATAPLAT-10"
        jas "$EPIC_NAME" "$JIRA_USERNAME"
        echo "$EPIC_NAME"
    else
        echo "The jira CLI app reported an error:" >&2
        echo >&2
        echo "$JIRA_CLI_OUTPUT" >&2
        return $JIRA_CLI_EXIT_STATUS
    fi
}

function jepic { jec "$@" ;}

function jic { # create a "normal" issue, which in my workflow means a Task
    _j-check-env || return 2
    check-args 1 varargs 'SUMMARY_ONELINE [JIRA_CLI_ARGS...]' "$@" || return 1
    local ONE_LINER="$1"
    shift

    local JIRA_CLI_OUTPUT JIRA_CLI_EXIT_STATUS
    JIRA_CLI_OUTPUT="$(jira issue create --no-input \
                                         --type Task \
                                         "$@" \
                                         --summary "$ONE_LINER")"
    JIRA_CLI_EXIT_STATUS="$?"
    if [[ "$JIRA_CLI_EXIT_STATUS" -eq 0 ]]; then
        local URL_OF_THE_NEW_ISSUE="$(echo "$JIRA_CLI_OUTPUT" | tail -1)"
        local ISSUE_NAME="${URL_OF_THE_NEW_ISSUE##*/}"
        jas "$ISSUE_NAME" "$JIRA_USERNAME"
        echo "$ISSUE_NAME"
    fi

    return "$JIRA_CLI_EXIT_STATUS"
}

function jc { jic "$@" ;}

function jil { # list issues (including epics)
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    
    jira issue list "$@"
}; function jls { jil "$@" ;}

function jel { # list epics only
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    jira epic list "$@"
}

function jlt { # list tasks only
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    
    jls --type Epic "$@"
}

function jld { # list my Done issues
    _j-check-env || return 2
    check-args 0 0 "[no arguments]" "$@" || return 1
    
    jira issue list --assignee "$JIRA_USERNAME" --status "Done"
}

function jmine {
    _j-check-env || return 2
    check-args 0 0 '[no arguments]' "$@" || return 1
    local JQL="assignee = '$JIRA_USERNAME' or creator = '$JIRA_USERNAME' ORDER BY updated DESC"
    open -u "$(_jql2url "$JQL")"
}

function jmine-epics {
    _j-check-env || return 2
    check-args 0 0 "[no arguments]" "$@" || return 1

    jira issue list --assignee "$JIRA_USERNAME" --type Epic --status Backlog --plain
}

function _jql2url {
    _j-check-env || return 2
    check-args 1 1 'JQL_QUERY_STRING' "$@" || return 1
    local QUERY="$1"
    
    local JQL_BASE_URL="${JIRA_BASE_URL}/issues/?jql="
    local URLENCODED_QUERY
    URLENCODED_QUERY=$(urlencode "$QUERY")
    if [[ $? -ne 0 ]]; then
        return $?
    else
        local FULL_URL="${JQL_BASE_URL}${URLENCODED_QUERY}"
        echo "$FULL_URL"
    fi
}

function jql {
    _j-check-env || return 2
    check-args 1 1 'JQL_QUERY_STRING' "$@" || return 1
    local QUERY="$1"
    open -u "$(_jql2url "$QUERY")"
}

# function jeip { # create an issue in an existing epic.
#     _j-check-env || return 2
#     check-args 2 varargs \
#                'EPIC_NAME ISSUE_SUMMARY_ONELINE [...]' \
#                "$@" \
#         || return 1

#     local EPIC_NAME="$1"
#     shift
    
#     local ISSUE_NAME
#     ISSUE_NAME="$(_create_issue "$@")" \
#         && jas "$ISSUE" \
#         && jira epic link --issue "$ISSUE_NAME" --epic "$EPIC_NAME"
# }

#### Music

function guit { # download guitar tabs from ultimate-guitar.com
    check-args 1 1 'SOME_URL.ultimate-guitar.com' "$@" ||return 1
    local URL="$1"

    # 1. Fetch the page (curl).
    # 
    # 2. Extract the tablature payload from the JS-encoded HTML (perl).
    #
    # 3. Convert each DOS CRLF newline, represented in the JS-encoded payload
    #    as the literal four-char string "\r\n", to an actual newline (sed 1).
    # 
    # 4. Remove Ultimate Guitar's idiosyncratic [tab] and [/tab]
    #    HTML-like tags (sed 2).
    #
    # 5. Ditto for the [ch] and [/ch] pseudo-tags (sed 3).
    #
    # 6. Convert the HTML entity reference for an apostrophe
    #     - which is  &#039; - to a literal Unicode ' (sed 4).
    # 
    # Later we will probably need to replace many more HTML entity references,
    # and probably more [/pseudo-tags]. I will address them as I notice
    # them in tablature that I actually read.
    
    set -o pipefail
    curl -s "$URL" \
        | perl -0777 -ne 'print "$1" if $_ =~ m|{&quot;wiki_tab&quot;:{&quot;content&quot;:&quot;(.*?)(?=&quot;,&quot;revision_id)|smx;' \
        | sed -e "s|\\\r\\\n|\n|g" \
        | sed -e 's|\[tab\]||g' -e 's|\[/tab\]||g' \
        | sed -e 's|\[ch\]||g'  -e 's|\[/ch\]||g' \
        | sed -e "s|&#039;|'|g"
    local EXIT_STATUS=$?
    set +o pipefail
    echo
    return $EXIT_STATUS
}

if on-macos; then

    function song-id {
        local MANNAME="get the ID of the Apple Music song currently playing"
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e \
            'tell application "Music" to get persistent ID of current track'
    }

    function song-name {
        local MANNAME="get the name of the Apple Music song currently playing"
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e 'tell application "Music" to get name of current track'
    }

    function album-id {
        check-args 0 0 '' "$@" || return 1
        /usr/bin/osascript -e '
tell application "Music"
    set currentTrack to current track
    set trackAlbum to album of currentTrack
    set trackArtist to album artist of currentTrack
    if trackArtist is "" then set trackArtist to artist of currentTrack
    set theAlbum to first album of library playlist 1 whose name is trackAlbum and artist is trackArtist
    get database ID of theAlbum
end tell
'
    }

    function play {
        check-args 1 1 'BEGINNING_OF_SONG_NAME' "$@" || return 1
        local PREF="$1"
        
        /usr/bin/osascript -e "
tell application \"Music\"
    set t to first track of library playlist 1 whose name starts with \"$PREF\"
    play t
end tell"
    }

    function play-album-untested {
        check-args 1 1 'APPLE_MUSIC_ALBUM_ID' "$@" || return 1
        local ALBUM_ID="$1"

        local APPLESCRIPT="
set albumID to $ALBUM_ID
tell application \"Music\"
    set theAlbum to first album whose database ID is albumID
    play theAlbum
end tell
"
        echo "$APPLESCRIPT" | /usr/bin/osascript
    }
fi # end macOS-specific functions
    
#### Games-merge

## Steam settings
if on-macos; then
    function steam {
        local STEAM_GAMES_DIR="$HOME/Library/Application Support/Steam/steamapps/common"
        check-args 0 1 "[GAME_NAME] # or its first few chars"  "$@" || return 1
        if [[ $# -eq 0 ]]; then
            command ls -1 "$STEAM_GAMES_DIR" \
                | command grep -v "Steam Controller Configs"
        elif [[ $# -eq 1 ]]; then
            local PREF="$1"
            open "$STEAM_GAMES_DIR/${PREF}"*/*.app
        fi
    }
fi

### Video game music

function zophar {
    local MANNAME="search zophar.net for video game music"
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
                err "console '$1' is unsupported"
                return 1;;
        esac
        
        open "$URL"
    fi
}

#### JSON and YAML

function yv {
    if [[ $# -ne 1 ]]; then
        echo "yv: error: wrong number of arguments. Usage: yv YAML_KEY_NAME"
    else
        KEY="$1"
        shift
        
        #FIXME: figure out how to glob for *.yaml case-insensitively. Maybe use fd?
        # command grep --no-filename "${KEY}:[ ]*" *.yaml | sort | uniq | sed "s/^[ ]*${KEY}:[ ]*//"
        fdd "$@" .yaml -X grep "${KEY}:"
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

function e-paper {
    open -u "https://www.waveshare.com/catalogsearch/result/?q=e-paper"
}

#### GUI dev settings

QT_PATH="/opt/homebrew/opt/qt/bin"

function qtmake {
    cmake -DCMAKE_PREFIX_PATH=$(brew --prefix qt)
}

#### Path settings #sec yy#

## PATH (to search for executables)

export PATH="$SUBL_PATH:$RUST_PATH:/usr/local/opt/ccache/libexec:$LUA51_PATH:/bin:$LUAROCKS_PATH:$NODE_PATH:$RUBY_PATH:$PYTHON_PIPX_PATH:$PGPATH:/usr/local/bin:/usr/local/sbin:$MY_HOMEBREW_PATH:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/color:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:$HOME/.local/bin:/usr/local/opt/ncurses/bin:$HOME/bin/site:$HOME/bin/site/upsa:$SCSH_PATH:$HOME/ai/bin:/opt/sidplayfp/bin:$GITLAWB_PATH:$QT_PATH"

## C libraries

# "CPP" in this case means "C preprocesor," not C++.
export CPPFLAGS="$LUA51_CPPFLAGS $RUBY_CPPFLAGS"
export LDFLAGS="$LUA51_LDFLAGS $SCSH_LDFLAGS $RUBY_LDFLAGS"
export PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}"

if on-macos; then
    export SDKROOT="$(xcrun --show-sdk-path)"
fi

## man settings

export MANPATH=":$HOME/.local/share/man:$HOMEBREW_MANPATH:/usr/share/openssl/man:$HOME/share/man:/usr/share/man:/usr/local/share/man:/usr/local/man"

set +o pipefail

#### site-specific stuff, not stored in git #sec zz#

source-if-exists "$HOME/.site.bash.d/postamble.bash"

