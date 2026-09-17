######## dependencies:
########     00-bash-metaprogramming.bash
########     04-pager.bash

######## Homebrew settings

export HOMEBREW_BAT=1
export HOMEBREW_BAT_CONFIG_PATH="$HOME/.config/bat/config"
export HOMEBREW_CASK_OPTS="--require-sha"
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_ASK=1
export HOMEBREW_NO_ENV_HINTS=1
export HOMEBREW_NO_INSECURE_REDIRECT="1"
export HOMEBREW_NO_INSTALL_UPGRADE="1"
export HOMEBREW_NO_UPDATE_REPORT_NEW="1"
export HOMEBREW_NO_UPGRADE_QUIT_CASKS="1"
export HOMEBREW_VERIFY_ATTESTATIONS="1"

## Homebrewed alternatives to bundled system utilities (mostly GNU)

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
        export HOMEBREW_MANPATH="$HDIR/manpages:$HDIR/share/man"
        export HOMEBREW_INFOPATH="$HDIR/share/info"
        export HOMEBREW_ROOT="$HDIR"
        export HOMEBREW_OPT="$HDIR/opt"
        export HOMEBREW_BIN="$HDIR/bin"
        export MY_HOMEBREW_PATH="$HOMEBREW_OPT:$HOMEBREW_BIN:$HDIR/sbin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin"
        export PATH="$(decolonize "$MY_HOMEBREW_PATH:$PATH")"
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
        _debug "It is now $NOW seconds since the Epoch"
        local LAST_BREW_UPDATE="$(mtime-in-epoch-seconds "$BREW_UPDATE_TIMESTAMP_FILE")"
        _debug "We last brewed $LAST_BREW_UPDATE seconds past the Epoch"
        local SECONDS_OLD=$(( NOW - LAST_BREW_UPDATE ))
        _debug "    Elapsed seconds: $SECONDS_OLD"
        if [[ $SECONDS_OLD -gt $BREW_UPDATE_STALENESS_THRESHOLD ]]; then
            _debug "$SECONDS_OLD > $BREW_UPDATE_STALENESS_THRESHOLD, so our brew is stale"
            return 0
        else
            _debug "$SECONDS_OLD <= $BREW_UPDATE_STALENESS_THRESHOLD, so our brew is still fresh"
            return 1
        fi
    fi
}

function brew-force-update {
    check-args 0 0 '' "$@" || return 1
    brew update \
         --quiet \
         2> >(grep -v "No remote 'origin' in /opt/homebrew/Library/Taps/quinn/homebrew-old-nethack" >&2) \
     && brew-update-timestamp
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

    # shellcheck disable=SC2068
    for FORMULA_NAME in ${OUTDATED_FORMULAE[@]}; do
        if [[ -v "${BREW_SERVICE_NAMES[$FORMULA_NAME]}" ]]; then
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
        # shellcheck disable=SC2068
        for SVC in ${OUTDATED_SERVICES[@]}; do
            color-printf "green" "==> "; color-printf "red" "Restarting "; color-printf "bright-green" "$SVC\n"
            brew services restart "$SVC"
        done
    fi
}

function boi {
    local MANNAME="show outdated Homebrew packages, and prompt to upgrade them"
    brew-update-if-stale

    local -a OUTDATED_FORMULAE OUTDATED_CASKS
    mapfile -t OUTDATED_FORMULAE < <(brew outdated --formula)
    local N_FORM=${#OUTDATED_FORMULAE[@]}
    if [[ $N_FORM -gt 0 ]]; then
        color-printf "red" "==> "; color-printf "red" "Outdated formulae:\n"
        echo "${OUTDATED_FORMULAE[@]}"
    fi


    mapfile -t OUTDATED_CASKS < <(brew outdated --cask | tee /dev/tty)
    local N_CASK=${#OUTDATED_CASKS[@]}
    if [[ $N_CASK -gt 0 ]]; then
        color-printf "red" "==> "; color-printf "red" "Outdated casks:\n"
        echo "${OUTDATED_CASKS[@]}"
    fi


    if [[ $N_FORM -gt 0 ]] || [[ $N_CASK -gt 0 ]]; then
        color-printf "bright-red" "==> Upgrade? [y/N]> "
        local USER_REPLY
        read -r USER_REPLY
        if [[ "${USER_REPLY,,}" =~ y ]]; then # downcase, then match
            bu
        fi
    fi
}

function tapname2taproot {
    check-args 1 1 'TAP' "$@" || return 1
    local RETVAL="$1"
    RETVAL="$HOMEBREW_ROOT/Library/Taps/$RETVAL"
    RETVAL="${RETVAL/%\/tap/\/homebrew-tap}"

    # Special-case code because repos don't follow a consistent convention
    # FIXME: generalize this by splitting $TAP on slash and checking for both "$POSTSLASH" and "homebrew-$POSTSLASH" subdirs
    RETVAL="${RETVAL/%\/emacs-plus/\/homebrew-emacs-plus}"
    RETVAL="${RETVAL/%\/keith\/formulae/\/keith\/homebrew-formulae}"
    echo "$RETVAL"
}

function tapname2formuladir {
    check-args 1 1 'TAP' "$@" || return 1
    local TAPROOT="$(tapname2taproot "$1")"

    local RETVAL
    if [[ -d "$TAPROOT/Formula" ]]; then
        RETVAL="$TAPROOT/Formula"
    else
        RETVAL="$TAPROOT"
    fi

    echo "$RETVAL"
}

function tapname2gitdir {
    check-args 1 1 'TAP' "$@" || return 1
    local TAPROOT="$(tapname2taproot "$1")"
    echo "$TAPROOT/.git"
}

function brew-tap-ls-formulae {
    check-args 1 1 'TAP # for instance, username/tapname' "$@" || return 1
    local DIR="$(tapname2formuladir "$1")"
    command ls -1 "$DIR" | sed -e 's/[.]rb$//'
}

function brew-tap-home {
    check-args 1 1 'TAP # for instance, username/tapname' "$@" || return 1
    local GIT_DIR="$(tapname2gitdir "$1")"
    if cd "$GIT_DIR" >/dev/null; then
        local URL="$(git remote get-url origin)"
        open -u "$URL"
        cd - >/dev/null
    fi
}

function brew-untrusted {
    check-args 0 1 '[--verbose]' "$@" || return 1

    function verbose { false ;}
    if [[ $# -eq 1 ]]; then
         if [[ "$1" = "--verbose" ]]; then
             function verbose { true ;}
         else
             err "unsupported flag '$1'"
             return 1
         fi
    fi
    
    while read -r FORMULA; do
        if [[ "$FORMULA" =~ ^homebrew/ ]]; then
            # homebrew/ (built-in) formulae are always trusted
            if verbose; then
                echo "trusted: $FORMULA"
            fi
            continue
        elif ! brew trust |command grep -E "^[ ][ ]${FORMULA}$" >/dev/null;then
            if verbose; then
                echo "untrusted: $FORMULA"
            else
                echo "$FORMULA"
            fi
        else
            if verbose; then
                echo "trusted: $FORMULA"
            fi
        fi
    done < <(brew tap)
}
