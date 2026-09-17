######## Dependencies:
######## 00-bash-metaprogramming.bash
######## 0x-homebrew.bash
########     00-bash-metaprogramming.bash
########     05-pager.bash
########     this file, argh, circular dependency
######## 03-fd.bash
########         00-bash-metaprogramming.bash
########         pager.bash

#### Default text editor(s)

# Use Emacs as my default text editor,
# but use Sublime Text (if installed) for certain apps.

MY_EDITOR="$HOME/bin/emacs-wrapper"
if [[ ! -x "$MY_EDITOR" ]]; then
    if echo <<END_EMACS_WRAPPER >> "$MY_EDITOR"
\#\!${HOMEBREW_BIN}/bash

source ~/.bashrc
emacs "$@"

END_EMACS_WRAPPER
    then
        chmod 0755 "$MY_EDITOR"
    fi
fi

export EDITOR="$MY_EDITOR"
export VISUAL="$MY_EDITOR"

if command-exists "subl"; then
    export GIT_EDITOR="subl"
    export PSQL_EDITOR="subl -w"
    export HOMEBREW_EDITOR="subl"
else
    export PSQL_EDITOR="$MY_EDITOR"
fi

#### Emacs

## Emacs daemon

function emacs-daemon-pid {
    pgrep -U "$USER" -f -i "Emacs --bg-daemon"
}

function pe {
    emacs-daemon-pid -l
}

function the-emacs-daemon-is-running {
    emacs-daemon-pid > /dev/null
    return $?
}

# se (Start Emacs-daemon): start the Emacs daemon (if not running).
function se {
    if the-emacs-daemon-is-running; then
        err "Looks like the emacs daemon is already running"
        return 1
    else
        echo "Starting the Emacs daemon..."
        local ORIG_DIR="$(pwd)"
        command emacs --bg-daemon >/dev/null 2>&1
        cd "$ORIG_DIR" >/dev/null
        
        # Give it time to start up and load my ~/.emacs.d/init.el
        sleep 4
        emacs "$@"
    fi
}

# ke (Kill Emacs-daemon): kill the Emacs daemon (if running).
function ke {
    check-args 0 1 '[SIGNAL]' "$@" || return 1
    local SIGNAL="$1"

    case "$SIGNAL" in
        "") SIGNAL="-TERM";;
        "-9"|"-KILL") :;;
        *) err "unsupported signal '$SIGNAL'"; return 1;;
    esac

    local PID="$(emacs-daemon-pid)"
    if [[ "$PID" ]]; then
        kill "$SIGNAL" "$PID"
    else
        err "emacs daemon PID not found"
        return 2
    fi
}

# re (Restart Emacs-daemon): stop it, then start it.
function re {
    ke "$@"
    se "$@"
}

# If we're in a terminal session, start an emacsclient session inside
# that terminal (first starting the Emacs server, if it's not already
# running).
#
# This looks just like a normal terminal emacs session, but starts faster,
# and can see buffers from other emacsclient sessions.

function emacs {
    # shellcheck disable=SC2078
    if [[ "$1" = "-c" && shift ]] || [[ "$INSIDE_EMACS" ]]; then
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

function cel {
    MANNAME="[byte]-Compile Emacs Lisp"
    check-args 1 varargs 'LISP_FILE [LISP_FILE ...]' "$@" || return 1
    command emacs -nw -q -batch -f batch-byte-compile "$@"
}

function bisect-emacs-init {
    local MANNAME="load only the first FRACTION of init.el, for debugging
    (automatically expanded to the next top-level sexp)"

    check-args 1 1 'FRACTION' "$@" || return 1
    local FRAC="$1"
    BISECT_EMACS="$FRAC" command emacs --debug-init "$HOME/.emacs.d/init.el"
}

function install-emacs-plus {
    check-args 1 1 'EMACS_VERSION_NUMBER # e.g., 29' "$@" || return 1
    local VERSION_NUMBER="$1"
    
    (
        set -euo pipefail
        brew install --formula \
                     --build-from-source \
                     --with-mailutils \
                     --without-cocoa \
                     "d12frosted/emacs-plus/emacs-plus@${VERSION_NUMBER}"  # "emacs-plus@${VERSION_NUMBER}"
    )
}

function emacs-package-clean {
    cd ~/.emacs.d \
        && fd --no-ignore '.el[cn]$' -x rm
}

#### vim

# Sometimes I want an editor with really low input latency.
# In those cases, I use vim, but with a custom configuration
# that makes it honor common Emacs keybindings.

function vimacs {
    # Run vim with settings that make its behavior and keybindings
    # resemble those of Emacs
    command vim -u "$XDG_CONFIG_HOME/vim/vimacsrc" "$@"
}

# vl ("Vim Less") - use vim as a colorizing pager
function vl {
    "$HOMEBREW_ROOT/share/vim/vim91/macros/less.sh" "$@"
}
