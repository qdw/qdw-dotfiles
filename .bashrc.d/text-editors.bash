######## Dependencies:
######## 00-bash-metaprogramming.bash
######## 0x-homebrew.bash
########     00-bash-metaprogramming.bash
########     05-pager.bash
########     this file, argh, circular dependency
######## 03-fd.bash
########         00-bash-metaprogramming.bash
########         pager.bash

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
                     "emacs-plus@${VERSION_NUMBER}" \
                     --with-mailutils \
                     --without-cocoa
    )
}

# # FIXME: doesn't work. Once it does, fill in DESIRED_PATCHES.
# Alas, emacs-plus's compile options are no longer adequate to my needs.
# Build Emacs from the original source, but apply emacs-plus's favored
# patches first.

# function install-emacs {
#     check-args 1 1 'EMACS_VERSION_NUMBER # e.g., 30' "$@" || return 1
#     local VERSION_NUMBER="$1"
    
#     local INSTALL_DIR="/opt/emacs/$VERSION_NUMBER"
#     local SOURCE_DIR="$HOME/src-3/emacs"
#     local PATCH_DIR="$HOMEBREW_ROOT/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-$VERSION"
    
#     local DESIRED_PATCHES=""
    
#     function dir-is-okay {
#         if [[ -d "$DIR" ]] && \
#            [[ -r "$DIR" ]] && \
#            [[ -w "$DIR" ]] && \
#            [[ -x "$DIR" ]] && \
#            [[ -z "$(command ls -A "$DIR")" ]]
#         then
#             return 0
#         else
#             err "cannot access directory '$DIR'"
#             return 2
#         fi
#     }
    
#     brew install "$(brew deps "emacs-plus@${VERSION_NUMBER}")" || return    
    
#     for DIRECTORY in "$INSTALL_DIR" "$SOURCE_DIR" "$PATCH_DIR"; do
#         dir-is-okay "$DIRECTORY" || return
#     done
        
#     cd "$SOURCE_DIR" || return
#     command git checkout "emacs-${VERSION_NUMBER}" || return
    
#     ./autogen.sh || return
#     ./configure \
#         --prefix "$INSTALL_DIR" \
#         ...
        
#     for PATCH in $DESIRED_PATCHES; do
#         apply_patch "$PATCH" || return
#     done
    
# }

function emacs-package-clean {
    cd ~/.emacs.d \
        && fd --no-ignore '.el[cn]$' -x rm
}

## Emacs daemon settings

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
        kill "$ARG" "$PID"
    else
        err "It doesn't look like the emacs daemon is running"
        return 2
    fi
}

# If we're in a terminal session, start an emacsclient session inside
# that terminal (first starting the Emacs server, if it's not already
# running).
#
# This looks just like a normal terminal emacs session, but starts faster
# (and can see buffers from other emacsclient sessions, but whatever).

function emacs {
    # If, instead of running a command like crontab(1) that implicitly runs $VISUAL,
    # I directly type "emacs my-file", then...
    # shellcheck disable=SC2078
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
        local ORIG_DIR="$(pwd)"
        command emacs --bg-daemon >/dev/null 2>&1
        cd "$ORIG_DIR" >/dev/null
        
	# Give it time to start up and load my ~/.emacs.d/init.el
        sleep 4
        emacs "$@"
    fi
}

# re (Restart Emacs-daemon): stop it, then start it.
function re {
    ke "$@"
    se "$@"
}

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

# Use Emacs as my default text editor,
# but use Sublime Text with certain apps.

export EDITOR="$MY_EDITOR"
export VISUAL="$MY_EDITOR"

if command-exists "subl"; then
    export GIT_EDITOR="subl"
    export PSQL_EDITOR="subl -w"
    export HOMEBREW_EDITOR="subl"
else
    export PSQL_EDITOR="$MY_EDITOR"
fi

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
