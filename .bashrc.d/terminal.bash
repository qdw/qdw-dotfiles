######## Terminal emulators (including functions for working with ANSI SGR colors)

## Strip non-printable characters

function printable {
    local MANNAME="remove non-printable characters from STRING"
    check-args 1 1 'STRING' "$@" || return 1
    local STRING="$1"
    tr -cd  "[:print:]"  "$STRING"
}

# bnw (Black 'n' White): remove non-printable chars,
# such as ANSI color codes. Works as a filter, e.g. 'git diff a b | bnw'

function bnw {
    local MANNAME="filter: remove ANSI color codes and control characters

SYNOPSIS
    cat SOME_FILE SOME_OTHER_FILE | bnw | less
"
    local CLEAN_LINE
    while IFS="" read -r LINE; do
        # Strip ANSI color codes
        CLEAN_LINE="$(echo "$LINE" | sed -r 's/\x1B\[[0-9;]*[a-zA-Z]//g')"

        # Strip control characters
        CLEAN_LINE="$(echo "$CLEAN_LINE" | tr -d '[:cntrl:]')"
        
        echo "$CLEAN_LINE"
    done
}

## ANSI SGR color

export QANSI='\033['
export QRESET='\033[0m'

# Enable 24-bit color (a.k.a. truecolor).
# All the terminal emulators that I use support this.
export COLORTERM="truecolor"
export CLICOLOR=1

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
    echo "$@"
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

## Extensions common to most terminal emulators

# title STRING: set the window title of an xterm/iTerm2/rxvt window to STRING.
# This works by using a special ANSI-like escape code that not all terminal
# emulators support.
function title {
    echo -ne "\033]2;" "$1" "\007"
}

#### iTerm2

# See https://iterm2.com/documentation-captured-output.html
function iterm2-cl {
    printf "\e]1337;ClearCapturedOutput\e\\"
}

function iterm2-integration-activate {
    # Enable "iTerm2 integration," that is, a bunch of fancy features
    # specific to the iTerm2 terminal emulator (example: click-to-download
    # over ssh plus lots more).
    #
    # It would slow down shell startup considerably if I automatically
    # sourced this code, so I've separated it into a function that you
    # can call explicitly when you want it.
    my-source ~/.iterm2_shell_integration.bash
}

function hex {
    MANNAME="take a hex triplet, and print it in the indicated color
LIMITATIONS
    Requires a terminal emulator that supports (ANSI SGR) 24-bit color,
    a.k.a. truecolor."
    check-args 1 1 'RRGGBB' "$@" || return 1
    
    # Extract the RGB values from the hex color code
    local hex=${1#"#"}
    local r=$(printf '0x%0.2s' "$hex")
    local g=$(printf '0x%0.2s' "${hex#??}")
    local b=$(printf '0x%0.2s' "${hex#????}")
    
    # Convert the hex values to decimal
    local R=$(printf '%d' "$r")
    local G=$(printf '%d' "$g")
    local B=$(printf '%d' "$b")
    
    # Print the hex string in the color that it specifies
    printf "\e[38;2;%d;%d;%dm%s\e[0m\n" "$R" "$G" "$B" "$hex"
}

#### Alacritty
#### (see also the following section, "Install Alacritty from source on MacOS".

## Use the Alacritty daemon, even though it is somewhat buggy.

export ALACRITTY_SOCKET="$HOME/tmp/alacritty-socket"

if on-macos; then
    export ALACRITTY_ROOT
    ALACRITTY_ROOT="$(which Alacritty.app 2>/dev/null)" \
        && export ALACRITTY_PATH="$ALACRITTY_ROOT/Contents/MacOS" \
        && export ALACRITTY_MANPATH="$ALACRITTY_ROOT/Contents/Resources/man"
fi

function alacritty-daemon-pid {
    pgrep -U "$USER" -f -i "alacritty --daemon"
}

function the-alacritty-daemon-is-running {
    alacritty-daemon-pid > /dev/null
}

function the-alacritty-socket-is-present {
    test -f "$ALACRITTY_SOCKET"
}

function start-alacritty-daemon {
    if the-alacritty-daemon-is-running; then
        err "looks like the alacritty daemon is already running"
        if the-alacritty-socket-is-present; then
            return 1
        else
            err "but the alacritty socket, '$ALACRITTY_SOCKET', is missing"
            return 2
        fi
    else
        local ORIG_DIR="$(pwd)"
        cd
        (
            # nohup "$ALACRITTY_PATH/alacritty" --daemon >/dev/null 2>&1 &
            "$ALACRITTY_PATH/alacritty" --daemon &
            NOHUP_ES=$?
            disown
            exit $NOHUP_ES
        )
        local EXIT_STATUS=$?
        cd "$ORIG_DIR" > /dev/null
        return $EXIT_STATUS
    fi
}

function kill-alacritty-daemon {
    check-args 0 2 '[SIGNAL]' "$@" || return 1
    local SIGNAL="$1"

    case "$SIGNAL" in
        "") SIGNAL="-TERM";;
        "-9"|"-KILL") :;;
        *) err "unsupported signal '$SIGNAL'"; return 1;;
    esac

    local PID="$(alacritty-daemon-pid)"
    if [[ "$PID" ]]; then
        if [[ -f "$ALACRITTY_SOCKET" ]]; then
            kill "$SIGNAL" "$PID"
        else
            err "daemon is running with PID '$PID', but socket '$ALACRITTY_SOCKET' is missing"
            if [[ "$SIGNAL" = "-9" ]] || [[ "SIGNAL" = "-KILL" ]]; then
                echo "trying to kill anyway, since the user passed SIGNAL '$SIGNAL'" >&2               kill "$SIGNAL" "$PID"
            else
                echo "not killing, since the user didn't pass signal -9 or -KILL" >&2
                return 3
            fi
        fi
    else
        err "alacritty daemon PID not found"
        return 2
    fi
}

function restart-alacritty-daemon {
    kill-alacritty-daemon && start-alacritty-daemon
}

if ! the-alacritty-daemon-is-running; then
    start-alacritty-daemon
fi

#### Install Alacritty from source on MacOS

# Homebrew disabled the Alacritty Cask. This is because it doesn't play Apple's
# executable-signing game, so macOS Gatekeeper rejects it by default.
#
# So you're best off installing Alacritty from source, which is not hard.
# However, there are a few manual steps.
# 
# Alacritty's macOS 'make install' doesn't put the man pages
# into subdirectories, so you have to do it yourself.
# This is the dir structure that I use:
# 
# quinn@au:/Applications/Alacritty.app/Contents/Resources$ echo;fd -t f man
# 
# man/man1/alacritty-msg.1.gz
# man/man1/alacritty.1.gz
# man/man5/alacritty-bindings.5.gz
# man/man5/alacritty.5.gz
# man/man7/alacritty-escapes.7.gz
# 
# Likewise, the macOS 'make install' doesn't install alacritty's
# terminfo entries for you. You have to do something like this:
#
# mkdir -p ~/.terminfo/61
# cd ~/.terminfo/61
# ln -s /Applications/Alacritty.app/Contents/Resources/61/alacritty
# ln -s /Applications/Alacritty.app/Contents/Resources/61/alacritty-direct

