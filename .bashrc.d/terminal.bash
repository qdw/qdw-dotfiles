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

## Extensions specific to iTerm2

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

## Env vars specific to Alacritty on macOS

if on-macos; then
    export ALACRITTY_ROOT
    ALACRITTY_ROOT="$(which Alacritty.app 2>/dev/null)" \
        && export ALACRITTY_PATH="$ALACRITTY_ROOT/Contents/MacOS" \
        && export ALACRITTY_MANPATH="$ALACRITTY_ROOT/Contents/Resources/man"
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


    # Likewise, the macOS 'make install' doesn't install alacritty's
    # terminfo entries for you. You have to do something like this:
    #
    # mkdir -p ~/.terminfo/61
    # cd ~/.terminfo/61
    # ln -s /Applications/Alacritty.app/Contents/Resources/61/alacritty
    # ln -s /Applications/Alacritty.app/Contents/Resources/61/alacritty-direct
fi
