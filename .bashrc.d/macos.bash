export BASH_SILENCE_DEPRECATION_WARNING=1

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

source-section "0x-homebrew.bash"
if [[ -f "$HOMEBREW_BIN/trash" ]]; then
    # function rm {
    #     "$HOMEBREW_BIN/trash" -F "$@"
    # }
    function trash {
        "$HOMEBREW_BIN/trash" -F "$@"
    }
fi



source-section "macos-apps.bash" ### Shortcuts for macOS apps, e.g. Mail, Mail.app, Finder, Finder.app

source-section "macos-light-and-dark.bash" ### Set light/dark mode in all apps, web pages, and other contexts
source-section "macos-app-completion.bash" ### Make 'la SUBSTRING tab' complete on macOS app names

## When on macOS, prefer Homebrewed GNU tools to their built-in
## equivalents, iff they're available

for GNU_COMMAND in cut date dircolors find head roff sed sort tac tail
do
    BREWED_GNU="${HOMEBREW_BIN}/g${GNU_COMMAND}"
    if [[ -e "$BREWED_GNU" ]]; then
        alias "${GNU_COMMAND}=${BREWED_GNU}"
    fi
done

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

## Safer deletion settings for macOS

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
        DARWIN_VERSION="${_[-1]}"
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

    _format_macos_version "$CODENAME" "$MACOS" "$MACOSVER"
}

function _format_macos_version {
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

    if on-linux || [[ "$(command -V stat | head -1)" = "stat is a function" ]]
    then
        gnu-mtime-seconds "$FILENAME"
    elif on-macos
    then
        bsd-mtime-seconds "$FILENAME"
    else
        carp "warning: unrecognized platform; guessing that stat(1) is GNU stat"
        gnu-mtime-seconds "$FILENAME"
    fi
}

function ldd {
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
}

#### Fonts

function mac-disable-font-smoothing {
    defaults write -g AppleFontSmoothing -int 0
    defaults write CGFontRenderingFontSmoothingDisabled -INT 1
}

function mac-globalize-fonts {
    local TERMINAL_APP_DIR
    if [[ -d "/System/Applications/Utilities/Terminal.app" ]]; then
        TERMINAL_APP_DIR="/System/Applications/Utilities/Terminal.app"
    else
        TERMINAL_APP_DIR="/Applications/Utilities/Terminal.app"
    fi

    # shellcheck disable=SC2033
    sudo cp -R \
        "$TERMINAL_APP_DIR/Contents/Resources/Fonts" \
        "/Library/Fonts/"
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

#### VMs

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

function run-anyway {
    check-args 1 1 'SomeApp.app' "$@" || return 1
    local FILE="$1"
    local FULL_PATH=""

    if [[ ! "$FILE" =~ [.]app$ ]]; then
        err "Malformed argument; it should end in .app"
        return 1
    elif [[ "$FILE" =~ / ]]; then
        FULL_PATH="$FILE"
    else
        FULL_PATH="$(which "$FILE")" || return 1
    fi
        
    xattr -dr com.apple.quarantine "$FULL_PATH"
}
