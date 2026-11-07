# shellcheck shell=bash
## export QDW_DEBUG_BASH="yeah"; # export QDW_TIMESTAMP="${EPOCHREALTIME/./}"

umask 0022
set -o pipefail

function source-if-exists {
    if [[ -f "$1" ]]; then
        source "$1"
    else
        return 254
    fi
}

function source-section {
    local FILE="$1"
    source "$HOME/.bashrc.d/$FILE"

    if [[ "$QDW_DEBUG_BASH" ]]; then
        local WIDTH ELAPSED UNIT FILE

        if [[ "$QDW_TIMESTAMP" ]]; then
            NOW="${EPOCHREALTIME/./}"

            WIDTH=8
            ELAPSED="$(( NOW - QDW_TIMESTAMP ))"
            UNIT=" μs "

            QDW_TIMESTAMP="$NOW"
        else
            WIDTH=0
            ELAPSED=""
            UNIT=""
        fi

        printf "%*s%ssourced section %s\n" "$WIDTH" "$ELAPSED" "$UNIT" "$FILE"
    fi
}

#### Numbered sections must be sourced in order.

source-section "00-bash-metaprogramming.bash"

#### Machine-specific files, not part of this git repo

QSITED="$HOME/.site.bash.d"
source-if-exists "$QSITED/preamble.bash"
source-if-exists "$HOME/.secrets.bash"

#### git-controlled code. These files must be sourced in order.

source-section "01-bash-debugging.bash"
source-section "012-pager.bash" ### less; bat; pandoc->elinks for Markdown
source-section "013-homebrew.bash"
source-section "015-terminal.bash"
source-section "02-grep.bash" ### grep; rg if present. Lots of customizations.
source-section "04-man.bash"
source-section "04-fd.bash" ### search for files (modern find(1) replacement).
source-section "04-string-manipulation.bash"
source-section "06-editors.bash" ### text editors, especially the Emacs daemon.
source-section "07-git.bash" ### generic git, plus GitHub.

#### Unnumbered files (in any order) must be sourced after numbered.

#### Time and date

source-section "time-and-date.bash"

#### OS-specific settings

source-section "unix.bash"

if on-macos; then
    source-section "macos.bash"
else
    source-section "linux.bash"
fi

#### Networking

source-section "network.bash"

#### Search: fzf

# source-section "fzf.bash"

#### Filename/path/dir/cd-related functions

source-section "filename-manip.bash"
source-section "file-contents-manip.bash"

######## Bash customizations

source-section "completion.bash"

#### Bash shell prompt

source-section "prompt.bash"

#### Bash history settings

source-section "history.bash"

source-section "alt-shells.bash"

source-section "lisps.bash"

#### AI

source-section "ai.bash"

source-section "fortune.bash"

#### AWS

# source-section "aws.bash" ### slow, so just source manually if needed

source-section "ls.bash"

#### diff settings

source-section "diff.bash"

#### FTP

source-section "ftp-client.bash"

#### Kubernetes (and OpenShift) settings

source-section "k8s.bash"


#### Perl settings

# source-section "perl.bash"


#### PostgreSQL
  ## (for additional PostgreSQL-realted settings, see the following files):
  ##    ~/.psqlrc*
  ##    ~/.pg_service
  ##    ~/.pgpass

source-section "postgresql.bash"

######## ometers

source-section "ometers.bash"

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
            while IFS= read -r URL || [[ -n "$URL" ]]; do
                firefox "$URL"
            done < "$URL_FILE"
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

function yak {
    todo "$@"
}

#### Python

source-section "python.bash"

#### Tor

function tor-curl { curl --socks4a 127.0.0.1:9050 "$@" ;}

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

#### Text-to-speech (TTS)

# # Broken.
# function tts {    
#     local MANNAME="take a file of plain text and read it aloud"
#     check-args 1 1 'FILE' "$@" || return 1
#     local FILE="$1"

#     local VOICE="en-US-AndrewMultilingualNeural"
#     local RATE="+25%"

#     if ! edge-tts --file "$FILE" --write-media "$MP3_FILE" --voice "$VOICE" --rate="$RATE"
#     then
#          edge-tts --text "Uh, sorry, I couldn't read the article, because the edge-tts command failed" --write-media "$MP3_FILE" --voice "$VOICE" --rate="$RATE"
#     fi

#     ffplay -nodisp -autoexit -loglevel quiet "$MP3_FILE"
# }

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
    # RUBYOPT=rubygems
    export RUBY_PATH=$HOMEBREW_ROOT/opt/ruby/bin
    export RUBY_LDFLAGS="-L${HOMEBREW_ROOT}/opt/ruby/lib"
    export RUBY_CPPFLAGS="-I${HOMEBREW_ROOT}/opt/ruby/include"
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

source-section "network.bash"

#### Processes

source-section "proc.bash"

#### Set operations

# source-section "set-operations.bash"

#### POTS telephony

source-section "phone.bash"

#### SSH

source-section "ssh.bash"

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

function usenet {
    tin "$@"
}

function newspapers {
    cast-text "$@"
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

# mr ("mount real") - show all mounted devices that are actually disks (see also dfr).
function mr {
    mount | head -1
    mount | command grep ^/
}

# elw Perl::Module, elw Perl/Module.pm ("edit library which") - find a perl module in PER5LIB and open it in VISUAL. #FIXME: test behavior.
# elw { $VISUAL $(lw "$@") ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder. Fails if the directory contains anything besides .DS_Store.

function rmds {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
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

#### Tor

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

# unrar_plural FILE1 ...: because unraring files one by one is tiresome.
function unrar_plural { for f in *rar; do unrar x -o+ "$f" .; done ;}

# xb ("xbindkeys") - reload custom (xbindkeys) X11 keybindings.
function xb { killall --user "$USER" xbindkeys && xbindkeys ;}

function mutt { neomutt "$@" ;}

#### Functions for exercising various web APIs

source-section "www.bash"

#### Music

#### JSON and YAML

source-section "json-yaml.bash"

#### C/C++ dev

# source-section "c.bash"

#### Apps that I don't really like, but use for work

# source-section "jenkins.bash"
# source-section "jira.bash"
# source-section "slack.bash"

#### Set PATH, MANPATH, et cetera (to debug, uncomment ##-commented code)

## BASH_PID=$$
## echo; echo "Bash PID is $BASH_PID"; echo
## read -p "Press enter once you've started ptrace> "
## setx
source-section "zz-paths.bash"
## set -x

#### Source any final machine-specific code (not part of this git repo)

source-if-exists "$QSITED/postamble.bash"
