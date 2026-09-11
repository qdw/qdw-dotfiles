# shellcheck shell=bash

set -o pipefail
umask 0022

QSITED="$HOME/.site.bash.d" # not checked into git; specific to this machine

function source-section {
    local QBASHD="$HOME/.bashrc.d" # checked into git

    local SECTION="$1" # filename including the ".bash"
    # echo "sourcing section '$SECTION'"
    source "$QBASHD/$SECTION"
}

#### #section-00#  My Bash metaprogramming library
  ## (helper functions, argument checking, functional programming...)

source-section "00-bash-metaprogramming.bash" ### Bash "

#### #section-01#  Helper functions for debugging Bash code

source-section "01-bash-debugging.bash"

#### #section-2#  Local-only files (don't commit to git)
  ## The following files shouldn't be checked into git 
  ## (either because they contain secrets, or because they contain
  ## third-party code with its own license)

source-if-exists "$QSITED/preamble.bash"
source-if-exists "$HOME/.secrets.bash"

#### #section-3#  Time and date

# Timestamp with time zone, no special chars, OK to interpolate into filenames

function ts {
    check-args 0 0 '[no-arguments]' "$@" || return 1
    date '+%Y-%m-%dT%H%M%S%z' "$@"
}

function ts-pg {
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

#### String manipulation

source-section "04-string-manipulation.bash" ### String manipulation

function lw {
    local MANNAME="show the real (non-symlink) path to an executable program"
    check-args 1 1 'PROGRAM_NAME' "$@" || return 1
    local PROG="$1"
    local PTH=$(which "$PROG")
    realpath "$PTH"
}

#### #section-4#  OS-specific settings

MY_OS="$(uname -s)"

function on-macos {
    [[ "$MY_OS" = "Darwin" ]]
}

function on-linux {
    [[ "$MY_OS" =~ Linux ]]
}

if on-macos; then
    source-section "macos.bash"
else
    source-section "linux.bash"
fi

#### Networking

source-section "network.bash"

#### Editors, particularly Emacs

source-section "editors.bash"

#### search: grep

source-section "02-grep.bash" ### grep (or rg, if installed). Lots of customizations.

#### search: fd

source-section "03-fd.bash" ### fd, a fast, modern replacement for find(1)

#### search: fzf

# source-section "04-fzf.bash"

#### .ignore files

source-section "dot-ignore.bash" ### helper functions for the .ignore files honored by git, fd, and rg

#### Terminal emulators (including functions for working with ANSI SGR colors)

source-section "terminal.bash"

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

function cdd { cd "$HOME/Desktop/$1" ;}
function cdq { cd "$HOME/src/1/qdw-dotfiles/$1" ;}
function cd3 { cd "$HOME/src-3/$1" ;}
function cd1 { cd "$HOME/src/1/$1" ;}
function cds { cd "$HOME/symlinks" ;}

# If the argument is a symlink, resolve it and then cd to the enclosing dir
# of the file that it point to.
#
# If the user passes no arguments, then do that to the directory portion
# of the previous argument passed ($_).
function cdl {
    # shellcheck disable=2016
    local MANNAME='cd to the enclosing dir of a file (or of $_ if none passed)'
    local ARG  REALPATH

    # shellcheck disable=2016
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
}

#### Bash

### Bash completion

source-section "completion.bash"

### Bash shell prompt

# BASH_PROMPT_USE_PREFIX_EMOJI="yes"
source-section "prompt.bash"
source-section "git-prompt.bash"

### Bash history settings

# Add a timestamp to history(1) output, using strftime(3) formatting.
export HISTTIMEFORMAT='  %Y-%m-%d %H:%M:%S %z    '

# Keep all history forever
export HISTFILESIZE=-1
export HISTSIZE=-1

#### Scheme Shell

if [[ -d "/opt/scsh" ]]; then
    SCSH_BASE="/opt/scsh"
    SCSH_PATH="${SCSH_BASE}/bin"
    LATEST_SCSH_VERSION="$(find "${SCSH_BASE}/lib" | sort --general-numeric-sort | tail -1)"
    SCSH_LDFLAGS="-L${SCSH_BASE}/lib/${LATEST_SCSH_VERSION}"
fi

#### Other shells

function shells {
    MANNAME="list installed Unix shells"
    check-args 0 0 '[no arguments]' "$@" || return 1
    (
        basename "$(which scsh 2>/dev/null)"
        brew ls \
            | command grep -E --color=never 'sh$|nushell' \
            | command grep -vE 'ssh|trash|hash|cash'
    ) \
    | sort
}

#### Pagers: GNU less and bat

source-section "05-pager.bash" ### Pager (GNU less plus bash)

### man

# # Tell man to display pages in color
# # Section headings? (bold orange)
# export LESS_TERMCAP_mb=$'\e[1;2;130m'
# # Program name? (bold purple)
# export LESS_TERMCAP_md=$'\e[1;4;20m'
# # Option names (dull blue)
# export LESS_TERMCAP_us=$'\e[0;9;25m'

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

#### AI

source-section "ai.bash"

#### fortune(1), an ancient BSD app that prints a random message from a file

function fortune {
    check-args 0 1 '[MY_FORTUNE_FILE] # default is "$HOME/fortunes/default"' \
        "$@" || return 1
    FILE="${1:-$HOME/fortunes/default}"
    
    if [[ -f "$FILE" ]] && [[ "$FILE" -nt "${FILE}.dat" ]]; then
        command strfile -rs "$FILE" || return
    fi

    command fortune "$FILE"
}

#### AWS

if [[ "$USE_FEATURE_AWS" ]]; then
    source-section "aws.bash"
fi

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
        ABS_PATH="${ABS_PATH/#${HOME}\//~/}"
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

    # shellcheck disable=SC2089
    MY_GNU_DIFF_OPTIONS='--palette="ad=36:de=31:hd=35:ln=34:rs=0"'
    function gdiff { "$DIFF_PATH/diff" "$@" ;}
    function diff-gnu { gdiff "$@" ;}
else
    WE_HAVE_GNU_DIFF=1
fi

function diff-vanilla {
    local COMMON_DIFF_OPTIONS="-u --color=always"
    if $WE_HAVE_GNU_DIFF; then
        # shellcheck disable=SC2090
        command diff $MY_GNU_DIFF_OPTIONS $COMMON_DIFF_OPTIONS "$@" | $PAGER
    else
        # shellcheck disable=SC2090
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

export FTP_LOGIN=anonymous
export FTP_PASSIVE_MODE=yes
export FTP_PASSWORD=""

#### Git, GitHub, gitlawb

source-section "06-git.bash" ### Git, GitHub, gitlawb

#### Kubernetes (and OpenShift) settings

function oc {
    if [[ "$1" = "login" ]] && [[ $# -eq 1 ]]; then
        local AUTH_URL
        if AUTH_URL=$(command oc login | sed s'/.*\(https.*\)/\1/'); then
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

# source-section "perl.bash"


#### PostgreSQL
  ## (for additional PostgreSQL-realted settings, see the following files):
  ##    ~/.psqlrc*
  ##    ~/.pg_service
  ##    ~/.pgpass

source-section "postgresql.bash"

#### IRL settings

## PurpleAir air quality monitor settings

function air {
    local RESPONSE_FILE
    RESPONSE_FILE=$(mktemp "$HOME/tmp/air.XXXX")

    if curl -L 'https://www.purpleair.com/json?show=119745&key=NPLQPTWKC54229MU' \
             > "$RESPONSE_FILE" \
             2> /dev/null
    then
        echo "Problem contacting the PurpleAir API server; maybe rate-limiting?"
        echo "Try waiting ten minutes and then re-running."
    else
        local PM25 STATS TSLM NUMBER_OF_MINUTES INFLECTED

        PM25="$(jq --raw-output '.results[0]."PM2_5Value"' < "$RESPONSE_FILE")"
        STATS="$(jq --raw-output '.results[0].Stats' < "$RESPONSE_FILE")"
        TSLM="$(echo "$STATS" | jq --raw-output '.timeSinceModified')"

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

function yak {
    todo "$@"
}

#### Haskell

# # Nope, defaulting to $HOME/.local/bin instead
# HASKELL_PATH="$HOME/.cabal/bin"

source-section "python.bash"

#### Aranet4 air quality monitoring settings

function co2  {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read -r "$MY_ARANET4" 2> /dev/null | jq ".co2"
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
    rdap "$@" 2>&1 | $PAGER 
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
        echo "=="
        echo
        "$NODE" "$COMMAND"
        echo
    done
}

#### Processes

source-section "proc.bash"

#### Set operations

# source-section "sets.bash"

#### Slack

# source-section "slack.bash"

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

#### Functions for exercising various web APIs

source-section "www.bash"
# source-section "jenkins.bash"
# source-section "jira.bash"


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
    
    curl -s "$URL" \
        | perl -0777 -ne 'print "$1" if $_ =~ m|{&quot;wiki_tab&quot;:{&quot;content&quot;:&quot;(.*?)(?=&quot;,&quot;revision_id)|smx;' \
        | sed -e "s|\\\r\\\n|\n|g" \
        | sed -e 's|\[tab\]||g' -e 's|\[/tab\]||g' \
        | sed -e 's|\[ch\]||g'  -e 's|\[/ch\]||g' \
        | sed -e "s|&#039;|'|g"
    local EXIT_STATUS=$?
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
    
#### Video game music

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

source-section "json-yaml.bash"

#### C/C++ dev

source-section "c.bash"

#### PATH

CURL_PATH="$HOMEBREW_ROOT/opt/curl/bin"

export PATH="$CURL_PATH:$SUBL_PATH:$RUST_PATH:/usr/local/opt/ccache/libexec:$LUA51_PATH:/bin:$LUAROCKS_PATH:$NODE_PATH:$RUBY_PATH:$PYTHON_INTERPRETER_PATH:$PYTHON_PIPX_PATH:$PGPATH:/usr/local/bin:/usr/local/sbin:$HASKELL_PATH:$MY_HOMEBREW_PATH:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/color:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:$HOME/.local/bin:/usr/local/opt/ncurses/bin:$HOME/bin/site:$HOME/bin/site/upsa:$SCSH_PATH:$HOME/ai/bin:/opt/sidplayfp/bin:$GITLAWB_PATH:$QT_PATH:$ALACRITTY_PATH"

# Account for the fact that some of those interpolated env vars may be empty.

function print-without-empty-paths {
    check-args 1 1 'MY_PATH' "$@" || return 1
    local PTH="$1"

    ORIG_EXTGLOB=$(shopt -p extglob)
    shopt -s extglob
    PTH="${PTH//+(:)/:}"    # Collapse runs of colons to single colon.
    PTH="${PTH#:}"          # Strip leading colon, if any.
    PTH="${PTH%:}"          # Strip trailing colon, if any.
    $ORIG_EXTGLOB
    echo "$PTH"
}

export PATH="$(print-without-empty-paths "$PATH")"

## C libraries

# "CPP" in this case means "C preprocesor," not C++.
export CPPFLAGS="$LUA51_CPPFLAGS $RUBY_CPPFLAGS"
export LDFLAGS="$LUA51_LDFLAGS $SCSH_LDFLAGS $RUBY_LDFLAGS"
export PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}"

if on-macos; then
    export SDKROOT="$(xcrun --show-sdk-path)"
fi

## man settings

export MANPATH="$HOME/.local/share/man:$HOMEBREW_MANPATH:/usr/share/openssl/man:$HOME/share/man:/usr/share/man:/usr/local/share/man:/usr/local/man:$ALACRITTY_MANPATH"

export MANPATH="$(print-without-empty-paths "$MANPATH")"

#### #section-8#  Site-specific stuff, not stored in git

source-if-exists "$HOME/.site.bash.d/postamble.bash"
