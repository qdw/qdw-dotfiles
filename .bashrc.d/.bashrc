# shellcheck shell=bash
# export QDW_DEBUG_BASH="yeah"; # export QDW_TIMESTAMP="${EPOCHREALTIME/./}"

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

## Numbered sections must be sourced in order.

source-section "00-bash-metaprogramming.bash"

QSITED="$HOME/.site.bash.d"
source-if-exists "$QSITED/preamble.bash" ### machine-specific; git-ignored
source-if-exists "$HOME/.secrets.bash" ### also exclulded from git

source-section "01-bash-debugging.bash"
source-section "013-homebrew.bash"
source-section "015-terminal.bash"
source-section "02-grep.bash" ### grep; rg if present. Lots of customizations.
source-section "03-pagers.bash" ### less; bat; pandoc->elinks for Markdown
source-section "04-man.bash"
source-section "04-fd.bash" ### search for files (modern find(1) replacement).
source-section "04-string-manipulation.bash"
source-section "06-editors.bash" ### text editors, especially the Emacs daemon.
source-section "07-git.bash" ### generic git, plus GitHub.

## Non-numbered sections SHOULD be sourced after numbered sections,
## and MUST be sourced after 00-bash-metaprogramming.bash

#### Time and date

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

#### Files and stuff (maybe move to 00-bash-metaprogramming.bash?)

function which-realpath {
    local MANNAME="show the real (non-symlink) path to an executable program"
    check-args 1 1 'PROGRAM_NAME' "$@" || return 1
    local PROG="$1"
    local PTH=$(which "$PROG")
    realpath "$PTH"
}

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

# #FIXME: clean up comments; move to SYNOPSIS -> If the argument is a symlink, resolve it and then cd to the enclosing dir
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

function mcd {
    local DIRNAME
    DIRNAME="$1"
    mkdir "$DIRNAME" \
        && cd "$DIRNAME"
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

######## Bash customizations

#### Bash completion

source-section "completion.bash"

#### Bash shell prompt

source-section "prompt.bash"

#### Bash history settings

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

#### Other programming languages

function lisps {
    (
        brew ls \
            | command grep -i \
                      --color=never \
                      -E 'lisp|scheme|guile|plt|racket|sbcl'
        
        for DIR in "$HOME/src-3/sectorlisp" "$HOME/src-3/scheme.rs"; do
            local LISP_NAME="$(basename "$(ls -d "$DIR" 2>/dev/null)")"
            if [[ "$LISP_NAME" ]]; then
                echo "$LISP_NAME (in $DIR/)"
            fi
        done
    ) \
    | sort
}


#### Makefiles

function make-targets {
    gsed -e 's/^[^:]*$// ; /^$/d ; s/\(.*\):.*/\1/' Makefile | sort
}

function make-rules {
    command grep -E --color=never '[[:alpha:]]+:' Makefile
}

#### AI

source-section "ai.bash"

#### fortune(1), an old BSD app that prints a random message from a file

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

# source-section "aws.bash" ### slow, so just source manually if needed

#### Improvements on core Unix commands #FIXME: maybe move this to macos.bash/linux.bash... actually, I should have a unix.bash, shouldn't I?

function dos2unix { perl -pie 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}
function unix2dos { perl -pie ' s{$}{\r}x; ' "$@" ;}

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

######## ometers

#### Aranet4 air quality monitoring settings

function co2  {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read -r "$MY_ARANET4" 2> /dev/null | jq ".co2"
}

function co2-verbose {
    echo "CO2 was $(co2) ppm at $(date '+%I:%M %P') on $(d)"
}

#### PurpleAir air quality monitor settings

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

# source-section "c.bash"

#### Apps that I don't really like, but use for work

# source-section "jenkins.bash"
# source-section "jira.bash"
# source-section "slack.bash"

#### Set PATH, MANPATH, et cetera

# BASH_PID=$$
# echo; echo "Bash PID is $BASH_PID"; echo
# read -p "Press enter once you've started ptrace> "
# setx
source-section "zz-paths.bash"
# set -x

#### Source any final machine-specific code.

source-if-exists "$QSITED/postamble.bash"
