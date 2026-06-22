########     dependencies:
########         none
########     third-party dependencies:
########         GNU less

set -a

LESS="--RAW-CONTROL-CHARS --quit-if-one-screen --no-init --mouse"

if [[ "$INSIDE_EMACS" ]]; then
    # Emacs shell-mode is a dumb terminal, so use special pager settings there.
    export PAGER="cat"
    export AWS_PAGER="$PAGER"
    export GIT_PAGER="$PAGER"
    export PSQL_PAGER="$PAGER"
    function fd {
        fdd "$@"
    }
else
    # If we're a non-dumb terminal, then page fancily,
    # with the color and pagination options I like.
    if command -v jless > /dev/null; then
        # export AWS_PAGER="jless"
        export AWS_PAGER="bat --language json --style=rule,snip"
    else
        export AWS_PAGER="$PAGER"
    fi

    export PAGER="less"
    
    # Show an extra line before each match - useful when running 'git diff'
    export GIT_PAGER="$PAGER -j 2"
    
    export PSQL_PAGER="less --no-init --mouse"
fi

ACK_PAGER="$PAGER"
BAT_PAGER="$PAGER"
MANPAGER="$PAGER"

set +a

######## Pager settings: experimental

export PPA='fmt -s -uw "$COLUMNS" | less'

### bat, a colorizing cat(1) that understands many file formats

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
