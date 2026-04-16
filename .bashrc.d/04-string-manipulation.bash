########     dependencies:
########         00-bash-metaprogramming.bash
########         02-grep.bash
########         pager.bash
########     third-party dependencies:
########         moreutils (for the sponge-command):
########         One True Awk (preferred, but your OS's awk probably works).
########     optional third-party dependencies:
########         a terminal emulator that supports Unicode
########         a terminal-emulator font that supports Unicode

function lisp-case {
    local MANNAME='"A Phrase with mixed Case" -> "a-phrase-with-mixed-case"'
    check-args 1 1 'STRING' "$@" || return 1
    local INPUT_STRING="$1"
    echo "$INPUT_STRING" | tr '[:upper:]' '[:lower:]' | sed 's/ /-/g'
}

function kebab-case {
    local MANNAME="alias for lisp-case"
    check-args 1 1 'STRING' "$@" || return 1
    lisp-case "$@"
}

function rot13 {
    echo "$1" | tr 'A-Za-z' 'N-ZA-Mn-za-m'
}        

# Invoke strings(1) safely, because http://www.jwz.org/blog/2014/10/we-live-in-a-magical-future-where-strings-is-exploitable/

function strings {
    command strings -a "$@" \
        | $PAGER
}

# para: print any paragraph contaning the desired string. A paragraph is defined as a collection of lines followed by an all-blank line (i.e., a newline).

function para {
    STRING="$1"; shift
    awk -v ORS="\n\n" -v RS='' "/$STRING/" "$@"
}

# Same as para, with line numbers, and a line with only whitespace also
# marks a paragraph boundary

function paraln {
    # check-args 1 varargs
    local PARALN_REGEX="$1"; shift
    # awk "/$PARALN_REGEX/ {print NR \": \" \$0; f=1} f && NF {print NR \": \" \$0} /^$/ {f=0}" \
    awk "/$PARALN_REGEX/ {if (!f) {print NR \": \" \$0}; f=1} f && NF && !/$PARALN_REGEX/ {print NR \": \" \$0} /^$/ {f=0}"   "$@"
}

# fip (Filter In Place) - filter lines matching regex from a file,
# modifying the file in-place to remove those lines. If you specify
# multiple files, it does this to all of them.
function fip {
    # check-args 2 varargs
    
    local FIP_REGEX="$1"
    shift
    
    # if [[ "$1" = "-"  ]]; then
    #     local LINE
    #     while true; do
    #         LINE="$(read)"
    #         if [[ "$LINE" = "" ]]; then
    
    for FILE in "$@"; do
        command grep -vE "$FIP_REGEX" "$FILE" | sponge "$FILE"
    done
}

######## Unicode

function zero-width-joiner {
    echo -en '\u200D' | pbcopy
}

function zwj {
    zero-width-joiner "$@"
}

function nbsp {
    zero-width-joiner "$@"
}
