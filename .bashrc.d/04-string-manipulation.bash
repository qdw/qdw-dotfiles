########     dependencies:
########         00-bash-metaprogramming.bash
########         02-grep.bash
########         03-pager.bash
########     third-party dependencies:
########         moreutils (for the 'sponge' command):
########         One True Awk (preferred, but your OS's awk probably works).
########     optional third-party dependencies:
########         a terminal emulator that supports Unicode,
########             using a font that supports Unicode

function lisp-case {
    local MANNAME='convert "A Phrase with mixed Case" to "a-phrase-with-mixed-case"'
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

function fip {
    local MANNAME="filter in place

SYNOPSIS
    Remove lines matching REGEX from FILE(s), modifying the files in-place.
    Motivation: I prefer grep-style regexes to sed."
    
    check-args 2 varargs 'REGEX FILE [FILE...]' "$@" || return 1
    my-grep -vE "$@" | sponge "$FILE"
}

#### Unicode

if on-macos; then
    function zero-width-joiner {
        echo -en '\u200D' | pbcopy
    }

    function zwj {
        zero-width-joiner "$@"
    }

    function nbsp {
        zero-width-joiner "$@"
    }
fi

function 1nd {
    # shellcheck disable=SC2034
    local MANNAME="print the nth whitespace-separated element
of each input line (with 1-based indexing).
Simple wrapper around awk."
    check-args 1 1 'ONE_BASED_INDEX' "$@"
    local INDEX="$1"

    # NF+n+1 trick supports negative indexes by looping around from the end
    awk -v n="$INDEX" '{print (n<0) ? $(NF+n+1) : $n}'
}
