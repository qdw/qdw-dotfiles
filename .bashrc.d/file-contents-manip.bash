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

#### Convert newlines to CRLF and vice versa

function dos2unix {
    perl -pie 's{ \r\n | \n | \r }{ \n }gx' "$@"
}

function unix2dos {
    perl -pie ' s{$}{\r}x; ' "$@"
}
