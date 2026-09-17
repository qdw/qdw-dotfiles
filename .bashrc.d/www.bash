#### Web

export CURL_PATH="$HOMEBREW_ROOT/opt/curl/bin"

function brow { defaultbrowser "$@" ;}
function brc { brow "chrome" ;}
function brs { brow "safari" ;}

function is-it-worth-automating {
    check-args 0 0 '[no arguments]' "$@" || return 1
    
    for URL in "https://xkcd.com/1205/" \
               "https://agileadam.com/worth-automating/" \
               "https://iiwtt.seadev.tools/"; do
        open "$URL"
    done
}

function xkcd-automate-p {
    local XKCD_CHART="$HOME/doc/is_it_worth_the_time_2x.png"
    if [[ -f "$XKCD_CHART" ]]; then
        open "$XKCD_CHART"
    else
        echo "Missing file '$XKCD_CHART'. Go grab it from "
    fi
}

function xkcd {
    check-args 1 1 'COMIC_NUMBER' "$@" || return 1
    local COMIC_NUMBER="$1"
    open "https://xkcd.com/$COMIC_NUMBER"
}

# Adapted from a function posted by user Chubler_XL, in a comment on https://www.unix.com/shell-programming-and-scripting/277083-url-encoding-string-using-sed.html

function urlencode {
    # URL encode string and optionally store in shell variable
    #
    # usage: url_var <string> [var]
    local LC_ALL=C
    local encoded=""
    local i c

    for (( i=0 ; i<${#1} ; i++ )); do
        c=${1:$i:1}
        case "$c" in
            [a-zA-Z0-9/_.~-] ) encoded="${encoded}$c" ;;
            * ) printf -v encoded "%s%%%02x" "$encoded" "'${c}" ;;
        esac
    done
    
    if [[ $# -gt 1 ]]; then
        printf -v "$2" "%s" "${encoded}"
    else
        printf "%s\n" "${encoded}"
    fi
}

function url-file {
    check-args 1 2 'URL [FILENAME]' "$@" || return 1
    local URL="$1"

    function write-that {
        local THE_URL="$1"
        printf "[InternetShortcut]\n%s\n" "$THE_URL"
    }

    if [[ "$2" ]]; then
        local FILENAME="$2"
        write-that "$URL" > "$FILENAME"
    else
        write-that "$URL"
    fi
}

function http-status-follow-redirects {
    check-args 1 1 'URL' "$@" || return 1
    local URL="$1"
    local STATUS="$(curl -L --head \
                         --silent --output /dev/null \
                         --write-out '%{http_code}' \
                         "$URL")"
    echo "$STATUS"
}

function http-200 {
    local STATUS="$(http-status-follow-redirects "$@")"
    [[ "$STATUS" = 200 ]]
}

GMAIL_URL="https://mail.google.com/mail/u/0"

function gmail {
    check-args 0 2 '[SEARCH_STRING [BROWSER]]' "$@" || return 1
    local URL="https://mail.google.com/mail/u/0"
    
    if [[ $# -eq 1 ]]; then
        local SEARCH_STRING="$1"
        gmail-search "$SEARCH_STRING"
    elif [[ $# -eq 2 ]]; then
        "$BROWSER" "$URL"
    else
        open -u "$URL"
    fi
}

function gmail-search {
    check-args 0 1 'QUERY [BASE_URL' "$@" || return 1
    local QUERY="$1"
    local BASE_URL="${2:-https://mail.google.com/mail/u/0}"
    
    local URL="$BASE_URL/#search"
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open -u "${BASE_URL}/${URLENCODED_QUERY}"
}

function goog {
    check-args 1 1 'QUERY_STRING # no need to URL-encode' "$@" || return 1
    local QUERY="$1"

    local BASE_URL="https://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q="
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open "${BASE_URL}${URLENCODED_QUERY}"
}

function wayback {
    check-args 1 1 "DESIRED_URL" "$@" || return 1
    local DESIRED_URL="$1"

    local THIS_YEAR="$(date '+%Y')"
    local WAYBACK_VERSION="${THIS_YEAR}0000000000\*"

    open "https://web.archive.org/web/$WAYBACK_VERSION/$DESIRED_URL"
}

function ddg {
    check-args 1 1 'QUERY_STRING # no need to URL-encode' "$@" || return 1
    local QUERY="$1"

    local BASE_URL="https://duckduckgo.com/?t=h_&q="
    local URLENCODED_QUERY="$(urlencode "$QUERY")"
    open "${BASE_URL}${URLENCODED_QUERY}"
}

function bang {
    local MANNAME="use a DuckDuckGo bang"
    local USAGE="BANG_CODE_WITHOUT_EXCLAMATION_POINT QUERY
EXAMPLE
    ${FUNCNAME[0]} w 'Duck-Billed Platypus'"

    check-args 2 2 "$USAGE" "$@" || return 1
    local BANGCODE="$1"
    local QUERY="$2"
    
    ddg "!${BANGCODE} ${QUERY}"
}

function wi {
    local MANNAME="search Wikipedia (via DuckDuckGo)"
    check-args 1 1 'SEARCH_QUERY' "$@" || return 1
    local SEARCH_QUERY="$1"
    bang w "$SEARCH_QUERY"
}

function sheets {
    open "https://sheets.google.com/"
}

function sheet {
    open "https://sheet.new"
}
