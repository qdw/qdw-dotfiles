function slack-channel {
    check-args \
        1 \
        1 \
        "slack-channel CHANNEL_NAME # (with or without the leading '#'; doesn't matter)" \
        "$@" \
     || return 1

    # Remove leading '#' if present
    local CHANNEL_NAME="${1/##/}"
    
    if on-macos; then
        macos-open-slack-channel.scpt "$CHANNEL_NAME"
    else
        open "https://${SLACK_TEAM}.slack.com/channels/${CHANNEL_NAME}"
    fi
}

SLACK_EXTRAS="$HOME/.site.bash.d/slack-urls.bash"
if [[ -f "$SLACK_EXTRAS" ]] && (( BASH_VERSINFO[0] >= 5 )); then
    # shellcheck disable=SC1090
    source "$SLACK_EXTRAS"

    function msg {
        check-args 1 1 'SLACK_USERNAME' "$@" || return 1
        URL="$(slack-username2url "$1")"
        if [[ $URL ]]; then
            open "$URL"
        fi
    }

    function slack-username2url {
        check-args 1 1 'SLACK_USERNAME # e.g. alice.hacker' "$@" || return 1
        local SLACK_USERNAME="$1"

        local URL="${SLACK_USERNAME2URL["$SLACK_USERNAME"]}"
        if [[ -z "$URL" ]]; then
            error "the URL for $SLACK_USERNAME is unknown"
            error "Add it to var SLACK_USERNAME2URL in file $SLACK_EXTRAS"
            return 2
        else
            echo "$URL"
        fi
    }
fi
