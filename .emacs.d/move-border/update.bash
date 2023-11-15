#!/usr/bin/env bash --noprofile --norc

MY_PATH="$0"
MY_DIR="$(dirname "$MY_PATH")"
cd "$MY_DIR"

command curl \
     -L https://raw.githubusercontent.com/ramnes/move-border/master/move-border.el \
        > new-move-border.el
CURL_EXIT_STATUS=$?
if [[ $CURL_EXIT_STATUS -ne 0 ]]; then
    command mv new-move-border.el new-move-border--download-failed
    exit $CURL_EXIT_STATUS
elif [[ ! -f move-border.el ]] || cmp move-border.el new-move-border.el; then
    command rm new-move-border.el
    exit 0
else
    command mv move-border.el "move-border-retired-"$(date +%Y%m%dT%H%M%S%z)""
    command mv new-move-border.el move-border.el
fi
