function nanp {
    check-args 1 1 'AREA_CODE' "$@" || return 1
    local AREA_CODE="$1"

    local CSV_URL="https://reports.nanpa.com/public/npa_report.csv"
    local CACHED_COPY="$HOME/.site.bash.d/npa_report.csv"
    if [[ ! -f "$CACHED_COPY" ]]; then
        local CURL_ES
        curl -L "$CSV_URL" > "$CACHED_COPY"
        CURL_ES=$?
        if [[ $CURL_ES -ne 0 ]]; then
            return $CURL_ES
        fi
    fi

    # Don't try to parse out location fields,
    # since they aren't consistently filled in,
    # and since the CSV file doesn't use proper double-quoting
    # Or escaping, which makes it hard to parse.

    # Column names
    local HEADER
    HEADER="$(head -2 "$CACHED_COPY" | tail -1 | grep ",")"

    local -a MATCHING_LINES
    mapfile -t MATCHING_LINES \
            < <(tail -n "+2" "$CACHED_COPY" | grep "^${AREA_CODE}")

    if [[ ${#MATCHING_LINES} -gt 0 ]]; then
        echo "$HEADER"
        echo
        echo "${MATCHING_LINES[@]}" | grep "," # highlight field separators
        return 0
    else
        return 2
    fi
}
