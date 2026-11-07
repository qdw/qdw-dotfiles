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
