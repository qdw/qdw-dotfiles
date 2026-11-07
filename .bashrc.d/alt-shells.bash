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
