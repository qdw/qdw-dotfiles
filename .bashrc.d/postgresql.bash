########     dependencies:
########         00-bash-metaprogramming.bash
########     third-party dependencies:
########         PostgreSQL, of course

function pg {
    MANNAME="manage multiple Homebrewed PostgreSQL versions
CAVEATS
    For this command to work, you must first set up each version's server
    on its own port, then populate ~/.pg_service.conf accordingly."

    check-args 1 varargs 'VERSION [PROGRAM [ARG1 [ARG2 ...]]]' "$@" || return 1

    local VERSION="$1"
    shift

    local PROGRAM
    if [[ "$1" ]]; then
        PROGRAM="$1"
        shift
    else
        PROGRAM="psql"
    fi
    
    local BINARY="$HOMEBREW_ROOT/opt/postgresql@${VERSION}/bin/${PROGRAM}"
    PGSERVICE="pg${VERSION}" "$BINARY" "$@"
}

function pg_dump_roles {
    pg_dumpall --roles-only --no-role-passwords "$@"
}

function pg_dump_globals_rds {
    pg_dumpall --globals-only --no-role-passwords "$@"
}
