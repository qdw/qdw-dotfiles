########     dependencies:
########         00-bash-metaprogramming.bash
########         0x-homebrew.bash
########     third-party dependencies:
########         PostgreSQL itself, of course

export POSTGRESQL_SUPPORTED_VERSIONS="15 16 17 18"
export POSTGRESQL_RC_VERSION="19"

function _pg_run_binary {
    check-args 1 varargs \
               '[--debug] PROGRAM [ARG1 [ARG2 ... ]]' "$@" \
        || return 1

    if [[ "$1" = "--debug" ]]; then
        shift
        DEBUG_FLAG="--debug"
        BINARY="/bin/echo"
        local ADDL_ENV_FLAGS="v"
    else
        if [[ "$1" ]]; then
            local BINARY="$1"
            shift
        else
            local BINARY="psql"
        fi
    fi

    env \
        "-i${ADDL_ENV_FLAGS}" \
        HOME="$HOME" \
        PGDATA="$HOMEBREW_ROOT/var/postgresql@${VERSION}" \
        PGDATABASE="postgres" \
        PGPORT="54${VERSION}" \
        PGHOST="/tmp" \
        PGUSER="$USER" \
        USER="$USER" \
        "$BINARY" \
        "$@"
}

function pg {
    local DEFAULT_VER="18"
    
    MANNAME="manage multiple Homebrewed PostgreSQL versions
CAVEATS
    For this command to work, you must first set up each version's
    postgresql.conf and pg_hba.conf so that it accepts local
    connections on port 54xy, where xy is its major version name
    (example: 5418).
"
    check-args 0 varargs \
               "[--debug] [VERSION] [PROG [ARG1 [ARG2 ...]]] # default version is $DEFAULT_VER" \
               "$@" \
        || return 1

    if [[ "$1" = "--debug" ]]; then
        local DEBUG=0
        shift
    fi
    
    local VERSION="${1:-$DEFAULT_VER}"
    shift

    local PROG
    if [[ "$1" ]]; then
        PROG="$1"
        shift
    else
        PROG="psql"
    fi
    
    local BINARY=$HOMEBREW_ROOT/opt/postgresql@${VERSION}/bin/${PROG}

    if [[ "$DEBUG" ]]; then
        _pg_run_binary --debug "$BINARY" "$@"
    fi

    _pg_run_binary "$BINARY" "$@"
}

function pg_dump_roles {
    pg_dumpall --roles-only --no-role-passwords "$@"
}

function pg_dump_globals_rds {
    pg_dumpall --globals-only --no-role-passwords "$@"
}

# replication-role -  say whether this is a primary or a replica (suitable for inclusion in your shell and/or psql prompt)

function replication_role {
    if ! which psql; then
        echo ''
    else
        INR=$(psql -tc 'select pg_is_in_recovery()')
        if [[ $INR == ' f' ]]; then
            echo '*** PRIMARY ***'
        elif [[ $INR == ' t' ]]; then
            echo 'replica'
        else
            echo '[ERROR: COULD NOT DETERMINE REPLICATION ROLE]'
        fi
    fi
}

# View or edit my ~/.pg_service.conf
# Usage:
#
#     # To open the file in a text editor:
#     ser
#
#     # To show a [section] in the file that matches a regex:
#     ser monolith
#     # ^ prints all sections with section headings matching '[.*monolith.*]'
# 
# ser - display sections of my ~/.pg_service.conf, or the whole thing

function ser {
    check-args 0 1 '[PGSERVICE_NAME]' "$@" || return 1    
    local COLORIZE="bat --language ini"

    if [[ $# -eq 0 ]]; then
        $COLORIZE ~/.pg_service.conf
    elif [[ $# -eq 1 ]]; then
        local SERVICE_NAME_SUBSTRING="$1"
        para "$SERVICE_NAME_SUBSTRING" ~/.pg_service.conf | $COLORIZE
    fi
}

function snip {
    local SNIPPET_DIR="$HOME/sql-snippets"

    if [[ ! -d "$SNIPPET_DIR" ]]; then
        err "no such directory '$SNIPPET_DIR'"
        return 2
    elif [[ $# -eq 0 ]]; then
        echo "Available snippets:"
        echo
        command ls -1 "$SNIPPET_DIR"
    elif [[ $# -eq 1 ]]; then
        if [[ "$PGSERVICE" ]]; then
            qql "$SNIPPET_DIR"
        else
            err "you must set the PGSERVICE env var"
            return 3
        fi
    fi
}

function envp {
    local MANNAME="show (or clear) PostgreSQL-related env vars"
    check-args 0 1 "[--clear]" "$@" || return 1
    if [[ $# -eq 0 ]]; then
        env | ggrep --color=always -P '^PG[^=]*'
    elif [[ $# -eq 1 ]]; then
        if [[ "$1" = "--clear" ]]; then
            for VAR in "PGDATABASE" "PGHOST" "PGPORT" "PGSERVICE" "PGUSER"; do
                unset "$VAR"
            done
        else
            err "unrecognized option '$1'"
            return 1
        fi
    fi
}
