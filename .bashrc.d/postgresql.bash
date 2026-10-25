########     dependencies:
########         00-bash-metaprogramming.bash
########         013-homebrew.bash
########     third-party dependencies:
########         PostgreSQL itself, of course

export POSTGRESQL_DEFAULT_VER="18"
export POSTGRESQL_SUPPORTED_VERSIONS="15 16 17 18"
export POSTGRESQL_RC_VERSION="19"

function _pg_run_binary {
    check-args 1 varargs 'PROGRAM [ARG1 [ARG2 ... ]]' "$@" || return 1
    local BINARY="$1"
    shift

    /usr/bin/env \
        HOME="$HOME" \
        PGDATA="$HOMEBREW_ROOT/var/postgresql@${VERSION}" \
        PGDATABASE="postgres" \
        PGHOST="/tmp" \
        PGPORT="54${VERSION}" \
        PGUSER="$USER" \
        USER="$USER" \
        "$BINARY" \
        "$@"
}

function pg {
    local VERSION PROG
    
    local MANNAME="manage multiple Homebrewed PostgreSQL versions
CAVEATS
    For this command to work, you must first set up each version's
    postgresql.conf and pg_hba.conf so that it accepts local
    connections on port 54xy, where xy is its major version name
    (example: 5418).
"
    
    check-args 0 varargs \
               "[VERSION] [PROG [ARG1 [ARG2 ...]]] # default VERSION is $POSTGRESQL_DEFAULT_VERSION; default PROG is psql" \
               "$@" \
        || return 1

    if [[ "$1" ]]; then
        VERSION="$1"
        shift
    else
        VERSION="$POSTGRESQL_DEFAULT_VERSION"
    fi

    if [[ "$1" ]]; then
        PROG="$1"
        shift
    else
        PROG="psql"
    fi

    if [[ "$VERSION" = "$POSTGRESQL_RC_VERSION" ]]; then
        setx
        # Hand-compiled release candidate; lives in a different
        # location, with a different directory structure, than
        # Homebrewed PostgreSQL
        PGDATA="/opt/postgresql/$VERSION/data" PGDATABASE="postgres" PGHOST="/tmp" PGPORT="54${VERSION}" PGUSER="$USER" "/opt/postgresql/$VERSION/bin/$PROG" \
            "$@"
        set +x
    else
        _pg_run_binary \
            "${HOMEBREW_ROOT}/opt/postgresql@${VERSION}/bin/${PROG}" \
            "$@"
    fi
}

function p-tips {
    psql-tips "$@"
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
    # shellcheck disable=SC2034
    local MANNAME="show (or clear) PostgreSQL-related env vars"
    check-args 0 1 "[--clear]" "$@" || return 1
    if [[ $# -eq 0 ]]; then
        env | ggrep --perl-regexp '^PG[^=]*'
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

# A tool for PostgreSQL C developers.
# If using this, consider also installing include-what-you-use.

function headerscheck {
    check-args 2 varargs '[FLAG1 [FLAG2] ...]' "$@" || return 1
    
    local PGINCLUDE_DIR="$HOME/src-3/pg/postgresql/src"

    local REAL_DIR
    REAL_DIR="$(realpath "$PGINCLUDE_DIR")" \
        && cd "$REAL_DIR" \
        && ./headerscheck "$@"
}
