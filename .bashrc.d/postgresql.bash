########     dependencies:
########         00-bash-metaprogramming.bash
########     third-party dependencies:
########         PostgreSQL itself, of course

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
    local COLORIZE="bat --language ini"

    if [[ $# -eq 0 ]]; then
        $COLORIZE ~/.pg_service.conf
    else
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
