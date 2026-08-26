#### Confluence

function confluence-docs-by-me {
    open "https://$JIRA_TEAM.atlassian.net/wiki/search?cql=creator%3DcurrentUser()"
}

#### Jira

function _j-check-env {
    local VALUE

    local MY_EXIT_STATUS=0
    for VARNAME in JIRA_USERNAME  \
                   JIRA_API_TOKEN \
                   JIRA_BASE_URL  \
                   JIRA_DEFAULT_PROJECT
    do
        VALUE="{!VARNAME}"
        if [[ -z "$VALUE" ]]; then
            MY_EXIT_STATUS=1
            echo "Error: environment variable $VARNAME is not set" 2>&1
        fi
    done
    
    return "$MY_EXIT_STATUS"
}

function jlabels {
    _j-check-env || return 2
    check-args 0 0 '[no arguments]' "$@" || return 1;

    local -A UNIQUE_LABELS

    function extract-unique-labels-from {
        while IFS= read -r line; do
            IFS=',' read -ra LABELS_ON_THIS_LINE <<< "$line"
            for LABEL in "${LABELS_ON_THIS_LINE[@]}"; do
                UNIQUE_LABELS["$LABEL"]="1"
            done
        done
    }

    extract-unique-labels-from < \
        <(jira issue list --plain \
                          --no-truncate \
                          --columns "LABELS" \
                          --jql "labels is not empty and (assignee = '$JIRA_USERNAME')" \
              | awk '{print $2}' | tail -n +2)
    
    for LBL in "${!UNIQUE_LABELS[@]}"; do
        echo "$LBL"
    done \
        | sort
}

function jiv { # view issue
    _j-check-env || return 2
    check-args 1 varargs 'ISSUE [JIRA_CLI_ARGS...]' "$@" || return 1
    local ISSUE="$1"
    shift
    jira issue view "$@" "$ISSUE"
}

function jiw { # open an issue in the web browser
    _j-check-env
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"
    open "$JIRA_BASE_URL/browse/$ISSUE"
}

function jmv { # change issue state (e.g. Planned -> Completed)
    _j-check-env || return 2
    check-args 2 2 'ISSUE_CODE STATE (example: DATAPLAT-01 Done)' "$@" \
        || return 1
    local ISSUE="$1"
    local STATE="$2"

    local STDOUT EXIT_STATUS
    STDOUT="$(jira issue move "$ISSUE" "$STATE")"
    EXIT_STATUS="$?"
    if [[ "$EXIT_STATUS" -ne 0 ]]; then
        echo "$STDOUT" >&2
        return "$EXIT_STATUS"
    fi
}

function jprog { # mark an issue as "in progress"
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "In Progress"
}

function jdone { # mark an issue as completed :)
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "Completed"
}

function jcancel { # mark issue Canceled (closest possible thing to deleting it)
    _j-check-env || return 2
    check-args 1 1 'ISSUE' "$@" || return 1
    local ISSUE="$1"

    jmv "$ISSUE" "Canceled"
}

function jrm { jcancel "$@" ;}

function jas { # assign an existing issue
    _j-check-env || return 2
    check-args 1 2 'ISSUE [USERNAME] # defaults to you!' "$@" || return 1
    local ISSUE="$1"
    local ASSIGNEE="${2:-$JIRA_USERNAME}"
    
    local STDOUT EXIT_STATUS
    STDOUT="$(jira issue assign "$ISSUE" "$JIRA_USERNAME")"
    EXIT_STATUS="$?"
    if [[ "$EXIT_STATUS" -ne 0 ]]; then
        echo "$STDOUT" >&2
        return "$EXIT_STATUS"
    fi
}

function jec { # create an epic
    _j-check-env || return 2
    check-args 1 varargs '[--ADDITIONAL -J -I -RA --CLI-FLAGS] SHORT_TITLE [LABELS]' "$@" || return 1
    local -a JIRA_CLI_ARGS

    # # Flags, like -f, --flag, or --flag-with-arg arg. Currently none supported.
    # while [[ $# -gt 0 ]]; do
    #     case "$1" in
    #         -*) echo "Unsupported option '$1'" >&2
    #             return 1;;
    # 
    #          *) break;; # first non-flag arg: start of positional args
    #     esac
    # done

    # Required positional argument: the issue summary
    local ONE_LINER="$1"; shift
    JIRA_CLI_ARGS+=("--name" "$ONE_LINER" "--summary" "$ONE_LINER")
    
    # Optional positional arguments: labels for the issue
    for LABEL in "$@"; do
        JIRA_CLI_ARGS+=("--label" "$LABEL")
    done
    
    local JIRA_CLI_OUTPUT  JIRA_CLI_EXIT_STATUS
    JIRA_CLI_OUTPUT="$(
        jira epic create --no-input "${JIRA_CLI_ARGS[@]}"
    )"
    JIRA_CLI_EXIT_STATUS="$?"

    if [[ "$JIRA_CLI_EXIT_STATUS" -eq 0 ]]; then
        local URL_OF_THE_NEW_EPIC="$(echo "$JIRA_CLI_OUTPUT" | tail -1)"
        local EPIC_NAME="${URL_OF_THE_NEW_EPIC##*/}" # e.g. "DATAPLAT-10"
        jas "$EPIC_NAME" "$JIRA_USERNAME"
        echo "$EPIC_NAME"
    else
        echo "The jira CLI app reported an error:" >&2
        echo >&2
        echo "$JIRA_CLI_OUTPUT" >&2
        return $JIRA_CLI_EXIT_STATUS
    fi
}

function jepic { jec "$@" ;}

function jic { # create a "normal" issue, which in my workflow means a Task
    _j-check-env || return 2
    check-args 1 varargs 'SUMMARY_ONELINE [JIRA_CLI_ARGS...]' "$@" || return 1
    local ONE_LINER="$1"
    shift

    local JIRA_CLI_OUTPUT JIRA_CLI_EXIT_STATUS
    JIRA_CLI_OUTPUT="$(jira issue create --no-input \
                                         --type Task \
                                         "$@" \
                                         --summary "$ONE_LINER")"
    JIRA_CLI_EXIT_STATUS="$?"
    if [[ "$JIRA_CLI_EXIT_STATUS" -eq 0 ]]; then
        local URL_OF_THE_NEW_ISSUE="$(echo "$JIRA_CLI_OUTPUT" | tail -1)"
        local ISSUE_NAME="${URL_OF_THE_NEW_ISSUE##*/}"
        jas "$ISSUE_NAME" "$JIRA_USERNAME"
        echo "$ISSUE_NAME"
    fi

    return "$JIRA_CLI_EXIT_STATUS"
}

function jc { jic "$@" ;}

function jil { # list issues (including epics)
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    
    jira issue list "$@"
}; function jls { jil "$@" ;}

function jel { # list epics only
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    jira epic list "$@"
}

function jlt { # list tasks only
    _j-check-env || return 2
    check-args 0 varargs '[jira-cli arguments]' "$@" || return 1
    
    jls --type Epic "$@"
}

function jld { # list my Done issues
    _j-check-env || return 2
    check-args 0 0 "[no arguments]" "$@" || return 1
    
    jira issue list --assignee "$JIRA_USERNAME" --status "Done"
}

function jmine {
    _j-check-env || return 2
    check-args 0 0 '[no arguments]' "$@" || return 1
    local JQL="assignee = '$JIRA_USERNAME' or creator = '$JIRA_USERNAME' ORDER BY updated DESC"
    open -u "$(_jql2url "$JQL")"
}

function jmine-epics {
    _j-check-env || return 2
    check-args 0 0 "[no arguments]" "$@" || return 1

    jira issue list --assignee "$JIRA_USERNAME" --type Epic --status Backlog --plain
}

function _jql2url {
    _j-check-env || return 2
    check-args 1 1 'JQL_QUERY_STRING' "$@" || return 1
    local QUERY="$1"
    
    local JQL_BASE_URL="${JIRA_BASE_URL}/issues/?jql="
    local URLENCODED_QUERY
    URLENCODED_QUERY=$(urlencode "$QUERY")
    if [[ $? -ne 0 ]]; then
        return $?
    else
        local FULL_URL="${JQL_BASE_URL}${URLENCODED_QUERY}"
        echo "$FULL_URL"
    fi
}

function jql {
    _j-check-env || return 2
    check-args 1 1 'JQL_QUERY_STRING' "$@" || return 1
    local QUERY="$1"
    open -u "$(_jql2url "$QUERY")"
}

# function jeip { # create an issue in an existing epic.
#     _j-check-env || return 2
#     check-args 2 varargs \
#                'EPIC_NAME ISSUE_SUMMARY_ONELINE [...]' \
#                "$@" \
#         || return 1

#     local EPIC_NAME="$1"
#     shift
    
#     local ISSUE_NAME
#     ISSUE_NAME="$(_create_issue "$@")" \
#         && jas "$ISSUE" \
#         && jira epic link --issue "$ISSUE_NAME" --epic "$EPIC_NAME"
# }
