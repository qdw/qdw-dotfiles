#!/usr/bin/env bash

#### Jira settings, regrettably (for these to work, JIRA_BASE_URL must be set)

### Good functions

function jql2url {
    local QUERY  JQL_BASE_URL  FULL_URL

    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    elif [[ $# -lt 1 ]]; then
        echo "jql: error: wrong number of arguments. Usage: jql QUERY_STRING"
    else
        JQL_BASE_URL="${JIRA_BASE_URL}/issues/?jql="

        QUERY="$1"
        shift
        if [[ $# -gt 0 ]]; then
            URL="$URL AND $@"
        fi
                
        URLENCODED_QUERY=$(urlencode "$QUERY") || return
        FULL_URL="${JQL_BASE_URL}${URLENCODED_QUERY}"
        echo "$FULL_URL"
    fi
}

function jql {
    open -u "$(jql2url "$@")"
}

function cjql {
    curl --silent --location "$(jql2url "$@")" | $PAGER
}

# function list-tickets {
#     cjql '(creator = currentUser() OR assignee = currentUser()) AND resolution = Unresolved order by updated DESC'
# }

### Bad functions

function jira-url-for-ticket {
    local TICKET_NUMBER
    
    if [[ $# -ne 1 ]]; then
        echo "jira-url-for-ticket: error: wrong number of arguments. Usage: jira-url-for-ticket TICKET_NUMBER"
    elif [[ -z "$JIRA_BASE_URL" ]]; then
        echo "jira-url-for-ticket: error: you must set the JIRA_BASE_URL environment variable"
    else
    
        TICKET_NUMBER="$1"
        
        # If the user passed a bare number, then prepend
        # the default project (e.g., "DATAPLAT-")
        if [[ "$TICKET_NUMBER" =~ ^[[:digit:]]+ ]]; then
            if [[ -z "$JIRA_DEFAULT_PROJECT" ]]; then
                echo "jira-url-for-ticket: error: numeric ticket ID given but JIRA_DEFAULT_PROCESS not set"
                return
            else
                TICKET_NUMBER="${JIRA_DEFAULT_PROJECT}-${TICKET_NUMBER}"
            fi
        fi

        echo "${JIRA_BASE_URL}/browse/${TICKET_NUMBER}"
    fi
}

function jira-open-existing-ticket {
    open -u "$(jira-url-for-ticket "$@")"
}

function ticket {
    open "$(jira-url-for-ticket "$@")"
}

# Take a file of text pasted from the left sidebar of Jira's "detailed view".
# Output properly labeled emacs org-mode links for each named ticket.
# 
# FIXME: later, make this work with stdin if "-" is the filename.
function jira-to-org {
    local FILENAME
    
    if [[ $# -ne 1 ]]; then
        echo "jira-to-org: error: missing argument. Usage: jira-to-org FILE" >&2
    else
        FILENAME="$1"   
        for TICKET_NAME in $(awk 'NR % 2 == 1 {print $NF}'  "$FILENAME"); do
            echo "[[$(jira-url-for "$TICKET_NAME")][$TICKET_NAME]]"
        done
    fi
}

function jira-browse-site {
    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    else
        open "$JIRA_BASE_URL"
    fi
}

function jira {
    if [[ $# -eq 0 ]]; then
        jira-browse-site
    elif [[ $# -eq 1 ]]; then
        jira-open-existing-ticket "$@"
    else
        echo "j: invalid number of arguments '$#'. Doing nothing"
    fi
}

#### Confluence settings

function confluence {
    open "$JIRA_BASE_URL/wiki/home"
}
