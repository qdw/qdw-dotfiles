#!/usr/bin/env bash


function test-jira-auth() {
    if [[ ! "$JIRA_AUTH_TOKEN" ]]; then
        echo "test-jira-auth: error: env var JIRA_AUTH_TOKEN not set"
        return 1
    else
        :
    fi
}


#### JIRA settings, regrettably

## For these to work, JIRA_BASE_URL must be set

# ji [ISSUE_TYPE] [PROJECT_ID]: open the page to create a new JIRA issue
#
# ISSUE_TYPE is an integer: 1 (bug), 2 (story), or 3 (task).
#
# PROJECT_ID is the integer ID of the project in which you want to create the issue.
# To get a list of project IDs, go to https://${YOUR_HOST}.atlassian.net/jira/projects
# and then click on the project you want.
#
# Thanks to Keri Savoca for documenting this URL format:
# https://kerisavoca.medium.com/how-to-make-a-link-that-creates-a-new-issue-in-jira-54ff93c61e24
function jira-create-ticket {
    local USAGE ISSUE_TYPE PROJECT_ID ERROR_OCCURRED

    USAGE="jira-create-ticket [ISSUE_TYPE [PROJECT_ID]]
You may omit the arguments if you define env vars JIRA_DEFAULT_ISSUE_TYPE and JIRA_DEFAULT_ISSUE_ID"
    ISSUE_TYPE="$1"; if [[ ! "$ISSUE_TYPE" ]]; then ISSUE_TYPE="$JIRA_DEFAULT_ISSUE_TYPE"; fi
    PROJECT_ID="$2"; if [[ ! "$PROJECT_ID" ]]; then PROJECT_ID="$JIRA_DEFAULT_PROJECT_ID"; fi
    
    err() {
        ERROR_OCCURED=1
        echo "jira-create-ticket: error: $@"
    }
    
    [[ -n "$JIRA_BASE_URL" ]] || err "missing environment variable JIRA_BASE_URL"
    [[ -n "$ISSUE_TYPE" ]] || err "missing argument ISSUE_TYPE"
    [[ -n "$PROJECT_ID" ]] || err "missing argument PROJECT_ID"
    
    if [[ ! "$ERROR_OCCURRED" ]]; then
         open "${JIRA_BASE_URL}issues/secure/CreateIssue.jspa?issuetype=${ISSUE_TYPE}&pid=${PROJECT_ID}"
    fi
}

function jc() {
    jira-create-ticket > /dev/null
}

function jira-open-existing-ticket {
    TICKET_NUMBER="$1"

    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    else
        open "${JIRA_BASE_URL}browse/${TICKET_NUMBER}"
    fi
}

function jira-browse-site {
    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    else
        open "$JIRA_BASE_URL"
    fi
}

function j {
    if [[ $# -eq 0 ]]; then
        jira-browse-site
    elif [[ $# -eq 1 ]]; then
        jira-open-existing-ticket "$@"
    elif [[ $# -eq 2 ]]; then
        jira-create-ticket "$@"
    else
        echo "j: invalid number of arguments '$#'. Doing nothing"
    fi
}

function jql() {
    local QUERY JQL_BASE_URL FULL_URL

    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    elif [[ $# -ne 1 ]]; then
        echo "jql: error: wrong number of arguments. Usage: jql QUERY_STRING"
    else
        JQL_BASE_URL="${JIRA_BASE_URL}/issues/?jql="
        QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$QUERY") || return
        FULL_URL="${JQL_BASE_URL}${URLENCODED_QUERY}"
        open "$FULL_URL"
    fi
}

function jmine() {
    jql '(creator = currentUser() OR assignee = currentUser()) AND resolution = Unresolved order by updated DESC'
}
