######## dependencies:
########     00-bash-metaprogramming.bash
########     04-string-manipulation.bash
########     05-pager.bash
########     06-editors.bash

######## Git: general settings (not specific to GitHub)

export GIT_SHELL_PATH="$(which bash)"

function gs { git status "$@" ;}

function branch-name {
    check-args 1 1 '"QUOTED_PHRASE"' "$@" || return 1
    local PHRASE="$1"
    kebab-case "$(strip-punctuation "$PHRASE")"
}

function main {
    ~/.gitconfig-helper--get-main-branch-name "$@"
}

function popb {
    command popb --quiet "$@"
}

function pushb {    
    if [[ $# -eq 0 || ($# -eq 1 && ("$1" = "-l" || "$1" = "--list")) ]]; then
        command popb --list
    else
        command pushb --quiet "$@"
    fi
}

function strip-punctuation {
    local INPUT_STRING

    if [[ $# -eq 1 ]]; then
        INPUT_STRING="$1"
        echo "$INPUT_STRING" | tr -d '[:punct:]'
    else
        err "wrong number of arguments. Usage: strip-punctuation 'my string'"
    fi
}

# function git-delete-this-branch {
#     OLD_BRANCH=$(git branch --show-current)
#     # delete_it {
#     #     git branch -d "$OLD_BRANCH"
#     # }
    
#     if [[ "$OLD_BRANCH" = "main" ]] || [[ "$OLD_BRANCH" = "master" ]]; then
#         echo "Can't delete ${OLD_BRANCH}!"
#     else
#         git main
#         git ff
#         if [[ "$1" = "-y" ]] || [[ "$1" = "--yes"]]; then
#             delete_it
#         else
#             echo "Delete ${OLD_BRANCH}? (y/N)"
#             read REALLY_DELETE
#             if [[ $REALLY_DELETE = 'y' ]] || [[ $REALLY_DELETE = 'Y' ]]; then
#                 delete_it
#             fi
#         fi
#     fi
# }

function git-backup-helper {
    local REPO_NAME ORIG_DIR EXIT_STATUS
    if [[ $# -ne 1 ]]; then
        echo "git-backup: error: missing argument. Usage: git-backup REPO_NAME"
    else
        REPO_NAME="$1"
        ORIG_DIR=$(pwd)
        cd ~/src \
        && rsync -qa \
            "$REPO_NAME" \
            "$HOME/git-backups/${REPO_NAME}-$(date +%Y-%m-%d-%H%M%S%z)"

        cd "$ORIG_DIR"
    fi
}

function git-backup {
    check-args 0 1  'git-backup [REPO_NAME]'  "$@" || return 1
    if [[ $# -eq 1 ]]; then
        git-backup-helper "$1"
    elif [[ $# -eq 0 ]]; then
        local GIT_URL="$(gurl)"
        if [[ "$GIT_URL" ]]; then
            local REPO="${GIT_URL##*/}"
            git-backup-helper "$REPO"
        else
            err "couldn't determine which repo to operate on"
            return 2
        fi
    fi
}

function back-up-git-working-copy {
    git-backup "$@"
}

function back-up-cursor-settings {
    local GIT_WORKING_COPY_ROOT="$HOME/src/1/qdw-dotfiles"
    local GIT_CURSOR_SUBDIR="Cursor"
    local GIT_FULL_CURSOR_PATH="$GIT_WORKING_COPY_ROOT/$GIT_CURSOR_SUBDIR"
    cd "$GIT_WORKING_COPY_ROOT" || return 1
    local -a CHCHCHCHCHANGES=( "$(git status | grep "$GIT_CURSOR_SUBDIR.*")" )

    if [[ "$CHCHCHCHCHANGES" ]]; then
        echo "You have uncommitted changes in $GIT_FULL_CURSOR_PATH/" >&2
        echo "Commit these, then rerun:" >&2
        echo "$CHCHCHCHCHANGES" >&2
        return 2
    else
        local FILES_DIFFER=""
        for FILE in \
            "$HOME/Library/Application Support/Cursor/User/settings.json" \
            "$HOME/Library/Application Support/Cursor/User/keybindings.json"
        do
            local PATH_IN_GIT="$GIT_FULL_CURSOR_PATH/$(basename "$FILE")"
            if ! cmp --silent "$FILE" "$PATH_IN_GIT"; then
                FILES_DIFFER="yup"
                echo command cp -pv "$FILE" "$PATH_IN_GIT"
            fi
        done

        if [[ $FILES_DIFFER ]]; then
            git add "$GIT_CURSOR_SUBDIR"
            echo "Okay, now do a git commit in $GIT_WORKING_COPY_ROOT." >&2
        else
            echo "No differences to back up" >&2
        fi
    fi
}

# gbs (Git Branches) - list (local) git branches
function gbs { git branch ;}

# For any subdirs of . that are git working copies,
# show my local branches.
function gbss {
    local ORIG_DIR="$(pwd)"
    for DIR in $(fdd --max-depth 1 --type d); do
        if [[ -d "${DIR}.git" ]] && cd "$DIR" >/dev/null; then
             echo "$(color "$DIR" "blue")"
             gbs
             echo
        fi
        cd "$ORIG_DIR" >/dev/null    
    done
}

# gr (git repo) - print repo name (if your cwd is in a git working copy)
function gr {
    basename "$(git rev-parse --show-toplevel)"
}

# gu (Git URL) - run this in any dir inside a Git working copy to show the URL
function gurl {
    git config --get remote.origin.url
}

function greldir {
    # greldir ("Git RELative DIRectory"): given a relative path,
    # cd to "$WORKING_COPY_ROOT/$RELPATH". If we're not inside
    # a Git working copy, then err.
    check-args 0 1 'greldir [RELATIVE_DIR]' "$@" || return 1
    local REL_DIR="$1" # okay if blank
    local ABS_DIR="$(git rev-parse --show-toplevel)/$REL_DIR"
    echo "$ABS_DIR"
}

# gcd ("Git CD") - cd to a directory, relative to the root dir of this repo
# For example, if you're in directory ~/src/my-project/javascript then
# 'gcd python' will change the cwd to ~/src/my-project/python
function gcd {
    check-args 0 1 'gcd [RELATIVE_DIR]' "$@" || return 1
    local ABS_DIR="$(greldir "$@")"
    if [[ $? -eq 0 ]]; then
        cd "$ABS_DIR"
    fi
}

# branchify - munge a branch description into a branch name
# (by translating spaces to dashes and uppercase to lowercase)
function branchify {
    check-args 1 1 'DESCRIPTIVE_STRING' "$@" || return 1
    local DESTR="$1"
    echo "$DESTR" | tr '[:upper:]' '[:lower:]' | tr ' ' '-' | tr '[[:punct:]]' '-'
}

function gb-jira {
    check-args 1 1 'TINY_SUMMARY' "$@" || return 1
    local SUMMARY="$1";
    if [[ -z "$GITHUB_USERNAME" ]]; then
        err "You must set the GITHUB_USERNAME env var" >&2
        return 2
    fi

    local ISSUE DESC BRANCH_NAME
    ISSUE="$(jic "$SUMMARY")" || return 3
    DESC="$(branchify "$SUMMARY")" || return 4
    BRANCH_NAME="$GITHUB_USERNAME/$ISSUE/$DESC"
    gb "$BRANCH_NAME"
    echo "$ISSUE" | pbcopy
}

function git-hist-grep {
    check-args 2 2 'REGEX FILENAME' "$@" || return 1
    local REGEX="$1"
    local FILENAME="$2"

    git log --format='%H' -- "$FILENAME" \
        | while read COMMIT; do
              git show "$COMMIT:path/to/file" 2>/dev/null \
                  | grep -HnE "$REGEX" && echo "There was a match in commit '$COMMIT'"
          done
}

function git-latest-commits {
    (
        for DIR in "$@"; do
            cd "$DIR" >/dev/null
            echo "$(git log -n 1 --pretty=%cd --date=iso)  $DIR"
            cd - >/dev/null
        done
    ) \
    | sort --reverse
}

function for-all-commits {
    check-args 1 1 'for-all-commits COMMAND_STRING' "$@" || return 1
    CMD="$1"

    for COMMIT in $(git rev-list HEAD); do
        color-printf "red" "Commit $COMMIT"
        eval "$CMD"
    done
}

function git-show-all-commits-of {
    check-args 1 1 'git-show-all-commits-of FILE' "$@" || return $?    
    local FILE="$1"
    
    local GIT_PAGER=cat
    for COMMIT in $(git rev-list HEAD); do
        color-printf "red"  "$(git show  --pretty='tformat:%ci %an %H'  "$COMMIT" | head -1)"
        echo
        git show "${COMMIT}:${FILE}"
        echo
    done
}

function commits { git-show-all-commits-of "$@" ;}    

function git-show-name-only-diffs {
    for-all-commits 'git diff --name-only --color=always $COMMIT HEAD'
}

######## GitHub-specific settings

function branch {
    local REPO_NAME="$(basename "$(git rev-parse --show-toplevel)")"
    open "https://github.com/$GITHUB_ORG/$REPO_NAME/branches"
}

function clone {
    check-args 1 1 'URL' "$@" || return 1
    local URL="$1"
    
    local SSH_REGEX="^git[@]"
    local HTTPS_REGEX="^https://"

    if [[ "$URL" =~ $SSH_REGEX ]]; then
        git clone "$URL"
    elif [[ "$URL" =~ $HTTPS_REGEX ]]; then
        local SSH_URL
        SSH_URL="$(github-https-to-ssh "$URL")"
        SED_EXIT_STATUS=$?
        if [[ $SED_EXIT_STATUS -ne 0 ]]; then
            err "regex substitution failed for URL '$URL'"
            return $SED_EXIT_STATUS
        elif [[ ! "$SSH_URL" ]]; then
            err "regex substitution succeeeded for URL '$URL', but returned the empty string"
            return 2
        else
            git clone "$SSH_URL"
        fi
    else
        err "unrecognized URL format in URL '$URL'"
        return 1
    fi
}

# function merge {
#     check-args 0 1 'PR_NUMBER' "$@" || return 1
#     
#     local PR_NUMBER="$(prs | cat | awk '{print $1}')"
#     
#     if [[ $? -eq 0 ]] && [[ $PR_NUMBER ]]; then
#         gh pr merge --squash "$PR_NUMBER"
#     fi
# }

function github-https-to-ssh {
    local MANNAME="convert a GitHub web URL to ssh format, which you can clone"
    check-args 1 1 'HTTPS_URL' "$@" || return 1
    local HTTPS_URL="$1"
    echo "$HTTPS_URL" | sed -e s'|^https://github.com|git@github.com:|'
}

function gists {
    gh git list "$@"
}

function readme {
    check-args 0 1 '[FILE]' "$@" || return 1
    local FILE="$1"
    local REPO_URL

    if [[ "$FILE" ]]; then
        bat "$FILE"
    elif [[ REPO_URL="$(gurl)" ]] && {
             [[ "$REPO_URL" =~ git@github.com ]] || \
             [[ "$REPO_URL" =~ https://github.com/ ]] }; then
        gh repo view "$@"
    elif [[ "$(echo README* | wc -w)" -gt 1 ]]; then
        ERRMSG="$(printf "There are multiple README files. Pass the one you want as an arg\n%s" \
                        "$(echo README*)")"
        err "$ERRMSG"
    elif [[ -f "README.md" ]]; then
        bat README.md
    elif [[ -f "README" ]]; then
        bat README
    else
        err "No README.md/README. Re-run and pass the filename as an arg"
    fi
}

function team {
    check-args 0 1 '[GITHUB_TEAM_NAME]' "$@" || return 1
    local TEAM_NAME="$1" # may be empty, which is okay
    open "https://github.com/orgs/$GITHUB_ORG/teams/$TEAM_NAME"
}

function owner_echo {
    local OWNER="$(head -1 CODEOWNERS | sed 's/^[*] [@]//')"
    echo "$OWNER"
}

function owner {
    local TEAM_NAME="$(owner_echo | sed s"|^$GITHUB_ORG[/]||")"
    team "$TEAM_NAME"
}

function get-main-branch-name {
    local MAIN_OR_MASTER
    
    MAIN_OR_MASTER="$(command git branch --remotes | command ggrep --perl-regexp --only-matching '  origin/HEAD -> origin/(?=master|main)\K.*$' | sort | head -1)"
    echo "$MAIN_OR_MASTER"
}

# gb (Git Branch) - create a new branch based on the main one, and switch to it
function gb {
    check-args 0 1 '[NEW_BRANCH_NAME]' "$@" || return 1
    if [[ $# -eq 0 ]]; then
        git branch --show-current
    else
        local MAIN_BRANCH
        local NEW_BRANCH_NAME="$1"
        MAIN_BRANCH="$(get-main-branch-name)" \
            && git switch --quiet "$MAIN_BRANCH" \
            && git pull --quiet --ff-only  \
            && git switch --create "$NEW_BRANCH_NAME" "$MAIN_BRANCH" \
                | command grep -vE 'remote:'
    fi
}

# gist - view gists or create a gist
function gist {
    check-args 0 2 \
               '[no args] | DESCRIPTION [FILENAME] # if you pass no FILENAME, it will read from standard input' \
               "$@" \
        || return 1
    
    local DESCRIPTION
    local FILENAME="$(command mktemp -p "$HOME/tmp/")"

    if [[ $# -eq 0 ]]; then
        local GIST_EDITOR
        for EDITOR_VAR in GIT_EDITOR VISUAL EDITOR; do
            if [[ -n "$EDITOR_VAR" ]]; then
                GIST_EDITOR="$EDITOR_VAR"
                break
            fi
        done
        if [[ -z "$EDITOR_VAR" ]]; then
            GIST_EDITOR="vi"
        fi

        "$GIST_EDITOR" "$FILENAME"

    elif [[ $# -eq 1 ]]; then
        # Assume we were invoked as some_command | gist "some description"
        DESCRIPTION="$1"
        local PIPED_INPUT="$(cat)"
        echo "$PIPED_INPUT" > "$FILENAME"

    elif [[ $# -eq 2 ]]; then
        DESCRIPTION="$1"
        FILENAME="$2"
    fi
    
    local DESCRIPTION_CLAUSE
    if [[ "$DESCRIPTION" ]]; then
        DESCRIPTION_CLAUSE="--desc '$DESCRIPTION'"
    fi
    
    gh gist create --web --filename "$FILENAME" $DESCRIPTION_CLAUSE
}

# gtr (Git Team and Repo) - run this in any dir inside a working copy of a GitHub repo to show the team name plus repo name in GitHub notation, e.g., myorg/myrepo
function gtr {
    local REPO_URL RETVAL
    REPO_URL="$(gu)"
    if [[ "$REPO_URL" =~ ^git[@]github.com[:](.*[/].*) ]]; then
        RETVAL=${BASH_REMATCH[1]}
        echo "$RETVAL"
    else
        echo "gtr: error: couldn't parse URL '$REPO_URL'" 2> /dev/null
    fi
}
   

# gline (Git Line) - open a file in GitHub's web UI, at a given line
function gline {
    check-args 1 2 'FILENAME [LINENO]' "$@" || return 1
    local FILENAME="$1"
    local LINENO="${2:-1}"
    
    local GIT_WORKING_COPY_ROOT_DIR="$(git rev-parse --show-toplevel)"
    FILENAME="${FILENAME/$GIT_WORKING_COPY_ROOT_DIR\//}"

    local BRANCH="$(git branch --show-current)"    
    gh browse --branch "$BRANCH" "${FILENAME}:${LINENO}"
}

function lst {
    local LAST_ARG_IS_THE_FILENAME

    if [[ $# -eq 0 ]]; then
        git diff HEAD^ HEAD
    else
        local LAST_ARG_IS_THE_FILENAME

        for ARG in "$@"; do
            LAST_ARG_IS_THE_FILENAME="$ARG"
        done

        # Set $@ to a an array slice
        # that includies all the args
        # except the last one ($# - 1)
        set -- "${@:1:${#-1}}"

        git diff "$@" HEAD^ HEAD -- "$LAST_ARG_IS_THE_FILENAME"
    fi
}

function gclp {
    local MANNAME="gclp (Git CLone Public) - quickly clone a GitHub repo"
    check-args 1 varargs '${USER}/${REPO} [ENCLOSING_DIR] [GITARGS...]' "$@" \
        || return 1
    
    local USER_SLASH_REPO="$1"
    shift

    local ENCLOSING_DIR
    if [[ "$1" ]]; then
        ENCLOSING_DIR="$1"
        shift
    else
        ENCLOSING_DIR="$HOME/src-3"
    fi
    
    echo "cd $ENCLOSING_DIR"
    cd "$ENCLOSING_DIR" \
        && git clone "git@github.com:$USER_SLASH_REPO" "$@"
    EC=$?
    
    echo "cd -"
    cd - >/dev/null

    return $EC
}

# gclo (Git CLone Organization) - quickly clone a repo in a GitHub organization

function gclo {
    check-args 1 varargs 'REPO_NAME [SRC_DIR] # defaults to ~/src' "$@" || return 1

    local REPO="$1"
    shift
    
    local SRC_DIR="${1:-$HOME/src}"
    shift
    
    cd "$SRC_DIR" >/dev/null \
        && { git clone "git@github.com:${GITHUB_ORG}/${REPO}" "$@" \
                 && cd "$REPO" >/dev/null \
                 && git config user.email "$EMAIL"
             cd - >/dev/null
           }
}

# ginit ("git init") - go through the multi-command dance to create a local git repo
function ginit {
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "gi: error: $(pwd) is already under git control"
    else
        git init \
            && git add . \
            && git commit -a -m 'Initial commit' \
            && git branch -m master main 2>/dev/null
    fi
}

function gl {
    local N_COMMITS

    if [[ $# -eq 0 ]]; then
        N_COMMITS=3
    elif [[ $# -eq 1 ]]; then
        N_COMMITS="$1"
        shift 1
    elif [[ $# -eq 2 ]] && [[ $1 = "-n" ]]; then
        N_COMMITS="$2"
        shift 2
    fi

    git log -n "$N_COMMITS" "$@"
}

function gitlawb {
    /opt/gitlawb/gl "$@"
}

function merge {
    gh pr merge --delete-branch --squash "$@"
}

# repos - multitool for listing/searching GitHub repositories 
function repos {
    local PATTERN VERBOSE

    if [[ ! "$GITHUB_ORG" ]]; then
        err "env var GITHUB_ORG is not set. Set it to your team name"
        return 1
    fi
        
    if [[ $# -eq 0 ]]; then
        PATTERN="." # match anything, i.e. print all repos in $GITHUB_ORG
    else
        PATTERN="$1"
        VERBOSE="$2" # may be the empty string
    fi
    
    function verbose-print {
        if [[ "$VERBOSE" ]]; then
            echo "$@" >&2
        fi
    }
    
    # Search in various places for repos matching PATTERN,
    # stopping at the first place where we find a match.
    {
        verbose-print "Searching $GITHUB_ORG repos..."
        verbose-print "------------------------------"
        verbose-print
        gh repo list --no-archived --limit 2000 "$GITHUB_ORG" | command grep -E "$PATTERN"
    } \
    || \
    {
        verbose-print "Searching personal repos..."
        verbose-print "---------------------------"
        verbose-print
        gh repo list --no-archived --limit 2000 | command grep -E "$PATTERN"
    } \
    || \
    {
        verbose-print "Searching all repos on github.com (in browser)..."
        verbose-print "-------------------------------------------------"
        verbose-print
        open "https://github.com/search?type=repositories&q=${PATTERN}"
    }
}

function github-repo-search {
    local MANNAME="search GitHub for repos whose names match PATTERN"
    check-args 1 1 'PATTERN' "$@" || return 1
    local PATTERN="$1"
    open "https://github.com/search?type=repositories&q=${PATTERN}"
}

# repo - open a named GitHub named repo in a web browser
function repo {
    REPO_NAME="$1"
    if [[ -z "$REPO_NAME" ]]; then
       gh repo view --web
    elif [[ "$REPO_NAME" = "list" ]]; then
        repos "."
    else
        # Try opening a repo of this exact name,
        # first in my org and then among repos owned by by GitHub username.
        #
        # If that fails, then do some fuzzy searching.
        if     ! gh repo view --web "$GITHUB_ORG/$REPO_NAME" >/dev/null 2>&1 \
            && ! gh repo view --web "$REPO_NAME" >/dev/null 2>&1; then
            
            echo "Couldn't find a $GITHUB_ORG repo named '$REPO_NAME'" >&2
            echo "Searching for similarly-named repos..." >&2
            echo "======================================" >&2
            echo >&2
            
            repos "$REPO_NAME" true 1>&2
        fi
    fi
}

function actions {
    local GIT_URL="$(gurl)"
    local CANONICALIZED_URL=""
    
    if [[ -z "$GIT_URL" ]]; then
        return 1
    elif [[ "$GIT_URL" =~ ^https:[/][/]github.com[/] ]]; then
        CANONICALIZED_URL="$GIT_URL"
    elif [[ "$GIT_URL" =~ ^git@github\.com:([^/]+)/([^/]+)(\.git)?$ ]]; then
        local OWNER="${BASH_REMATCH[1]}"
        local REPO="${BASH_REMATCH[2]}"
        CANONICALIZED_URL="https://github.com/${OWNER}/${REPO}"
    fi
    
    local ACTIONS_URL="$CANONICALIZED_URL/actions"
    open -u "$ACTIONS_URL"
}

# Open the latest GitHub action (or the main GitHub actions page if none found)
function action {
    check-args 0 0 'gac' "$@" || return 1

    local LATEST_ACTION_ID="$(gh run list --json databaseId --jq '.[0].databaseId')"
    if [[ "$LATEST_ACTION_ID" ]]; then
        gh run view --web "$LATEST_ACTION_ID" >/dev/null
    else
        err "no GitHub actions found (at least, not for this branch)"
        return 2
    fi
}

# PR (Pull Request) - manipulate GitHub pull requests
# Depends on the official GitHub CLI tool, 'gh'.
# Usage:
#
#     pr        # Show my pull requests (for the repo whose directory I'm in).
#     pr v      # Same thing.
#     pr o      # Same thing.
#     pr view   # Same thing.
#
#     pr cr     # Create a PR, pushing first if necessary.
#     pr create # Same thing.
#
#     pr cro    # Create a PR as above, and open it in a browser
#               #   so that I can watch its deploy scripts run.
#     pr crv    # Same thing.
#
# PR (Pull Request) - manipulate GitHub pull requests

# prl ("Pull Request: Latest"): open my most recent PR from this repo, in a web browser
function prl {    
    local URL="$(gh pr list --author "$GITHUB_USERNAME" --json url --jq '.[0].url')"
    if [[ -z "$URL" ]]; then
        err "you don't have any PRs open in this repo"
    else
        echo "$URL"
        open "$URL"
    fi
}

function pr {
    function list_my_prs {
        gh pr list --author "$GITHUB_USERNAME" "$@"
    }

    function push_it {
        # Push to a remote branch (creating if necessary)
        local BRANCH
        BRANCH="$(git branch --show-current)"
        git push --set-upstream origin "$BRANCH" --quiet
    }

    function create_pr {
        local GIT_FETCH_STDOUTERR=$(git fetch 2>&1)
        if [[ "$GIT_FETCH_STDOUTERR" ]]; then
            # If there's stuff to merge, cowardly bail
            # and make the user merge it manually
            echo "$GIT_FETCH_STDOUTERR"
        else
            push_it
            
            local CMD
            if [[ -n "$PR_TITLE" ]]; then
                CMD="gh pr create --title '$PR_TITLE'"
            else
                CMD="gh pr create --fill"
            fi

            $CMD 2>&1 | tail -1
        fi
    }
    
    function main {
        local PR_NUMBER_REGEX='^[[:digit:]]+$'
        
        if [[ "$1" = "--list" ]]; then
            shift
            list_my_prs
        elif [[ "$1" =~ $PR_NUMBER_REGEX ]]; then
            gh pr view --web "$1"
        else
            create_pr
        fi
    }

    main "$@"
}

function prs {
    gh pr list | command grep -vE '^no open pull requests'
}

function prm {
    gh pr list --author '@me' | command grep -vE '^no open pull requests'
}

# purl ("Pull request URL") - print the URL of the latest pull request on this branch.
# Also copy it to the clipboard.
function purl {
    gh pr view | command grep --color=never --only-matching 'https://.*' | tee >(pbcopy)
}

function pulls {
    open "https://github.com/pulls"
}

function git-list-all-repos-in-org {
    while true; do
        gh api "/orgs/$GITHUB_ORG/repos" \
            --paginate \
            --jq '.[] | [.name, .updated_at] | @tsv' \
            -F per_page=$PER_PAGE \
            -F sort=updated \
            -F direction=desc \
            -F page=$PAGE || break
        ((PAGE++))
    done
}

function git-e {
    check-args 2 2 'REPO FILENAME' "$@" || return 1
    local REPO="GITHUB_ORG/$1"
    local FILENAME="$2"

    STATUS=$(gh api "/repos/$REPO/contents/$DIRECTORY" \
       -H "Accept: application/vnd.github.v3+json" \
        --silent \
        --method GET \
        --output /dev/null
        --show-response-status \
            | tail -1)
                
    if [[ "$STATUS" == *"200"* ]]; then
        echo "$REPO has $DIRECTORY"
    else
        echo "$REPO missing $DIRECTORY"
    fi
}

function git-infra-dirs {
    for REPO in $(git-list-all-repos-in-org); do
        git-e "$REPO" "infra"
    done
}

######## gitlawb, a distributed take on Git. Alpha.

GITLAWB_PATH="/opt/gitlawb"
export GITLAWB_NODE="https://node.gitlawb.com"
