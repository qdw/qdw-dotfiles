#### Jenkins

# Broken, I guess?

# function jenkins {
#     local  REPO  BRANCH  JENKINS_OUTPUT_FILENAME  JENKINS_OUTPUT_URL  DOCKER_IMAGE_URL  SHORT_MESSAGE  LOGFILE
    
#     if [[ "$1" = "--no-sms" ]] || [[ "$JENKINS_NO_SMS" ]]; then
#         function sms {
#             : # no-op
#         }
#     else
#         function sms {
#             ~/...sms-me "$@"
#         }
#     fi

#     REPO="$(jenkins-repo)"
#     BRANCH="$(jenkins-branch)"
    
#     if [[ -z "$JENKINS_BASE_URL" ]]; then
#         echo 'jenkins: error: first export JENKINS_BASE_URL=[your URL]'
#     elif [[ -z "$REPO" ]]; then
#         echo 'jenkins: error: first call jenkins-repo $REPO_NAME'
#     elif [[ -z "$BRANCH" ]]; then
#         echo 'jenkins: error: first call jenkins-branch $BRANCH_NAME'
#     else
#         JENKINS_OUTPUT_URL="$JENKINS_BASE_URL/job/$REPO/job/$BRANCH/lastBuild/consoleText"
        
#         JENKINS_OUTPUT_FILENAME="$(mktemp "$HOME/tmp/jenkins-log--$REPO--$BRANCH.XXXXXX")"
#         /usr/bin/osascript ~/bin/jenkins-fetch-console-output.osascript "$JENKINS_OUTPUT_URL" \
#             | tee "$JENKINS_OUTPUT_FILENAME" \
#             | tee >(command grep -m 1 -o "docker://.*" | sed 's/docker:[/][/]//' | tee pbcopy) \
#             2>&1

#         sms "Jenkins finished the build"
#         # # This logfile-parsing code is brittle, so disable it for now
#         # if command grep --quiet SUCCESS "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Successfully built repo '$REPO', branch '$BRANCH'"
#         #     DOCKER_IMAGE_URL="$(command grep --color=never --only-matching 'harbor.*:git-.*$' "$LOGFILE" | command uniq)"
#         #     echo "$SHORT_MESSAGE"
#         #     echo "Docker image URL copied to clipboard:"            
#         #     echo "$DOCKER_IMAGE_URL" | tee >(pbcopy)
#         # elif command grep --quiet 'Log in with OpenShift' "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Jenkins login needed"
#         #     echo "First log in at $JENKINS_BASE_URL"; open "$JENKINS_BASE_URL"
#         # elif command grep --quiet 'FAILURE' "$LOGFILE" >/dev/null; then
#         #     SHORT_MESSAGE="Failed to build repo '$REPO', branch '$BRANCH'"
#         #     cat "$LOGFILE"
#         #     echo
#         #     echo "$SHORT_MESSAGE"
#         # else
#         #     SHORT_MESSAGE="Unrecognized Jenkins output for repo '$REPO', branch '$BRANCH'"
#         #     cat "$LOGFILE"
#         #     echo
#         #     echo "^ $SHORT_MESSAGE"
#         # fi
            
#         # if [[ -n "$JENKINS_SMS_ME" ]]; then
#         #     sms "$SHORT_MESSAGE"
#         # fi
#     fi
# }

# function jthis {
#     local REPO  BRANCH
#     REPO="$(gr)"
#     if [[ $? -eq 0 ]]; then
#         BRANCH="$(gb)"
#         if [[ $? -eq 0 ]]; then
#             jenkins-repo "$REPO"
#             jenkins-branch "$BRANCH"
#         fi
#     fi
#     j
# }

# function jenkins-repo {
#     local REPOFILE
#     REPOFILE="/tmp/jenkins-repo"

#     if [[ $# -eq 0 ]]; then
#         cat "$REPOFILE"
#     elif [[ $# -eq 1 ]]; then
#         echo "$1" > "$REPOFILE"
#     else
#         echo "jenkins-repo: error: too many arguments" >&2
#     fi
# }

# function jenkins-branch {
#     local BRANCHFILE
#     BRANCHFILE="/tmp/jenkins-branch"

#     if [[ $# -eq 0 ]]; then
#         cat "$BRANCHFILE"
#     elif [[ $# -eq 1 ]]; then
#         echo "$1" > "$BRANCHFILE"
#     else
#         echo "jenkins-branch: error: too many arguments" >&2
#     fi
# }
