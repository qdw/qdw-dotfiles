# shellcheck disable=SC1090

## Dependencies:
##     - bash-git-prompt, preferably my fork: https://github.com/qdw/bash-git-prompt

export GIT_PROMPT_THEME="Custom"
export GIT_PROMPT_ONLY_IN_REPO=1

function use-gitprompt {
    local MY_FORKED_SCRIPT="$HOME/src/forks/bash-git-prompt/gitprompt.sh"
    local ORIGINAL_SCRIPT="$HOMEBREW_ROOT/share/gitprompt.sh"

    if [[ -f "$MY_FORKED_SCRIPT" ]]; then
        export GIT_PROMPT_BRANCH_OFFSET="0"
        export GIT_PROMPT_BRANCH_LIMIT="11"
        source "$MY_FORKED_SCRIPT"
    else
        source "$ORIGINAL_SCRIPT"
    fi
}

function my_prompt_function {
    local REALPATH="$(realpath "$PWD")"
    
    if [[ -d "$REALPATH/.git" \
              && ( -z "$DO_NOT_COMPLETE_REPOS_MATCHING" \
                   || ! "$REALPATH" =~ $DO_NOT_COMPLETE_REPOS_MATCHING ) ]]
    then
        use-gitprompt "$REALPATH"
        if [[ ! "$GIT_COMPLETION_ACTIVATED" ]]; then
            my-source "$BASH_COMPLETION_DIR/gh"
            my-source "$BASH_COMPLETION_DIR/git-completion.bash"
            export GIT_COMPLETION_ACTIVATED=1
        fi
    fi
}
    
MY_PROMPT_FUNCTION_DEFINITION="$(command -V my_prompt_function | tail -n +2)"
export PROMPT_COMMAND="$MY_PROMPT_FUNCTION_DEFINITION; my_prompt_function"
