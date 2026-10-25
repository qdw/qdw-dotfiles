# shellcheck disable=SC1090

#### Bash prompt settings
## Dependencies:
##     - bash-git-prompt, preferably my fork: https://github.com/qdw/bash-git-prompt

## Part 1/2: user-configurable settings

# PROMPT_EMIT_OSC_7="yes"
PROMPT_INCLUDE_HOST_EMOJI=
PROMPT_INCLUDE_GIT="yes"
PROMPT_ENABLE_GIT_COMPLETION="yes"
DO_NOT_COMPLETE_REPOS_MATCHING="postgresql|linux"

## Part 2/2: implementation details

function set-up-prompt {
    if [[ ! "$PROMPT_IS_SET_UP" ]]; then
        export PROMPT_IS_SET_UP="yes"
    fi

    if [[ "$PROMPT_INCLUDE_HOST_EMOJI" ]]; then
        set-host-emoji
    fi

    PROMPT_COMMAND=()
    if [[ "$PROMPT_EMIT_OSC_7" ]]; then
        PROMPT_COMMAND+=("emit-osc-7")
    fi
    if [[ "$PROMPT_INCLUDE_GIT" || "$PROMPT_ENABLE_GIT_COMPLETION" ]]; then
        PROMPT_COMMAND+=("lazy-load-git-stuff")
    fi

    if command-exists "override-ps1"; then
        override-ps1
    else
        export PS1="\\[\\e[33m\\]${HOST_EMOJI}\\u@\\h:\\w\\$\\[\\e[0m\\] "
    fi
}

function set-host-emoji {
    if [[ "$INSIDE_EMACS" ]]; then
        export HOST_EMOJI=$'\u2130 ' # cursive uppercase E
    elif [[ "$HOSTNAME" = "$LAPTOP_HOSTNAME" ]]; then
        export HOST_EMOJI=$'\uF8FF ' # Apple logo
    else
        export HOST_EMOJI=""
    fi
}

function emit-osc-7 {
    local DIR

    # URL-encode CWD, one char at a time
    local i
    for ((i = 0; i < ${#PWD}; i++)); do
        local CHAR="${PWD:i:1}"
        case "$CHAR" in
            [a-zA-Z0-9/_.~-]) url_path+="$CHAR" ;;
            *) printf -v hex '%%%02X' "'$CHAR"
               DIR+="$hex" ;;
        esac
    done

    printf '\e]7;file://%s%s\a' "$HOSTNAME" "$DIR"
}

function lazy-load-git-stuff {
    if [[ "$GIT_STUFF_ALREADY_LOADED" ]] || [[ ! -d "$PWD/.git" ]]; then
        return
    fi


    if [[ "$PROMPT_INCLUDE_GIT" ]]; then
        activate-git-prompt
    fi

    if [[ "$ENABLE_GIT_COMPLETION" ]]; then
        activate-git-completion
    fi

    export GIT_STUFF_ALREADY_LOADED="yes"
}

function activate-git-prompt {
    export GIT_PROMPT_THEME="Custom"
    export GIT_PROMPT_ONLY_IN_REPO="1"

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

function activate-git-completion {
    if [[ "$DO_NOT_COMPLETE_REPOS_MATCHING" ]] \
    && [[ "$(realpath "$PWD")" =~ "DO_NOT_COMPLETE_REPOS_MATCHING" ]]
    then
        return
    fi

    my-source "$BASH_COMPLETION_DIR/gh"
    my-source "$BASH_COMPLETION_DIR/git-completion.bash"
}

# MY_PROMPT_FUNCTION_DEFINITION=$(command -V my-prompt-function | tail -n +2)
# export PROMPT_COMMAND="$MY_PROMPT_FUNCTION_DEFINITION; my-prompt-function"

set-up-prompt
