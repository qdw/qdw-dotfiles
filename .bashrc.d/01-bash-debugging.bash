########     dependencies:
########         none

# Run this function interactively to enable tracing *without*
# the many prompt-related commands that automatically run every time I enter a
# command. This makes the trace much more uncluttered and readable.
function setx {
    unset PROMPT_COMMAND
    export PS1="\u@\h:\w\\$ "
    export PS4='+ $EPOCHREALTIME ${BASH_SOURCE}:${LINENO} '
    set -x
}

function sb {
    # shellcheck disable=SC1091
    source "$HOME/.bashrc"
}
