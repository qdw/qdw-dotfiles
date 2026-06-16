########     dependencies:
########         none

# Uncomment this to enable various hackish debugging statements in this file.
# export MY_DEBUG_BASH="1"

# Run this function interactively to enable tracing *without*
# the many prompt-related commands that automatically run every time I enter a
# command. This makes the trace much more uncluttered and readable.

function setx {
    unset PROMPT_COMMAND
    export PS1="\u@\h:\w\\$ "
    export PS4='+ $LINENO: ${BASH_SOURCE}:${LINENO} -> '
    set -x
}

function sb {
    source "$HOME/.bashrc"
}
