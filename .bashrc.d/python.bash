#### Python

######## dependencies:
############ 00-bash-metaprogramming.bash
############ 0x-homebrew.bash
############ 03-fd.bash
################ fd(1)

for PREFERRED in "$HOMEBREW_BIN/python3" "/usr/bin/python3"; do
    if [[ -f "$PREFERRED" ]]; then
        PYTHON_INTERPRETER_PATH="$PREFERRED"
        break
    fi
done

function python {
    python3 "$@"
}

function defs {
    # Show Python function names
    check-args 0 varargs \
               '[GREP_ARG_1 [GREP-ARG_2 ...]] [DIRECTORY] # defaults to .' \
               "$@" \
        || return 1

    # Get the last argument, and remove it from $@
    local PTH
    if [[ $# -ne 0 ]]; then
        local PTH="${@[$# - 1]}"
        unset "${_[$# - 1]}"
        echo "PTH: $PTH"
        echo "remaining args: $@"
    else
         PTH="."
    fi
    
    command fd -t f '.py$' \
        -x command grep -RiE --color=always "$@"  "^[ ]*def[ ]"  "{}" \; \
        "$PTH"
}

function venv {
    check-args 1 varargs 'NAME_FOR_THE_NEW_VIRTUALENV [UV_ARG1 [UV_ARG2 ...]]' "$@" \
        || return 1
    local VENV_NAME="$1"
    shift
    uv venv "$@" "$VENV_NAME"
    source "$VENV_NAME"
}

function pipr {
    check-args 1 1 'REQUIREMENTS_TXT_FILE' "$@" || return 1
    local FILE="$1"
    uv pip install -r "$FILE"
}

