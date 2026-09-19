#### Python

######## dependencies:
############ 00-bash-metaprogramming.bash
############ 0x-homebrew.bash
############ 03-fd.bash
################ fd(1)

for PREFERRED in "$HOMEBREW_BIN/python3" "/usr/bin/python3"; do
    if [[ -f "$PREFERRED" ]]; then
        export PYTHON_INTERPRETER_PATH="$PREFERRED"
        break
    fi
done

function python {
    python3 "$@"
}

function venv {
    check-args 1 varargs 'NAME_FOR_THE_NEW_VIRTUALENV [UV_ARG1 [UV_ARG2 ...]]' "$@" \
        || return 1
    local VENV_NAME="$1"
    shift
    uv venv "$@" "$VENV_NAME"

    # shellcheck disable=SC1090
    source "$VENV_NAME"
}

function pipr {
    check-args 1 1 'REQUIREMENTS_TXT_FILE' "$@" || return 1
    local FILE="$1"
    uv pip install -r "$FILE"
}

