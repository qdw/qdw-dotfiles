#### JSON and YAML

export JQ_COLORS="1;31:1;31:1;31:0;38:0;32:0;38:0:33"

function pp {
    local MANNAME="pretty-print

SYNOPSIS
    my-json-generating-program | pp [--json]
    my-yaml-generating-program | pp --yaml

    --json is the default. Only json and yaml are supported at present."

    check-args 0 1 '[--json]' "$@" || return 1

    case "$1" in
        "" | "--json") jq -C | $PAGER ;;
        "--yml" | "--yaml") yq -c | $PAGER ;;
        *) err "unsupported format '$1'"; return 1 ;;
    esac
}

function yv {
    if [[ $# -ne 1 ]]; then
        echo "yv: error: wrong number of arguments. Usage: yv YAML_KEY_NAME"
    else
        KEY="$1"
        shift
        
        #FIXME: figure out how to glob for *.yaml case-insensitively. Maybe use fd?
        # command grep --no-filename "${KEY}:[ ]*" *.yaml | sort | uniq | sed "s/^[ ]*${KEY}:[ ]*//"
        fdd "$@" .yaml -X grep "${KEY}:"
    fi
}

function yvu {
    yv "$@" | sed "s/.*${KEY}:[ ]*//" | sort | uniq
}

# ys ("YAML Sort")" - take a file and sort its YAML keys alphabetically.
function ys {
    local FILE="$1"
    yq -P "sort_keys(..)" "$FILE"
}

# yd ("YAML Diff") - diff YAML files, sorting their keys first. Does not change the original files.
function yd {
    for FILE in "$1" "$2"; do
        ys "$FILE"
    done
    
    diff "$1" "$2"
}

function json2yaml {
    if [[ $# -eq 0 ]]; then
        echo "json2yaml: error: missing filename argument"
    else
        local JSON_FILENAME="$1"
        shift
        yq eval -p json -o yaml "$JSON_FILENAME" "$@"
    fi
}

function json-key { 
    check-args 1 varargs 'json-key KEY_NAME [FILE1 [FILE2...]]' "$@"
    local KEY_NAME="$1"
    shift
    if [[ $# -eq 0 ]]; then
        grep -i -R "\"$KEY_NAME\":"  "."
    else
        grep -i "\"$KEY_NAME\":"  "$@"
    fi
}
    
function yaml2json {
    if [[ $# -eq 0 ]]; then
        echo "yaml2json: error: missing filename argument"
    else
        local YAML_FILENAME="$1"
        shift
        yq eval -p yaml -o json "$YAML_FILENAME" "$@"
    fi
}
