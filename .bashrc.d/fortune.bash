#### fortune(1), an old BSD app that prints a random message from a file

function fortune {
    check-args 0 1 '[MY_FORTUNE_FILE] # default is "$HOME/fortunes/default"' \
        "$@" || return 1
    FILE="${1:-$HOME/fortunes/default}"
    
    if [[ -f "$FILE" ]] && [[ "$FILE" -nt "${FILE}.dat" ]]; then
        if ! command strfile -rs "$FILE"; then
            return
        fi
    fi

    command fortune "$FILE"
}

#### Requires you to download a fortune-formatted version of Oblique Strategies

function oblique {
    fortune "oblique-strategies"
}
