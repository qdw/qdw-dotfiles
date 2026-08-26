function ip {
    local MANNAME="show this machine's public IP address"
    check-args 0 1 '[v6|v4] # default is v4' "$@" || return 1
    local SERVICE="ifconfig.me"

    if [[ $# -eq 0 ]] || [[ "$1" = "v4" ]]; then
        curl -4 "$SERVICE"
    elif [[ "$1" = "v6" ]]; then
        curl -6 "$SERVICE"
    else
        err "unsupported IP version '$1'. Valid values: 'v4', 'v6'"
        return 1
    fi
}

