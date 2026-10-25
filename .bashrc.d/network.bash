function ip {
    # shellcheck disable=SC2034
    local MANNAME="show this machine's public IP address"
    check-args 0 1 '[v6|v4] # default is v4' "$@" || return 1
    local SERVICE="ifconfig.me"

    if [[ $# -eq 0 ]] || [[ "$1" = "v4" ]]; then
        curl -4 "$SERVICE"
        echo
    elif [[ "$1" = "v6" ]]; then
        curl -6 "$SERVICE"
        echo
    else
        err "unsupported IP version '$1'. Valid values: 'v4', 'v6'"
        return 1
    fi
}

function ips {
    local MANNAME="ips - show all IPv4 interfaces on thist machine"
    check-args 0 0 '[no arguments]' "$@" || return 1
    
    /sbin/ifconfig -a \
        | my-grep --color=never 'inet ' \
        | my-grep --color=never -v 'inet6 ' \
        | my-grep 'inet' # add color
}

function ipv6 {
    local MANNAME="ipv6 - show all IPv6 interfaces on this machine"
    check-args 0 0 '[no arguments]' "$@" || return 1
    /sbin/ifconfig -a | my-grep 'inet6 '
}

function whois {
    carp "warning: whois is deprecated; using rdap instead"
    rdap "$@" 2>&1 | $PAGER 
}
