######## Dependencies:
########     013-homebrew.bash # if you want a recent SSH
########     03-fd.bash

function ssh-keygen {
    command ssh-keygen -t ed25519 "$@"
}

# mhost ("Map to Host(s)") - repeatedly execute a command on a list of hostnames (passing in each hostname as the command's last arg)
function mhost {
    COMMAND="$1"; shift
    for NODE in "$@"; do
        echo "$NODE"
        echo "=="
        echo
        "$NODE" "$COMMAND"
        echo
    done
}

# mshost ("Map Ssh [to] Host(s)") - execute an ssh command on a list of hosts, with paragraphized output
function mshost {
    REMOTE_COMMAND="$1"; shift
    COMMAND="ssh $REMOTE_COMMAND"
    mhost "$COMMAND" "$@"
}

# FIXME: doesn't work
# function stanza {
#     check-args 1 1 'PATTERN' "$@" || return 1
#     local PATTERN="$1"

#     para "$PATTERN" ~/.ssh/*.config \
#         | my-grep "$PATTERN"
# }

# shn ("SSH hostname") - print the canonical hostname for a ~/.ssh/config alias.
function shn {
    my-grep -A 1 "$1" ~/.ssh/*config* | bnw | tail -1 | awk '{ print $3; }'
}

# function ssh-tunnel {
#     :
# }

# function ssh-port-forw {
#     :
# }
