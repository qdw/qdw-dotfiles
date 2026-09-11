######## Dependencies:
########     ./0x-homebrew.bash # if you want a recent SSH
########     ./03-fd.bash

function ssh-keygen {
    command ssh-keygen -t ed25519 "$@"
}

# mshost ("Map Ssh [to] Host(s)") - execute an ssh command on a list of hosts, with paragraphized output
function mshost {
    REMOTE_COMMAND="$1"; shift
    COMMAND="ssh $REMOTE_COMMAND"
    mhost "$COMMAND" "$@"
}

# function ssh-tunnel {
#     :
# }

# function ssh-port-forw {
#     :
# }
