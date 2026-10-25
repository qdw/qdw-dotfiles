# gpg-agent(1)
# Unix domain socket; no TCP/IP port
# run_gpg_agent_idempotently() {
#     # This code is from gpg-agent(1) man page:
#     if test -f $HOME/.gpg-agent-info && \
#         kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null; then
#         GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info)
#         export GPG_AGENT_INFO
#     else
#         eval $(gpg-agent --daemon)
#         echo $GPG_AGENT_INFO >$HOME/.gpg-agent-info
#     fi
# }

# ssh-agent(1).
#SSH_ADD=/usr/local/bin/ssh-add
#SSH_AGENT=/usr/local/bin/ssh-agent
#SSH_ENV="$HOME/.ssh/environment"

SSH_ADD=/usr/bin/ssh-add
SSH_AGENT=/usr/bin/ssh-agent
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
     $SSH_AGENT | sed 's/^echo/#echo/' > "${SSH_ENV}"
     chmod 600 "${SSH_ENV}"
     . "${SSH_ENV}" > /dev/null
     $SSH_ADD;

     # Or do it the way the homebrew formula suggests,
     # by putting this in ~/.bash_profile
     # 
     # eval $(ssh-agent)
     # function cleanup {
     #     echo "Killing SSH-Agent"
     #     kill -9 $SSH_AGENT_PID
     # }
     # trap cleanup EXIT
}

run_ssh_agent_idempotently() {
    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        ps -ef | command grep ${SSH_AGENT_PID} | command grep ssh-agent$ > /dev/null || {
            start_agent;
        }
    else
        start_agent;
    fi
}

# run_sshd_idempotently() {
#     if (! ps auxwww | command grep ' /usr/local/sbin/sshd$' >/dev/null); then
#         sudo /usr/local/sbin/sshd
#     fi
# }

# Tor
# ports 9001 and 9003
run_tor_idempotently() {
    if ( ! ps aux | awk '{ print $11; }' | command grep -q '^tor$'); then
        # Use a fully qualified path, avoiding the old trick where someone
        # social-engineers you to run a script that alters PATH, then gets you
        # to execute a Trojan version of (in this case) tor.
        /opt/local/bin/tor --quiet --runasdaemon 1
    fi
}
