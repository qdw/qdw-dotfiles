# rubygems documentation server
# port 8080
run_gem_server_idempotently() {
    if (! p 'gem server' >/dev/null 2>&1); then
        sudo /bin/bash -c 'gem server >/dev/null 2>&1 &'
    fi
}

# gpg-agent(1)
# Unix domain socket; no TCP/IP port
run_gpg_agent_idempotently() {
    # This code is from gpg-agent(1) man page:
    if test -f $HOME/.gpg-agent-info && \
        kill -0 $(cut -d: -f 2 $HOME/.gpg-agent-info) 2>/dev/null; then
        GPG_AGENT_INFO=$(cat $HOME/.gpg-agent-info)
        export GPG_AGENT_INFO
    else
        eval $(gpg-agent --daemon)
        echo $GPG_AGENT_INFO >$HOME/.gpg-agent-info
    fi
}

# ssh-agent(1)
# run_ssh_agent_idempotently() {
#     if test -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT"; then
#         ssh-agent
#         trap "kill $SSH_AGENT_PID" 0
#     fi
# }

# Subversion server. I used to have to run this for some clients.
#
# It uses one of two ports, depending on how you run it:
# 3690 is the port for the custom Subversion protocol (which I used);
# 80 is the port for the HTTP-based protocol.
run_svn_idempotently() {
    if ( ! pq svnserve); then
        /usr/bin/svnserve -d -r ~/.subversion_repositories >/dev/null 2>&1
    fi
}

# Tor
# ports 9001 and 9003
run_tor_idempotently() {
    if ( ! ps aux | awk '{ print $11; }' | grep -q '^tor$'); then
        # Use a fully qualified path, avoiding the old trick where someone
        # social-engineers you to run a script that alters PATH, then gets you
        # to execute a Trojan version of (in this case) tor.
        /opt/local/bin/tor --quiet --runasdaemon 1
    fi
}

# Tracks (a Rails GTD app). This copy is ancient and bitrotten.
# port 3000
run_tracks_idempotently() {
    if ( ! pq ~/tracks/script/server); then
        ~/tracks/script/server -e production & >/dev/null 2>&1
    fi
}
