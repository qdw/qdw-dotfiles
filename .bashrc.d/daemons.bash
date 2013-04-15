# Run emacs as a daemon, so it's there and ready for emacsclient to reach.
# If the daemon is killed for some reason, emacsclient will just run it again,
# since I have the ALTERNATE_EDITOR environment variable set to ''.
# But it's nice to get the overhead of running emacs out of the way early.
# For further reading, see http://www.emacswiki.org/emacs/EmacsAsDaemon
run_emacs_daemon_idempotently() {
    if (! ps auxwww | grep $USER | grep 'emacs --daemon' >/dev/null 2>&1); then
        emacs --daemon &
    fi
}

# rubygems documentation server
# port 8080
run_gem_server_idempotently() {
    if (! ps auxwww | grep $USER | 'gem server' >/dev/null 2>&1); then
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

# ssh-agent(1).
SSH_ADD=~/.homebrew/bin/ssh-add
SSH_AGENT=~/.homebrew/bin/ssh-agent
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
     $SSH_AGENT | sed 's/^echo/#echo/' > "${SSH_ENV}"
     chmod 600 "${SSH_ENV}"
     . "${SSH_ENV}" > /dev/null
     $SSH_ADD;
}

run_ssh_agent_idempotently() {
    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
            start_agent;
        }
    else
        start_agent;
    fi
}

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
