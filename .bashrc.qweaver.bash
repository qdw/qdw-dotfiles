# Make it so Ctrl-S and Ctrl-Q aren't interpreted as software flow control
# characters, messing up your terminal (see http://catern.com/posts/terminal_quirks.html).
# This also frees up Ctrl-Q for use as the tmux prefix key.
if (tty > /dev/null 2>&1); then # (if-clause makes sure this doesn't run when I do 'ssh MY_HOST MY_COMMAND')
    stty -ixon  > /dev/null 2>&1
    stty -ixoff > /dev/null 2>&1
fi


alias e='emacs -nw'
alias grep='egrep --color=auto'
alias ls='ls -1F --group-directories-first --color=no'
unset -f ll; function ll() { ls -ltFA --color=no $@ | less --quit-if-one-screen ;}

function dcc() { colordiff -u "$1" "$2" | less -R ;}
function dfr() { df -h | grep ^/ ;}
function dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}
function mr() { mount | grep ^/ ;}
function ips() { /sbin/ifconfig -a | grep inet | grep -v inet6 ;}
function ipv6() { /sbin/ifconfig -a | grep inet6 ;}
function p() { ps auxwww | head -1; ps auxwww | grep -v grep | grep $@ ;}
function timestamp() { date +%Y%m%dT%H%M%S%z ;}
function ts() { timestamp $@ ;}

function sup() {
    if (hostname -s | grep 'b$' > /dev/null 2>&1); then
        su -
    else
        sudo su	- postgres
    fi
}

# ta $SESSION_NAME ("Tmux Attach"): attach to named session, creating if needed.
function ta() {
    if (! tmux has-session -t $1 2> /dev/null); then
        tmux -f ~/.tmux.qweaver.conf new-session -s $1 -n $1
    else
        tmux -f ~/.tmux.qweaver.conf -2 attach-session -t $1
    fi
}

# tn ("Tmux New"): create a new tmux session, because ta breaks on old tmux versions.
function tn() {
    tmux -f ~/.tmux.qweaver.conf new-session -n $1 -s $1
}

# tl ("Tmux List"): list running tmux sessions.
function tl() {
    tmux list-sessions 2> /dev/null
}

function flast() {
    head -1 $1 | awk '{ print $1, $2; }'
    tail -1 $1 | awk '{ print $1, $2; }'
}

function replication_role()
{
    if ! which psql; then
        echo ''
    else
        INR=$(psql -tc 'select pg_is_in_recovery()')
        if [[ $INR == ' f' ]]; then
            echo '*** MASTER ***'
        elif [[ $INR == ' t' ]]; then
            echo 'replica'
        else
            echo '[ERROR: COULD NOT DETERMINE REPLICATION ROLE]'
        fi
    fi
}

if hostname -s | $HNS | grep '^pgdb' > /dev/null 2>&1; then
    REPLICATION_ROLE=$(replication_role)
    export PS1="\\[\\e[34;1m\\]\u@\\h:\\w  $REPLICATION_ROLE \\$ \[\e[0m\\]"
    mkdir -p ~qweaver/postgres && cd $_
else
    export PS1='\[\e[34;1m\]\u@\h:\w \\$\[\e[0m\] '
fi


set -a

EDITOR='emacs -nw'
MYVIMRC=~/.vimrc.qweaver
PAGER=less
PGPAGER="$PAGER"
PATH=$PATH:~/qweaver/bin
TERM=xterm-256color
VISUAL="$EDITOR"

set +a

# hnmatch() {
#     SUBSTRING="$1"
#     hostname -f | fgrep "$SUBSTRING"
#     return $?
# }

# FQDN=$(hostname -f)
# if [[ echo $FQDN | fgrep .iad. > /dev/null 2>&1 ]]; then
#     export http_proxy=
    

# yum install -y ack colordiff emacs-nox htop nc nmap pgcenter pv tmux screen pgcli iperf python-psycopg2 yum-plugin-downloadonly pstree reptyr

vc() { code --goto $1:1 ;}
