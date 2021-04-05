replication_role()
{
    INR=$(psql -tc 'select pg_is_in_recovery()')

    if [[ $INR == ' f' ]]; then
        echo '*** MASTER ***'
    elif [[ $INR == ' t' ]]; then
        echo 'replica'
    else
        echo '[ERROR: COULD NOT DETERMINE REPLICATION ROLE]'
    fi
}

set -a

EDITOR=emacs
REPLICATION_ROLE=$(replication_role)
PS1="\u@\h:\w  $REPLICATION_ROLE # "
MYVIMRC=~/.vimrc.qweaver
VISUAL="$EDITOR"

set +a


alias grep='grep --color=auto'
alias ls='ls -1FA --group-directories-first --color=auto'
dfr() { df -h | grep ^/ ;}
mr() { mount | grep ^/ ;}

cd ~qweaver/postgres
