#FIXME: use the github release, not this copypasta.

export GPGUSER=Nobody
export GPGOPTS="--batch --quiet --no-tty --armor --user $GPGUSER"
export WALLET=~/wallet.asc

# _dginit() { # Helper function:  create the key and file, iff they don't exist.
#     if [ ! -e $WALLET ]; then
#         touch $WALLET
#     fi

#     gpg --list-keys Nobody
#     if [ $? != 0 ]; then
#         gpg --gen-key
# }

_decryp() { # Helper function:  decrypt the encrypted passwords file.
    gpg $GPGOPTS --decrypt $WALLET
}

pass() { # Grep lines from my encrypted passwords file.
    if [ $# != 1 ]; then
        echo 'Usage:  pass [regex_to_grep_for]'
    else
        _decryp | grep -i "$1"
    fi        
}

padd() { # Add a line to my encrypted passwords file.
    if [ $# == 0 ]; then
        echo 'Usage:  padd some text to add to the file...'
    else
        cp $WALLET $WALLET.bak

        TMPFILE=$WALLET.tmp
        (_decryp $WALLET && echo $@) | gpg $GPGOPTS --encrypt > $TMPFILE \
            && mv $TMPFILE $WALLET
    fi
}
