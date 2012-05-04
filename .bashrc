umask 0022

# The order of these ##########################-demarcated sections matters.

########################## PATH, INFOPATH, MANPATH (no LD_LIBRARY_PATH ATM)
# For this to work right, PATH must not include $PATH. Otherwise subshells
# will get the previous $PATH reinterpolated n times (for subshells n deep).
# The effect of such an ever-growing PATH depends on the position of PATH
# within $PATH, so it can easily cause errors.

ELISP_INFOPATH=~/.elisp/tramp/install/share/info

MACPORTS_INFOPATH=/opt/local/share/info
MACPORTS_MANPATH=/opt/local/share/man
MACPORTS_PATH=/opt/local/bin:/opt/local/sbin

MYSQL_MANPATH=/usr/local/mysql/man
MYSQL_PATH=/usr/local/mysql/bin # the MySQL AB release, not the MacPorts version

# This is for perlbrew itself, not for perlbrew-installed Perl installations.
PERLBREW_MANPATH=~/perl5/man
PERLBREW_PATH=~/perl5/perlbrew/bin:~/perl5/bin

PERSONAL_PATH=~/bin
PERSONAL_PERL5LIB=~/lib

POSTGRESQL_MANPATH=/usr/local/pgsql/share/man
POSTGRESQL_PATH=/usr/local/pgsql/bin

## Commented out for now, since it slows down shell init a lot.
# VIRTUALENVWRAPPER_PYTHON=/opt/local/bin/python2.7
# source /opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/virtualenvwrapper.sh

SYSTEM_INFOPATH=/usr/share/info:/usr/lib/info
SYSTEM_MANPATH=/usr/share/man:/usr/X11/share/man
SYSTEM_PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11/bin:/usr/local/bin:/etc/init.d

# FIXME: implement this in code. # The order of precedence, for INFOPATH, MANPATH, and PATH alike, is as follows (LD_LIBRARY_PATH is fine as it is, so don't mess with it)
# # PERLBREW POSTGRESQL MYSQL MACPORTS SYSTEM ELISP PERSONAL
# for CATEGORY in ELISP PERLBREW POSTGRESQL MYSQL MACPORTS SYSTEM PERSONAL; do
#     for VARTYPE in INFOPATH MANPATH PERL5LIB PATH; do
#         VARNAME="${CATEGORY}_${VARTYPE}"
#         VARVAL=`eval '$VARNAME'
#         eval "export VARNAME=$VARVAL"
#     done
# done

set -a

INFOPATH=$MACPORTS_INFOPATH:$SYSTEM_INFOPATH:$ELISP_INFOPATH

MANPATH=$PERLBREW_MANPATH:$POSTGRESQL_MANPATH:$MYSQL_MANPATH:$MACPORTS_MANPATH:$SYSTEM_MANPATH

###################################
########### *** NEED TO CORRECT THIS ***
########### quinn@tao:~$ which perl
########### /opt/local/bin/perl
###################################
PATH=$PERLBREW_PATH:$PYTHON_PATH:$POSTGRESQL_PATH:$MYSQL_PATH:$MACPORTS_PATH:$SYSTEM_PATH:$PERSONAL_PATH

# Python virtualenvwrapper (like Perl's Pinto). This must be initialized *after*
# PATH is set; otherwise it tries to use the wrong Python.
WORKON_HOME=~/.python-virtualenv

set +a

# Display all processes, in a style that I like.
# If an argument is given, display only those that match that regex.
p() {
    A_COMMAND="ps auxww | grep -v grep"
    if [[ $1 ]]; then
	A_COMMAND="$A_COMMAND | grep $1"
    fi

    eval "$A_COMMAND"
}

# FIXME: stop using grep as a golden hammer; instead, use awk to separate fields in the output of ps(1).

# Display my processes, except the ones attached to this tty.
# If an argument is given, display only those that match that regex.
pu() {
    OS=`uname -s`

    if   [[ $OS == 'Linux' ]]; then 
        THIS_TTY=`tty | sed -e 's/\/dev\///'`
    elif [[ $OS == 'Darwin' ]]; then
        THIS_TTY=`tty | sed -e 's/\/dev\/tty//'` #FIXME: wrong regex!
    fi
    
    COMMAND="p $USER | grep -v $THIS_TTY"

    if [[ $1 ]]; then
	COMMAND="$COMMAND | grep $1"
    fi

    eval "$COMMAND"
}

# grep the list of processes, but don't show any output.
# This is useful for non-interactive functions that just need
# to test whether a process is running.
pq() {
    p auxww | grep -q $1 | grep -qv grep
}

vrep() {
    grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

########################## git convenience functions by Shawn O. Pearce
source ~/git-completion.bash

########################## Project-specific settings

# (None for now)


########################## Emacs (and bash, and less)

MY_PROMPT="\u@\h:\w\$ "

set -a

if [[ $RUNNING_UNDER_EMACS ]]; then # set by ~/.emacs.d/init_bash.sh
    # ANSI colors don't work here. Use a non-color shell prompt.
    PS1=${MY_PROMPT}

    # Pagers don't work here. Use cat(1) instead of less(1).
    PAGER=cat
else
    # Color the shell prompt slate blue (to distinguish commands from output)
    PS1="\[\e[34;m\]${MY_PROMPT}\[\e[0m\]"

    LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --no-init'
    PAGER="less $LESS"
    GIT_PAGER=$PAGER
fi

EDITOR=emacsclient
VISUAL=$EDITOR

set +a

########################## Perl

# Make Module::Install auto-follow dependencies.
export PERL_MM_USE_DEFAULT=1

########################## PostgreSQL

set -a
PGDATA=/usr/local/pgsql/data
PGUSER=postgres
set +a

########################## FTP

set -a
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=' '
set +a

########################## Ruby

export RUBYOPT=rubygems

###################### cheatsheet for David Wheeler's PGX::Build
build_test() {
    # Run all tests or, if so directed, a specific test file.
    # (You must use this function; you can't run a single test file directly
    # using 'perl test.t', because it won't get the correct settings.)
    TEST_FILE=$1
    if [[ $TEST_FILE ]]; then
        ./Build test --test_files $TEST_FILE
    else
        ./Build test
    fi
}

build_database_anew() {
    # Rebuild the database.
    ./Build db --drop_db 1
}

########################## Aliases and shell functions - lots of them.

a() {
    sudo apache2ctl graceful
}

aa() {
    sudo apache2ctl "$@"
}

ch() {
    SUBJECT=$1
    cat ~/Cheatsheets/${SUBJECT}-cheatsheet
}

cl() {
    emacs -nw -q -batch -f batch-byte-compile "$@"
}

cr() { # cd to a symlink's referent, not the symlink itself.  That way your prompt and pwd give the full path (and autocompletion will work properly in emacs shell-mode).
    SYMLINK=$1
    eval `cr_helper $SYMLINK`
}

cs() {
    TOPIC=$1
    less ~/Cheatsheets/${TOPIC}-cheatsheet
}

###################### specific to apt-based systems (e.g. Ubuntu, Debian)
d() {
    apt-cache search ".*$1.*"
}

di() {
    dpkg -l "*$1*" | grep ^ii
}
###################### end commands for apt-based systems (e.g. Ubuntu, Debian)

dido() {
    $DIDO_RUN_FROM_SOURCE_DIR/bin/dido.pl \
        $DIDO_RUN_FROM_SOURCE_DIR/var/lib/dido/Museum.xml
}

alias diff='diff -u'

dos2unix() {
    perl -pi -e ' s{ \r\n | \n | \r }{ \n }gx ' "$@"
}

dream() {
    /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background &
}

e() {
    find . -name "$@" | xargs emacsclient
}

ec() {
    emacsclient "$@"
}

ecf()
{
    echo `find . -name $1`
    ec `find . -name $1`
}

ecw()
{
    ec `which $1`
}

fd() {
    find . -type d -name "*${1}*"
}

ffd() {
    grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

ffind() {
    find "$@" | grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

fixmes() {
    ggrep -ri fixme . | grep -v dojo-release
}

fix_spaces()
{
    # Fix OS X Leopard's Spaces function.
    # Make it so that, for a currently open app, clicking on that app
    # in the Dock starts a new window (rather than transporting you
    # back to the virtual desktop where the original window lives).
    # For further explanation, see
    # http://www.37signals.com/svn/posts/859-making-life-easier-with-spaces-on-leopard
    defaults write com.apple.Dock workspaces-auto-swoosh -bool NO
    killall Dock
}

fn()
{
    grep -A 5 "$1()" ~/.bashrc
}

fr() {
    ssh qweaver@glacier.frostconsultingllc.com
}

alias func=fn

gemi()
{
    sudo gem install --remote \
                     --test \
                     --rdoc \
                     --ri \
\
                     "$@"
}

ggrep() {
    grep "$@" \
        | grep -v '\.git' \
        | grep -v _MTN \
        | grep -v '\.svn' \
        | grep -v CVS \
        | grep -v '\.DS_Store' \
        | grep -v _build \
        | grep -v blib
}

gc() {
    MESSAGE="$@"

    git add . \
        || exit $?

    if [[ $MESSAGE ]]; then
        git commit -a -m "$MESSAGE"
    else
        git commit -a
    fi
}

gi() {
    git init && git add . && gc "Initial commit"
}

git-tree() {
    git log --graph --decorate --pretty=oneline --abbrev-commit
}

alias gpg=gpg2

crep() {
    ggrep -c $@ | grep -v ':0'
}

# run the profile manager to create a new profile
firefox() {
    FIREFOX_EXECUTABLE="/Applications/Firefox\\ 3.app/Contents/MacOS/firefox-bin"
    
    if [[ $1 ]]; then
        # Run with a specific profile, named in $1.
        sh -exec "$FIREFOX_EXECUTABLE -ProfileManager $1"
    else
        # Run the Profile Manager (allows you to view all profiles or add a new one)
        sh -exec "$FIREFOX_EXECUTABLE -p"
    fi
}

# gsm:  play a series of .gsm files.  Works on Linux only.
gsm() {
    tcat "$@" > /dev/audio
}

hide_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles FALSE
    killall Finder 
}

hop() {
    cd "$_"
}

alias js='socat READLINE TCP4:localhost:4242' # MozRepl console:  http://wiki.github.com/bard/mozrepl/

kindle() { # Install a Project Gutenberg .mobi for use on my jailbroken iPhone.
    ADIR='root@te:/var/mobile/Applications/A063DC1A-20BF-4A66-9858-288FF88DB3ED/Documents/eBooks'
    scp "$@" $MOBI_PATH $ADIR/
}

alias ls='ls -1F' # Must be written as an alias; a shell function would recurse.

l() {
    ls | less
}

lwhich() {
    perldoc -l "$1"
}
lw() {
    lwhich "$@"
}
eclw() {
    ec $(lw "$@")
}

lo() {
    deploy_fastagi
}

alias m=mtn

alias mi='mtn automate inventory'

alias mtns="mtn9 sync --db ~/.monotone_dbs/com.getsnapdragon.mtn 66.93.182.98 '*'"

#FIXME:  make an intelligent factory that detects whether you are running with
# GNU grep or with BSD grep, and creates an mg that does the right thing.
mg() {
    # mg ("Mail Grep"):  grep for something in all my monthly mboxes, in order
    # from most recent to oldest.
    #
    # Depends on GNU sort.  BSD ls has a general numeric sort option, so this
    # could be made to detect the OS and work on *BSD*... if I cared.
    ls ~/Mail/mbox* | sort --general-numeric-sort | xargs grep -i "$@"
}

mg_bsd() {
    ls -gr ~/Mail/mbox* | xargs grep -i "$@"
}

mp() {
    # mp ("My Processes"):  grep among all processes owned by my user ID.
    ps axww -u $USER | grep $1 | grep -v grep
}

mysqlstart() {
    mysql.server
}

mysqlstop() {
    mysqladmin5 -u root -p shutdown
}

newpassword() {
    dd if=/dev/urandom bs=$1 count=1 | base64
}

newpassword_alphanumeric() {
    N_CHARS=$1
    if [ ! $N_CHARS ]; then
	N_CHARS=10
    fi

    apg -n 1 -M Ncl -m $N_CHARS -x $N_CHARS -s
}

newpassword_wep() {
    apg -n 1 -a 1 -M nc -m 26 -x 26 -E GHIJKLMNOPQRSTUVWXYZ
}

newpassword_mac() {
    apg -n 1 -a 1 -M nc -m 12 -x 12 -E GHIJKLMNOPQRSTUVWXYZ | xargs /home/quinn/bin/colonize
}

newpin() {
    apg -n 1 -a 1 -M nc -m 4 -x 4 -E ABCDEFGHIJKLMNOPQRSTUVWXYZ
}

pa() {
    psql -U admin postgres
}

alias pb=perlbrew

ppp() {
    psql -U postgres postgres
}

pws() {
    plackup -MPlack::App::File -e 'Plack::App::File->new->to_app'
}

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
        (_decryp $WALLET && echo "$@") | gpg $GPGOPTS --encrypt > $TMPFILE \
            && mv $TMPFILE $WALLET
    fi
}

perms() {
    find $1 | xargs ls -ld
}

pf() {
    REPORT_TYPE=$1

    cd /usr/local/pgsql/pgfouine-1.1
    sudo ./pgfouine.php -top 10 -logtype csvlog -file ~/Desktop/Zublogs/24.csv -report $REPORT_TYPE > ~/Desktop/Reports/$REPORT_TYPE.html
}

###################### OS X cheat aliases
if [[ $(uname) == Darwin ]]; then
    alias ldd='otool -L'
fi

#pperl_back_up_locally_installed_modules() {
#    # Use CPAN's autobundle feature
#}

unrarrr() {
    for f in *rar; do unrar x -o+ "$f" .; done
}

pull_from_lao()
{
    rsync -va \
        "$@" \
        --exclude '*cache*' \
        --exclude '*Cache*' \
        --exclude '.bash*' \
        --exclude '.emacs*' \
        --exclude '*RecentDocuments*' \
        --exclude '*kcookiejar*' \
        --exclude '*IconPositions' \
        --exclude '.mozilla*' \
        --exclude '.liferea_1.4*' \
        --exclude '.openoffice.org2' \
        quinn@192.168.0.99:~/ \
        ~/
}

push_to_demo()
{
    rsync -va \
        "$@" \
        --exclude '.kde/socket*' \
        --exclude '.kde/share/apps*' \
        --exclude '.kde/share/config/session*' \
        --exclude '*cache*' \
        --exclude '*Cache*' \
        --exclude '*RecentDocuments*' \
        --exclude '*kcookiejar*' \
        --exclude '*IconPositions' \
        .bash_profile \
        .bashrc \
        .elisp \
        .emacs \
        .emacs.d/init_bash.sh \
        .kde \
        .xbindkeysrc \
        demo@192.168.0.2:~/
}

s() {
    ssh pgx-test
}

te() {
    if [ $1 ]; then
        N_LINES=$1
    else
        N_LINES=1
    fi

    /usr/bin/sudo /usr/bin/tail -$N_LINES /var/log/apache/error.log
}

tor_wget() {
    HTTP_PROXY=http://127.0.0.1:8118/ http_proxy=$HTTP_PROXY \
        wget --mirror --convert-links --html-extension --domains $1 $2
}

tor_on() {
    export HTTP_PROXY=http://127.0.0.1:8118/
    export http_proxy=$HTTP_PROXY    
}

tor_off() {
    unset HTTP_PROXY http_proxy    
}

# I can never remember the weird argument order for ssh tunneling, hence
# this "executable cheatsheet."
# Usage:
# tunnel $REMOTE_HOST $REMOTE_PORT $LOCAL_PORT
tunnel() {
    ssh -fNR $2:localhost:$3 $1
}

# Translate newlines to the DOS convention (CRLF).
2dos() {
    /usr/bin/perl -pi -e ' s{$}{\r}x; ' "$@"
}

# Reload custom X11 keybindings.
xb() {
    killall --user $USER xbindkeys
    xbindkeys
}

xx()
{
    find /etc/rc*.d -name "*$1*" | xargs sudo ~/bin/x
}

set +x

########################## Run daemons, if installed but not already runnning.
source ~/.bashrc-daemons && run_gpg_agent_idempotently
source ~/.bashrc-python
source ~/.bashrc-perlbrew


if [[ ! $BASHRC_ALREADY_RAN ]]; then
    export BASHRC_ALREADY_RAN=1
fi
