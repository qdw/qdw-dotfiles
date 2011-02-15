umask 0022

    set -a
CATALYST_DEBUG=0
EDITOR=emacsclient
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=' '
INFOPATH=~/.elisp/tramp/install/share/info:$INFOPATH
MANPATH=/usr/local/share/man:/opt/local/share/man:$MANPATH
PAGER=less
PATH=/usr/local/pgsql/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/sbin:/usr/sbin:/etc/init.d:/usr/local/mysql/bin:~/bin
PERL_MM_USE_DEFAULT=1 # make Module::Install auto-follow dependencies
PERL5LIB=~/Hack/pg-version-compare/lib:~/share/lexy3/lib:~/share/lib:~/share/lexy2perl/lib
#PGDATA=/usr/local/pgsql-9.0alpha5-build1
PS1="\u@\h:\w\\$ "
RUBYOPT=rubygems
SHLVL=1
#TERM=${TERM:-cons25}
#TERM=vt102
VISUAL=$EDITOR
    set +a

if [[ -e ~/.mac_os_x_login ]]; then
    source ~/.mac_os_x_login
fi

a() {
    /usr/bin/sudo /usr/sbin/apache2ctl "$@"
}

alias aa='sudo apache2ctl graceful'

###################### cheatsheet for David Wheeler's PGX::Build
bt() {
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

bd() {
    # Rebuild the database.
    ./Build db --drop_db 1
}
###################### end of cheatsheet for David Wheeler's PGX::Build

cl() {
    emacs -nw -q -batch -f batch-byte-compile "$@"
}

d() {
    apt-cache search ".*$1.*"
}

di() {
    dpkg -l "*$1*" | grep ^ii
}

dido() {
    $DIDO_RUN_FROM_SOURCE_DIR/bin/dido.pl \
        $DIDO_RUN_FROM_SOURCE_DIR/var/lib/dido/Museum.xml
}

dream() {
    /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background &
}

alias e=emacsclient_or_emacs

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

ffind()
{
    find "$@" | grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

fix_spaces()
{
    # Leopard:  Spaces:
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
    grep $@ \
        | grep -v '\.git' \
        | grep -v _MTN \
        | grep -v '\.svn' \
        | grep -v CVS \
        | grep -v '\.DS_Store' \
        | grep -v _build \
        | grep -v blib
}

gc() {
    MESSAGE=$0
    if [[ $MESSAGE ]]; then
        git commit -a -m $MESSAGE
    else
        git commit -a
    fi   
}

alias gpg=gpg2

crep()
{
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
gsm()
{
    tcat "$@" > /dev/audio
}

hide_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles FALSE
    killall Finder 
}

ic() { # Install Cliphp.
    ~/Hack/Cliphp/NUKE_DB_AND_INSTALL.sh
}

kindle() { # Install a Project Gutenberg .mobi for use on my jailbroken iPhone.
    ADIR='root@te:/var/mobile/Applications/A063DC1A-20BF-4A66-9858-288FF88DB3ED/Documents/eBooks'
    scp "$@" $MOBI_PATH $ADIR/
}

l() {
    lexy_fastagi.pl 4573
}

alias ls='ls -F'

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
    /opt/local/share/mysql5/mysql/mysql.server
}
mysqlstop() {
    /opt/local/bin/mysqladmin5 -u root -p shutdown
}

newpassword()
{
    N_CHARS=$1
    if [ ! $N_CHARS ]; then
	N_CHARS=8
    fi

    apg -n 1 -a 1 -m $N_CHARS -x $N_CHARS -s
}

newpassword_alphanumeric()
{
    N_CHARS=$1
    if [ ! $N_CHARS ]; then
	N_CHARS=8
    fi

    apg -n 1 -M Ncl -m $N_CHARS -x $N_CHARS -s
}

newpassword_wep()
{
    apg -n 1 -a 1 -M nc -m 26 -x 26 -E GHIJKLMNOPQRSTUVWXYZ
}

newpassword_mac()
{
    apg -n 1 -a 1 -M nc -m 12 -x 12 -E GHIJKLMNOPQRSTUVWXYZ | xargs /home/quinn/bin/colonize
}

newpin()
{
    apg -n 1 -a 1 -M nc -m 4 -x 4 -E ABCDEFGHIJKLMNOPQRSTUVWXYZ
}

p() {
    ps auxww | grep $1 | grep -v grep
}

ppp()
{
    /usr/bin/psql -U postgres postgres
}

pa() {
    /usr/bin/psql -U admin    postgres
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

###################### Mac OS X cheat aliases
if [[ $(uname) == Darwin ]]; then
    alias ldd='otool -L'
fi

###################### end Mac OS X cheat aliases

###################### port cheatsheet (MacPorts)

port_update_repository() {
    sudo port -d sync
}

port_upgrade_all() { # Upgrade all (installed) ports to the latest version.
    sudo port selfupdate
    sudo port -d sync
    sudo port upgrade outdated
}

port_installed() { # List installed ports that match substring $1
    if [[ $1 ]]; then
        PORT_NAME_SUBSTRING=$1
        sudo port list installed | grep $PORT_NAME_SUBSTRING
    else
        sudo port list installed
    fi
}

port_contents() { # List the files installed by a port.
    sudo port contents "$@"
}

port_dependents() { # List ports that depend on the port named $1
    PORT_NAME=$1
    sudo port dependents $PORT_NAME
}

port_dependecies() { # List the ports the port named $1 depends on
    PORT_NAME=$1
    sudo port deps $PORT_NAME
}

ports() {
    PATTERN=$1
    sudo port list "*$PATTERN*"
}

###################### end port cheatsheet (MacPorts)


#pperl_back_up_locally_installed_modules() {
#    # Use CPAN's autobundle feature
#}

unrarrr() {
    for f in *rar; do unrar x -o+ "$f" .; done
}

alias s=svn

show_hidden_files()
{
    defaults write com.apple.finder AppleShowAllFiles TRUE
    killall Finder 
}

show_paths_in_finder() {
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
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

start()
{
    sudo /usr/sbin/apachectl -f \
        /Users/quinn/Desktop/fun-with-css/httpd/httpd.conf
}

stop()
{
    sudo kill $(cat /Users/quinn/Desktop/fun-with-css/httpd/pid)
}

ta()
{
    tail "$@" /usr/local/var/log/asterisk/messages
}

td()
{
    /usr/bin/tail "$@" $DIDO_RUN_FROM_SOURCE_DIR/var/log/dido/messages
}

te()
{
    if [ $1 ]; then
        N_LINES=$1
    else
        N_LINES=1
    fi

    /usr/bin/sudo /usr/bin/tail -$N_LINES /var/log/apache/error.log
}

tget()
{
    ton
    wget --mirror --convert-links --html-extension --domains $1 $2
}

tl() {
    /usr/bin/sudo /usr/bin/tail -1 /var/log/apache/access.log
}

ton() {
    export HTTP_PROXY=http://127.0.0.1:8118/
    export http_proxy=$HTTP_PROXY    
}

toff() {
    unset HTTP_PROXY http_proxy    
}

tunnel() {
    ssh -R127.0.0.1:4575:127.0.0.1:4573 alexander.lexy.com
}

unix2dos() {
    /usr/bin/perl -pi -e ' s{$}{\r}x; ' "$@"
}

mac_safari_unsmooth_fonts_smaller_than() {
    POINT=$1
    sudo defaults write com.apple.safari AppleAntiAliasingThreshold $POINT
}

mac_globally_disable_antialiasing() {
    sudo defaults write CoreGraphics CGFontDisableAntialiasing YES
}

xb()
{
    killall --user $USER xbindkeys
    xbindkeys
}

xx()
{
    find /etc/rc*.d -name "*$1*" | xargs sudo ~/bin/x
}
