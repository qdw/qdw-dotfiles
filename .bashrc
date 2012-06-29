umask 0022

##########################
########################## <- Sections demarcated in hashes, like this,
########################## must appear in order in order for this code
########################## to work.
##########################

########################## 
########################## bash shell completion for git,  by Shawn O. Pearce.
########################## Must be loaded before I set PS1, as I use it there.

source ~/git-completion.bash

##########################
########################## *PATH variables for the custom software I use, in
########################## the order I want (I ignore and override the default
########################## paths provided by the system bash init files,
########################## because I do not like them).
########################## 
########################## You might ask why I define these in ~/.bashrc,
########################## such that they execute every time I start a shell,
########################## rather than defining them in ~/.bash_profile,
########################## such that they would execute only once, at
########################## login time. The answer is that I like to ensure
########################## that my shells always have an up-to-date
########################## environment. I make small changes to ~/.bashrc
########################## often, and I want them to be reflected as soon as
########################## I start a new shell (whether or not the software
########################## launching that shell ran it with --login).
##########################
########################## I even run certain daemons via my ~/.bashrc, iff
########################## they're not already running. This allows me to
########################## restart them if they die somehow.
##########################

DO_IT_THEORYS_WAY=1

##########
# Perlbrew (itself, as opposed to perlbrew-installed Perl installations).
##########

# Note: use of Perlbrew may be disabled below; search for DO_IT_THEORYS_WAY
PERLBREW_ROOT=~/perl5/perlbrew # the default, but I like to state it explicily.
PERLBREW_PATH=$PERLBREW_ROOT/bin

############
# PostgreSQL
############
if [[ $DO_IT_THEORYS_WAY ]]; then
    # Use theory's custom PostgreSQL build (built using
    # ('cd ~quinn/src/3/my-cap && cap my:build:postgres').
    POSTGRESQL_MANPATH=/usr/local/pgsql/share/man:/usr/local/share/man
    POSTGRESQL_PATH=/usr/local/pgsql/bin

    export PGDATA=/usr/local/pgsql/data
fi

#######################
# (Homebrewed) Python 3
#######################
# See https://github.com/mxcl/homebrew/wiki/Homebrew-and-Python
PYTHON3_PATH=~/.homebrew/share/python3 # install-scripts dir
PYTHON3_PATH=$PYTHON3_PATH:/usr/local/share/python # location of easy_install

#######
# MySQL
#######
MYSQL_MANPATH=/usr/local/mysql/man
MYSQL_PATH=/usr/local/mysql/bin # from the MySQL AB/Oracle tarball release

##########
# Homebrew
##########
# HOMEBREW_ROOT=/usr/local # the default, but I like to state it explicitly
HOMEBREW_ROOT=~/.homebrew # I keep my own private homebrew instance
HOMEBREW_PATH=$HOMEBREW_ROOT/bin:$HOMEBREW_ROOT/sbin
HOMEBREW_MANPATH=$HOMEBREW_ROOT/share/man
HOMEBREW_DYLD_LIBRARY_PATH=$HOMEBREW_ROOT/lib

#############
# System path (OS-specific)
#############
OS=$(uname -s)
if [[ $OS = 'Darwin' ]]; then
   SYSTEM_INFOPATH=/usr/share/info:/usr/lib/info
   SYSTEM_MANPATH=/usr/share/man:/usr/X11/share/man
   SYSTEM_PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11/bin:/usr/local/bin:/etc/init.d
else
    echo "Don't know what system path to use for $OS"
    exit 43
fi

############
# Emacs LISP (third-party) packages
############
ELISP_INFOPATH=~/.elisp/tramp/install/share/info

#####################
# My own utility code (~/bin et cetera)
#####################
PERSONAL_PATH=~/bin
PERSONAL_PERL5LIB=~/perl5lib

# OK, now assemble PATH, MANPATH et cetera from the above-specified paths.
for VAR in PATH MANPATH INFOPATH LD_LIBRARY_PATH DYLD_LIBRARY_PATH PERL5LIB; do
    # 1. Clear the PATH, in order to get rid of OS-imposed cruft.
    eval "$VAR=''"

    # 2. Append any package-specific paths (perlbrew, postgresql, et cetera)
    # in the desired order.
    for CATEGORY in PERLBREW POSTGRESQL PYTHON3 MYSQL HOMEBREW SYSTEM ELISP PERSONAL; do
        VALUE_BEFORE_APPENDING=$(eval echo \$$VAR)
        VALUE_TO_APPEND=$(eval echo \$${CATEGORY}_${VAR}) # e.g. $(eval echo \$PERLBREW_MANPATH) yields ~/perl5/man
        if [[ $VALUE_TO_APPEND ]]; then
            if [[ ! $VALUE_BEFORE_APPENDING ]]; then
                eval "$VAR=$VALUE_TO_APPEND"
            else
                eval "$VAR=$VALUE_BEFORE_APPENDING:$VALUE_TO_APPEND"
            fi
        fi
    done
done

##########
# Perlbrew
##########
if [[ $DO_IT_THEORYS_WAY ]]; then
    # Use theory's Perl, which resides in /usr/local/bin, which is already in
    # my path thanks to HOMEBREW_PATH. I built theory's Perl using
    # 'cd ~quinn/src/3/my-cap && cap my:build:perl'.
    PATH=/usr/local/bin:$PATH
    MANPATH=/usr/local/man:$MANPATH
else
    # Use Perlbrew (specifically, use whatever version of Perl I set as the
    # default using 'perlbrew switch').
    source $PERLBREW_ROOT/etc/bashrc
fi

########################## 
########################## App-specific environment variable settings
########################## 

set -a # export all variables that I define from now until I say 'set +a'

###################
# bash, emacs, less
###################
if [[ $RUNNING_UNDER_EMACS ]]; then # this var is set by ~/.emacs.d/init_bash.sh
    # Emacs shell-mode is a dumb terminal, so don't use advanced features:

    # ANSI colors don't work. Use a non-color shell prompt.
    PS1='$(__git_ps1 "[%s] ")\u@\h:\w\$ '

    # Pagers don't work. Use cat(1) instead of less(1).
    PAGER=cat; GIT_PAGER=$PAGER

    # emacsclient *does* work, so use that.
    EDITOR=emacsclient; VISUAL=$EDITOR
else
    # Other terminals can do fancier stuff:

    # Color the shell prompt slate blue (to distinguish commands from output).
    # If I'm on a git branch, preend the branch name in purple.
    PS1='\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;m\]\u@\h:\w\$\[\e[0m\] '

    # Use my favorite pager and pager options.
    LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --no-init'
    PAGER=less; GIT_PAGER=$PAGER

    # Use vi for quick edits inside this terminal window.
    EDITOR=vi; VISUAL=$EDITOR
fi

######
# bash completion
######
# Homebrew
#if [ -f `brew --prefix`/etc/bash_completion ]; then
#    source `brew --prefix`/etc/bash_completion
#fi

#####
# FTP
#####
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=''

######
# Perl
######

# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
PERL_MM_USE_DEFAULT=1

############
# PostgreSQL
############
# If I forget to specify a DB, use one that's safe to trash.
PGDATABASE=sandbox

########
# Python virtualenv (as installed under the OS X preinstaled Python)
########
VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
source /usr/local/bin/virtualenvwrapper.sh

# mkve ("make virtualenv"): create a new Python virtualenv, using
# the OS X preinstalled Python.
alias mkve='mkvirtualenv --no-site-packages'

# mkhbve ("Make Homebrew virtualenv"): create a new Python virtualenv, using
# Homebrew's Python 2.7. Commented out for now, since that formula breaks.
alias mkhbve='mkvirtualenv --no-site-packages --python=/usr/local/Cellar/python/2.7.1/bin/python'

# mkve3 ("make virtualenv for Python 3"): create a new Python virtualenv, using
# Homebrew's Python 3.
alias mkve3='mkvirtualenv --no-site-packages --python=/usr/local/Cellar/python3/3.2/bin/python3'

######
# Ruby
######
RUBYOPT=rubygems

set +a

########################## 
########################## Shell functions and aliases, for convenience
##########################

# FIXME: stop using grep as a golden hammer; instead, use awk to separate fields in the output of ps(1).

###########
# Processes
###########

# Display all processes, in a style that I like.
# If an argument is given, display only those that match that regex.
p() {
    A_COMMAND="ps auxww | grep -v grep"
    if [[ $1 ]]; then
	A_COMMAND="$A_COMMAND | grep $1"
    fi

    eval "$A_COMMAND"
}

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

# grep, excluding (D)VCS metadata and other metadata. # FIXME: implement in one grep command, not a pipeline, so that I don't have to wait for the whole thing to complete before I start seeing output.
vrep() {
    grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

#########################
# Miscellaneous functions (ordered alphabetically, or meant to be)
#########################

# aa graceful|start|...: because typing 'sudo apache2ctl' is too much work.
aa() { sudo apache2ctl $@ ;}

# cl FILE.el ("compile LISP"): byte-compile an emacs LISP file.
cl() { emacs -nw -q -batch -f batch-byte-compile $@ ;}

# cr /SYMLINK ("cd to referent"): cd to a symlink's referent.
# That way your $PS1 and pwd will show the full path, and autocompletion will
# work properly in emacs shell-mode.
cr() { cd $(~/bin/cr_helper $1) ;}

# diff *: I prefer git's color diffs. Override the diff command with 'git diff'.
alias diff='git diff'

# dos2unix FILE1 ...: translate-in-place DOS newlines to Unix newlines.
dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' $@ ;}

# dream: Dream of Electric Sheep.
dream() { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# ec FILE1 ...: because typing 'emacsclient' is too much work.
ec() { emacsclient $@ ;}

# ew /PATH ("edit which"): find a program in $PATH and edit it.
ew() { ec `which $1` ;}

# fd SUBSTRING: find file by (name) substring. This is my common find(1) usage.
fd() { find . -type d -name "*${1}*" ;}

# ffind *: find, excluding , excluding (D)VCS metadata and other metadata.
ffind() { find $@ | vrep ;}

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
gemi() {
    sudo gem install --remote \
                     --test \
                     --rdoc \
                     --ri \
                     $@
}

# gr REGEX ("grep recursively"): grep -ri . That's how I usually grep.
gr() { grep -ri $1 . ;}

# gi ("git init"): go through the multi-command dance to create a local git repo
gi() { git init && git add . && gc "Initial commit" ;}

# cg REGEX FILE1 ... ("count grep"): show matches per file, if not 0.
cg() { vrep | grep -v ':0' ;}

# ff [PROFILE_NAME]: run Firefox with a profile. No args? List/create profiles.
ff() {
    FIREFOX_EXECUTABLE="/Applications/Firefox\\ 3.app/Contents/MacOS/firefox-bin"
    if [[ $1 ]]; then
        # Run with a specific profile, named in $1.
        sh -exec "$FIREFOX_EXECUTABLE -ProfileManager $1"
    else
        # Run the Profile Manager (which allows you to view all profiles and
        # add a new one).
        sh -exec "$FIREFOX_EXECUTABLE -p"
    fi
}

# ga *: because typing 'git commit -a' is too much work
alias ga='git commit -a'

# mz: Use mozrepl to connect to Firefox for some interactive debugging.
# See http://wiki.github.com/bard/mozrepl/
mz() { socat READLINE TCP4:localhost:4242 ;}

# kindle FILE1 ...: load free eBooks into iPhone Kindle app. Requires jailbreak.
kindle() {
    IPHONE_HOSTNAME=te
    IPHONE_KINDLE_DIR=/var/mobile/Applications/A063DC1A-20BF-4A66-9858-288FF88DB3ED
    DEST='root@${IPHONE_HOSTNAME}:${IPHONE_KINDLE_DIR}/Documents/eBooks/'
    scp $@ $MOBI_PATH $ADIR/
}

# ls, always print one column, even if there are few files. It's easier to scan.
alias ls='ls -1F' # Must be written as an alias; a shell function would recurse.

# lsl *: list files by mtime, with permissions and ownership. I do this a lot.
lsl() { ls -lt $@ | less ;}

# lw Perl::Module, lw Perl/Module.pm ("library which"): show the path to Perl module in PERL5LIB. FIXME: test behavior
lw() { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which"): find a perl module in PER5LIB and open it in EDITOR. #FIXME: test behavior.
# elw() { $EDITOR $(lw $@) ;}

# start_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
start_mysql() { mysql.server ;}

# stop_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
stop_mysql() { mysqladmin5 -u root -p shutdown ;}

#####################
# Password-generating functions. All but pw depend on apg.
#####################

# pw: urandomly generate a new password base64-encoded).
pw() { dd if=/dev/urandom bs=$1 count=1 | base64 ;}

# newpassword_alphanumeric: as required by some dumb websites.
newpassword_alphanumeric() {
    N_CHARS=$1
    if [ ! $N_CHARS ]; then
	N_CHARS=10
    fi

    apg -n 1 -M Ncl -m $N_CHARS -x $N_CHARS -s
}

# newpassword_wep: generate an appropriate-length hex string WEB password.
newpassword_wep() { apg -n 1 -a 1 -M nc -m 26 -x 26 -E GHIJKLMNOPQRSTUVWXYZ ;}

# newpassword_mac: generate a random MAC address (with colons).
newpassword_mac() { apg -n 1 -a 1 -M nc -m 12 -x 12 -E GHIJKLMNOPQRSTUVWXYZ | xargs /home/quinn/bin/colonize ;}

# newpin: generate a new four-digit PIN, as required by many dumb ATMs.
newpin() { apg -n 1 -a 1 -M nc -m 4 -x 4 -E ABCDEFGHIJKLMNOPQRSTUVWXYZ ;}

# pb *: because typing 'perlbrew' is too much work.
pb() { perlbrew $@ ;}

# unrar_plural FILE1 ...: because unraring one file at a time is tiresome.
unrar_plural() { for f in *rar; do unrar x -o+ "$f" .; done ;}

# tor_wget URL1 [DEST_PATH]: get the contents of a URL, using Tor for anonymity.
tor_wget() {
    HTTP_PROXY=http://127.0.0.1:8118/ http_proxy=$HTTP_PROXY \
        wget --mirror --convert-links --html-extension --domains $1 $2
}

# tor_on: start proxying all this shell's HTTP traffic through Tor.
tor_on() { export HTTP_PROXY=http://127.0.0.1:8118/ http_proxy=$HTTP_PROXY ;}

# tor_off: stop proxying this shell's HTTP traffic through Tor.
tor_off() { unset HTTP_PROXY http_proxy ;}

# tunnel REMOTE_HOST REMOTE_PORT LOCAL_PORT: ssh-tunnel out.
tunnel() { ssh -fNR $2:localhost:$3 $1 ;}

# unix2dos FILE1 ...: translate-in-place newlines to the DOS convention (CRLF).
unix2dos() { /usr/bin/perl -pi -e ' s{$}{\r}x; ' $@ ;}

# xb ("xbindkeys"): reload custom (xbindkeys) X11 keybindings.
xb() { killall --user $USER xbindkeys && xbindkeys ;}

########################## 
########################## OS-, distro-, and version-specific settings
########################## 

OS=$(uname)
if [[ $OS = Darwin ]]; then
    source ~/dotfiles/.bashrc.d/unixes/os-x/any-os-x.sh
elif [[ $OS = Linux ]]; then
    source ~/dotfiles/.bashrc.d/unixes/linux/any-linux.sh
fi

########################## 
########################## Run daemons, if installed but not already runnning.
########################## 

# Load functions for running several daemons.
if source ~/dotfiles/.bashrc.d/daemons.sh; then
    # gpg-agent is the only daemon I still use, though.
    run_gpg_agent_idempotently
fi

########################## 
########################## Keep my passwords encrypted.
########################## These functions could use some work.
########################## 

#FIXME: use the github release of key-corto, not this copypasta.

export GPGUSER=Nobody
export GPGOPTS="--batch --quiet --no-tty --armor --user $GPGUSER"
export WALLET=~/wallet.asc

# _dginit() { # Helper function:  create the key and file, iff they don't exist.
#     if [ ! -e $WALLET ]; then
#         touch $WALLET
#     fi

#     gpg2 --list-keys Nobody
#     if [ $? != 0 ]; then
#         gpg2 --gen-key
# }

_decryp() { # Helper function:  decrypt the encrypted passwords file.
    gpg2 $GPGOPTS --decrypt $WALLET
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
        (_decryp $WALLET && echo $@) | gpg2 $GPGOPTS --encrypt > $TMPFILE \
            && mv $TMPFILE $WALLET
    fi
}
