umask 0022

# Environment variables. Define these idempotently in ~/.bashrc rather than in
# ~/.bash_profile so that each new shell will get the new settings.

if [[ -e ~/.bashrc.d/local.bash ]]; then
    source ~/.bashrc.d/local.bash
fi

set -a

if (! which gpg > /dev/null 2>&1); then
    alias gpg=gpg2
fi

OS=$(uname -s)

#####################
# My own utility code
#####################
PERSONAL_PATH=~/bin:~/src/continuous
if [[ $OS = 'Darwin' ]]; then
    #FIXME: convert these (from shell scripts) to aliases in any-os-x.sh
    PERSONAL_PATH=$PERSONAL_PATH:~/bin/mac
fi

######
# PATH
######
PATH=/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:/opt/X11/bin:$PERSONAL_PATH
PERSONAL_PERL5LIB=~/perl5lib
PERL5LIB=$PERL5LIB:$PERSONAL_PERL5LIB

# Platform-specific env var settings
if [[ $OS = Darwin ]]; then
    source ~/.bashrc.d/unix/os-x/any-os-x.bash
elif [[ $OS = Linux ]]; then
    source ~/.bashrc.d/unix/linux/any-linux.bash
    if (grep ^Debian /etc/issue > /dev/null 2>&1); then
        source ~/.bashrc.d/unix/linux/debian-gnu-linux.bash
    else
        # Guess.
        DIST_FILE=~/.bashrc.d/unix/linux/unknown-linux.bash
        echo "Warning: unrecognized distro. Defaulting to config in $DIST_FILE"
        source $DIST_FILE
    fi
else
    echo "Warning: unsupported OS $OS. Falling back to system defaults."
fi

##########################
########################## <- Sections demarcated in hashes, like this,
########################## must appear in their present order, or code
########################## will break.
##########################

########################## 
########################## bash shell completion
########################## 

# shell completion for brew(1)
# if [[ -f $(brew --prefix)/etc/bash_completion ]]; then
#    source $(brew --prefix)/etc/bash_completion
# fi

# shell completion for git(1)
source ~/.git-completion.bash

##########
# Homebrew (OS X)
##########
if [[ $(uname -n) != 'tao.local' ]]; then
    HOMEBREW_BUILD_FROM_SOURCE=1
fi

#######################
# perlbrew (all Unixes)
#######################
source ~/perl5/perlbrew/etc/bashrc

#################
# perl in general
#################
#
# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
PERL_MM_USE_DEFAULT=1

############
# PostgreSQL
############

# psql: keep all history forever
HISTFILESIZE=
HISTSIZE=

# If I forget to specify a DB, use one that's safe to trash.
PGDATABASE=sandbox

# Start postgres (if it's not running already).
HOMEBREW_PGDATA=$HOMEBREW_ROOT/var/postgres
if [[ -d "$HOMEBREW_PGDATA" ]]; then
    PGDATA="$HOMEBREW_PGDATA"

    pg_ctl status >/dev/null
    if [[ $? = 3 ]]; then
        pg_ctl -D $PGDATA -l $PGDATA/server.log start > /dev/null
    fi
fi

########
# Python virtualenv
########

# One-time setup:
# easy_install pp
# pip install virtualenv
# pip install virtualenvwrapper

# VIRTUALENVWRAPPER_PYTHON=$HOMEBREW_ROOT/bin/python
# VIRTUALENV_SCRIPT=$HOMEBREW_ROOT/share/python/virtualenv

# VIRTUALENV_SCRIPT_DIR=$(dirname $VIRTUALENV_SCRIPT)
# PATH=$PATH:$VIRTUALENV_SCRIPT_DIR
# VIRTUALENVWRAPPER_SCRIPT=$HOMEBREW_ROOT/share/python/virtualenvwrapper.sh
# source $VIRTUALENVWRAPPER_SCRIPT
# WORKON_HOME=~/.virtualenvs
# MKVE_OPTS='--no-site-packages'

# # mkve26: make a Python 2.6 virtualenv.
# mkve26() {
#     mkvirtualenv $MKVE_OPTS \
#         --python $HOMEBREW_ROOT/Cellar/python26/2.6.8/bin/python "$@"
# }

# # mkve27: make a Python 2.7 virtualenv.
# mkve27() {
#     mkvirtualenv $MKVE_OPTS \
#         --python $HOMEBREW_ROOT/Cellar/python/2.7.1/bin/python "$@"
# }

# # mkve3: make a Python 3.3 virtualenv.
# mkve3() {
#     mkvirtualenv $MKVE_OPTS \
#         --python $HOMEBREW_ROOT/Cellar/python/3.3.3/bin/python "$@"
# }

######
# Ruby
######
RUBYOPT=rubygems

###################
# bash, emacs, less
###################

# Editor: emacsclient. The empty ALTERNATE_EDITOR setting tells emacsclient
# to start an emacs session ('emacs --daemon') if one is not running already.
EDITOR=emacsclient; VISUAL=$EDITOR
## ALTERNATE_EDITOR='' ## murr, doesn't pick up ~/.emacs correctly. Fix later.

if [[ $RUNNING_UNDER_EMACS ]]; then # this var is set by ~/.emacs.d/init_bash.sh
    # Emacs shell-mode is a dumb terminal, so don't use advanced features:
    
    # ANSI colors don't work. Use a non-color shell prompt.
    PS1='$(__git_ps1 "[%s] ")\u@\h:\w\$ '
    
    # Pagers don't work. Use cat(1) instead of less(1).
    PAGER=cat; GIT_PAGER=$PAGER; ACK_PAGER=$PAGER
    
else
    # Other terminals can do fancier stuff:
    
    # Set the window title to the name of the current process.
    ## Agh, this doesn't work. PROMPT_COMMAND doesn't get executed when I wish
    ## it would (bash can't do this, but zsh's precmd() hook can.
    ## See http://www.ibiblio.org/pub/Linux/docs/HOWTO/Xterm-Title
    ## PROMPT_COMMAND='echo -ne    "\033]2;"   $(ps uxwc | grep $$ | awk "{ print \$11 }")   "\007"'
    
    # Color the shell prompt slate blue (to distinguish commands from output).
    # If I'm on a git branch, preend the branch name in purple.
    PS1='\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;1m\]\u@\h:\w\$\[\e[0m\] '
    
    # Use my favorite pager and pager options.
    LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --no-init'
    PAGER=less; GIT_PAGER=$PAGER; ACK_PAGER=$PAGER
    
    # Use vi for quick edits inside this terminal window.
    EDITOR=vi; VISUAL=$EDITOR
fi

#####
# FTP
#####
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=''

set +a

#############################
# Aliases and shell functions
#############################

rss() { newsbeuter "$@" ;}

# ve: short for 'virtualenv'
ve() { virtualenv "$@" ;}

########################## 
########################## Shell functions and aliases, by category
##########################

#####################
# Password-generation functions (all but pw depend on apg).
#####################

# pw [n_chars] ("password"): urandomly generate a new password, base64-encoded.
pw() { /bin/dd if=/dev/urandom bs=$1 count=1 | base64 ;}

# newpassword_alphanumeric LENGTH_IN_CHARS: as required by some dumb websites.
newpassword_alphanumeric() {
    N_CHARS=$1
    apg -n 1 -M Ncl -m $1 -x $1 -s
}

# newpassword_wep: generate a new WEP password.
newpassword_wep() { apg -n 1 -a 1 -M nc -m 26 -x 26 -E GHIJKLMNOPQRSTUVWXYZ ;}

# newpin: generate a new four-digit PIN, as required by many dumb ATMs.
newpin() { apg -n 1 -a 1 -M nc -m 4 -x 4 -E ABCDEFGHIJKLMNOPQRSTUVWXYZ ;}

# random_mac_address: generate a random MAC address (with colons). Caveat hacker: some of the MAC addresses you get will, of course, be in use by real devices.
newpassword_mac() { apg -n 1 -a 1 -M nc -m 12 -x 12 -E GHIJKLMNOPQRSTUVWXYZ | xargs /home/quinn/bin/colonize ;}

###########
# Processes #FIXME: stop using grep as a golden hammer; instead, use awk to separate fields in the output of ps(1).
###########

# Display all processes, in a style that I like.
# If an argument is given, display only those that match that regex.
p() { ps aux | grep -v grep | grep "$@" --color=auto ;}

# Display my processes.
pu() { p $USER ;}

# grep the list of processes, but don't show any output.
# This is useful for non-interactive functions that just need
# to test whether a process is running.
pq() {
    p auxww | grep -q $1 | grep -qv grep
}

# For some asinine reason, psql doesn't listen to options when I specify
# them in ~/.psqlrc, so I'm just aliasing it to change its behavior.
alias psql="PGOPTIONS='--client-min-messages=warning' $(which psql) --quiet -x"

alias vi=vim

# grep, excluding (D)VCS metadata and other metadata. # FIXME: implement in one grep command, not a pipeline, so that I don't have to wait for the whole thing to complete before I start seeing output.
vrep() {
    grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

########################## 
########################## Miscellaneous shell functions and aliases
########################## (sorted alphabetically)
##########################

# aa graceful|start|...: because typing 'sudo apache2ctl' is too much work.
aa() { sudo apache2ctl "$@" ;}

atom() { newsbeuter "$@" ;}

# cl FILE.el ("compile LISP"): byte-compile an emacs LISP file.
cl() { emacs -nw -q -batch -f batch-byte-compile "$@" ;}

# cr /SYMLINK ("cd to referent"): cd to a symlink's referent.
# That way your $PS1 and pwd will show the full path, and autocompletion will
# work properly in emacs shell-mode.
cr() { cd $(~/bin/cr_helper $1) ;}

# dos2unix FILE1 ...: translate-in-place DOS newlines to Unix newlines.
dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream: Dream of Electric Sheep.
dream() { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# ec FILE1 ...: because typing 'emacsclient' is too much work. '-c' means GUI.
ec() { emacsclient -c "$@" ;}

# Run Cocoa Emacs from a shell. That way it inherits the shell's environment.
# If you run Emacs by clicking its icon, that doesn't happen, because OS X
# does not run ~/.bash_profile or ~/.bash_login when you log in.
COCOA_EMACS=~/Applications/Emacs.app/Contents/MacOS/Emacs
if [[ -x "$COCOA_EMACS" ]]; then
    alias emacs="$COCOA_EMACS"
fi

# et FILE1 ...: run emacsclient in terminal mode, not GUI mode.
et() { emacsclient -t "$@" ;}

# ew /PATH ("edit which"): find a program in $PATH and edit it.
ew() { ec `which $1` ;}

# fn SUBSTRING: find a file by case-insens name (my common find(1) case)
fn() { STAR='*'; find $1 -iname "${STAR}${2}${STAR}" ;}

# fd SUBSTRING: find file by (name) substring (next common find(1) usage).
fd() { find . -type d -name "*${1}*" ;}

# ffind *: find, excluding (D)VCS metadata and other metadata.
ffind() { find "$@" | vrep ;}

alias grep='grep --color'

gggrep() { grep "$@" | vrep ;}

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
gemi() {
    sudo gem install --remote \
                     --rdoc \
                     --ri \
                     "$@"
}

# gc: git commit -a (that is, add changes, but not new files, to the index).
gc() { git commit -a ;}

# gr REGEX ("grep recursively"): grep -ri . That's how I usually grep.
gr() { grep -ri $1 . ;}

# gi ("git init"): go through the multi-command dance to create a local git repo
gi() { git init && git add . && git commit -a -m 'Initial commit' ;}

# cg REGEX FILE1 ... ("count grep"): show matches per file, if not 0.
cg() { vrep | grep -v ':0' ;}

# ff [PROFILE_NAME]: run Firefox with a profile. No args? List/create profiles.
ff() {
    FIREFOX_EXECUTABLE='/Applications/Firefox.app/Contents/MacOS/firefox-bin'
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
ga() { git commit -a "$@" ;}

# mz: Use mozrepl to connect to Firefox for some interactive debugging.
# See http://wiki.github.com/bard/mozrepl/
mz() { socat READLINE TCP4:localhost:4242 ;}

info() {
    if [[ $OS = Darwin ]]; then
        echo "Don't use info on OS X; it will give you out-of-date docs."
    else
        info "$@"
    fi
}

js() {
    FILE=$1
    shift
    jsl -process "$FILE" "$@"
}

# kindle FILE1 ...: load free eBooks into iPhone Kindle app. Requires jailbreak.
kindle() {
    IPHONE_HOSTNAME=te
    IPHONE_KINDLE_DIR=/var/mobile/Applications/A063DC1A-20BF-4A66-9858-288FF88DB3ED
    DEST='root@${IPHONE_HOSTNAME}:${IPHONE_KINDLE_DIR}/Documents/eBooks/'
    scp "$@" $MOBI_PATH $ADIR/
}

# Some Red Hat versions alias ls='ls --color=auto' in /etc/profile. That's bad!
# It causes the following unexpected behavior:
#
#    bash$ touch filename
#    bash$ chmod 400 $_
#    chmod: unrecognized option '--color=auto'
#
# unalias ls (in case it's aliased) so that doesn't happen
# (ignoring complaints that it isn't aliased anyway).
unalias ls 2>/dev/null

# ls: always print one column, even if there are few files. It's easier to scan.
# This must be written as an alias, because a shell function would recurse.
if [[ $OS = Darwin ]] && [[ -d /usr/local/opt/coreutils/libexec/gnubin ]]; then
    PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
    MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
    alias ls='ls  -1 --color=auto --group-directories-first'
elif [[ $OS = Linux ]]; then
    alias ls='ls  -1 --color=auto --group-directories-first'
else
    alias ls='ls -1 -G'
fi

# lw Perl::Module, lw Perl/Module.pm ("library which"): show the path to Perl module in PERL5LIB. FIXME: test behavior
lw() { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which"): find a perl module in PER5LIB and open it in EDITOR. #FIXME: test behavior.
# elw() { $EDITOR $(lw "$@") ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder.
# Fails if the directory contains anything besides .DS_Store.
rmds() {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
}

# serve: shares all the files in the current folder over HTTP, port 8080
serve() { python -m SimpleHTTPServer 8080 ;}

# start_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
start_mysql() { mysql.server ;}

# stop_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
stop_mysql() { mysqladmin5 -u root -p shutdown ;}

# pb *: because typing 'perlbrew' is too much work.
pb() { perlbrew "$@" ;}

# pi ("Perl @INC"): print @INC dirs, one per line, preceded by the path to Perl.
pi() {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

# sw: switch to a shorter bash prompt (for when I'm in dirs with long paths).
#alias sw='export PS1="\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;m\]\u@\h:\W\$\[\e[0m\] "'

# sww: switch back to a longer bash prompt.
#alias sww='export PS1="\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;m\]\u@\h:\w\$\[\e[0m\] "'

# title STRING: set the window title of an xterm/iTerm2/rxvt window to STRING.
title() { echo -ne "\033]2;" $1 "\007" ;}

# ta $SESSION_NAME ("Tmux Attach"): attach to named session, creating if needed.
ta() {
    tmux has-session -t $1 2>/dev/null
    if [[ $? -ne 0 ]]; then
        tmux new-session -d -s $1 -n $1
    fi
    tmux -2 attach-session -t $1
}

# tls ("Tmux List"): list running tmux sessions.
tls() {
    tmux list-sessions 2>/dev/null
}

# tl: alias for tls, so I don't have to do all that tiresome extra typing!
tl() { tls "$@" ;}

top() { echo "Don't use top; use htop instead." ;}

# tor_wget URL1 [DEST_PATH]: get the contents of a URL, using Tor for anonymity.
tor_wget() {
    HTTP_PROXY=http://127.0.0.1:8118/ http_proxy=$HTTP_PROXY \
        wget --mirror --convert-links --html-extension --domains $1 $2
}

# tor_on: start proxying all this shell's HTTP traffic through Tor.
tor_on() { export HTTP_PROXY=http://127.0.0.1:8118/ http_proxy=$HTTP_PROXY ;}

# tor_off: stop proxying this shell's HTTP traffic through Tor.
tor_off() { unset HTTP_PROXY http_proxy ;}

# unix2dos FILE1 ...: translate-in-place newlines to the DOS convention (CRLF).
unix2dos() { /usr/bin/perl -pi -e ' s{$}{\r}x; ' "$@" ;}

# unrar_plural FILE1 ...: because unraring one file at a time is tiresome.
unrar_plural() { for f in *rar; do unrar x -o+ "$f" .; done ;}

# xb ("xbindkeys"): reload custom (xbindkeys) X11 keybindings.
xb() { killall --user $USER xbindkeys && xbindkeys ;}

########################## 
########################## Run daemons, if installed but not already runnning.
########################## Applies to my personal Mac only.
##########################

# Load functions for running several daemons.
DAEMON_HELPERS=~/.bashrc.d/daemons.bash
if [[ -f "$DAEMON_HELPERS" ]]; then
    source "$DAEMON_HELPERS"
    run_gpg_agent_idempotently
    run_ssh_agent_idempotently
fi

##########################
# Client-specific settings (not in git, as they may contain sensitive info).
##########################
CSFILE=~/.bashrcs.client-specific.d/Source-me.bash
if [[ -f "$CSFILE" ]]; then
    source "$CSFILE"
fi
