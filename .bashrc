umask 0022

##########################
########################## <- Sections demarcated in hashes, like this,
########################## must appear in their present order, or code
########################## will break.
##########################

########################## 
########################## bash shell completion
########################## 
# Homebrew
# if [ -f $(brew --prefix)/etc/bash_completion ]; then
#    source $(brew --prefix)/etc/bash_completion
# fi

# Git
source ~/.git-completion.bash

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

############
# PostgreSQL
############
# Just use homebrew's version (see Section '# Homebrew', below).

#######################
# (Homebrewed) Python 3
#######################
# See https://github.com/mxcl/homebrew/wiki/Homebrew-and-Python
PYTHON3_PATH=~/.homebrew/share/python3 # install-scripts dir
PYTHON3_PATH=$PYTHON3_PATH:/usr/local/share/python # location of easy_install

#########################
# (Homebrewed) Ruby 1.9.3
#########################
# This dir will be the the location of any binaries installed by Ruby gems.
RUBYGEMS_PATH=~/.homebrew/Cellar/ruby/1.9.3-p194/bin

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

##########
# (Homebrewed) readline
#
# The installation process for readline explains,
#
#    This formula is keg-only, so it was not symlinked into
#    /Users/quinn/.homebrew. OS X provides the BSD libedit library, which
#    shadows libreadline. In order to prevent conflicts when programs look
#    for libreadline we are defaulting this GNU Readline installation to
#    keg-only.
#    
#    Generally there are no consequences of this for you.  If you build your
#    own software and it requires this formula, you'll need to add its lib &
#    include paths to your build variables:
#     
#        LDFLAGS  -L/Users/quinn/.homebrew/Cellar/readline/6.2.4/lib
#        CPPFLAGS -I/Users/quinn/.homebrew/Cellar/readline/6.2.4/include
##########

#############
# System path (OS-specific)
#############
OS=$(uname -s)
if [[ $OS = 'Darwin' ]]; then
    SYSTEM_INFOPATH=/usr/share/info:/usr/lib/info
    SYSTEM_MANPATH=/usr/share/man:/usr/X11/share/man
    SYSTEM_PATH=/bin:/usr/bin:/sbin:/usr/sbin:/usr/X11/bin
    SYSTEM_DYLD_LIBRARY_PATH=/usr/local/pgsql/lib
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
PERSONAL_PATH=~/bin:~/src/continuous
if [[ $OS = 'Darwin' ]]; then
    PERSONAL_PATH=~/bin/mac:$PERSONAL_PATH
fi
PERSONAL_PERL5LIB=~/perl5lib

# OK, now assemble PATH, MANPATH et cetera from the above-specified paths.
for VAR in PATH MANPATH INFOPATH LD_LIBRARY_PATH DYLD_LIBRARY_PATH PERL5LIB; do
    # 1. Clear the path, in order to get rid of OS-imposed cruft.
    eval "$VAR=''"

    # 2. Append any package-specific paths, in the desired order.
    for CATEGORY in POSTGRESQL PYTHON3 MYSQL HOMEBREW RUBYGEMS SYSTEM ELISP PERSONAL; do
        VALUE_BEFORE_APPENDING=$(eval echo \$$VAR)
        VALUE_TO_APPEND=$(eval echo \$${CATEGORY}_${VAR}) # e.g. $(eval echo \$POSTGRESQL_PATH) yields /usr/local/pgsql/bin
        if [[ $VALUE_TO_APPEND ]]; then
            if [[ ! $VALUE_BEFORE_APPENDING ]]; then
                eval "$VAR=$VALUE_TO_APPEND"
            else
                eval "$VAR=$VALUE_BEFORE_APPENDING:$VALUE_TO_APPEND"
            fi
        fi
    done
done

########################## 
########################## Programming language settings
########################## 

set -a

######
# Perl: a special case not handled by the preceding 'for' loop
######

# First add perlbrew itself to paths.
PERLBREW_ROOT=~/perl5/perlbrew
PERLBREW_PATH=$PERLBREW_ROOT/bin

# Then add perlbrew's version of perl to paths.
source $PERLBREW_ROOT/etc/bashrc

# In grep (1) output, highlight matches, line numbers, et cetera
# iff the terminal supports colors.
alias grep='grep --color=auto'

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
# Python virtualenv
########

# easy_install pip
# pip install virtualenv
# pip install virtualenvwrapper
VIRTUALENVWRAPPER_PYTHON=~/.homebrew/bin/python
VIRTUALENV_SCRIPT=~/.homebrew/share/python/virtualenv
VIRTUALENV_SCRIPT_DIR=$(dirname $VIRTUALENV_SCRIPT)
PATH=$PATH:$VIRTUALENV_SCRIPT_DIR
VIRTUALENVWRAPPER_SCRIPT=~/.homebrew/share/python/virtualenvwrapper.sh
source $VIRTUALENVWRAPPER_SCRIPT # adds a delay of several seconds. :(
WORKON_HOME=~/.virtualenvs
MKVE_OPTS='--no-site-packages'

# mkve26: make a Python 2.6 virtualenv.
mkve26() { mkvirtualenv $MKVE_OPTS --python $HOMEBREW_ROOT/Cellar/python26/2.6.8/bin/python "$@" ;}

# mkve27: make a Python 2.7 virtualenv.
mkve27() { mkvirtualenv $MKVE_OPTS --python $HOMEBREW_ROOT/Cellar/python/2.7.1/bin/python "$@" ;}

# mkve3: make a Python 3.3 virtualenv.
# mkve3() { mkvirtualenv $MKVE_OPTS --python $HOMEBREW_ROOT/Cellar/python/3.3.3/bin/python "$@" ;}

# qt_designer, for writing PyQt apps
qt_designer() { open '/usr/local/Cellar/qt/4.7.2/bin/Designer.app' ;}

rss() { newsbeuter "$@" ;}

# ve: short for 'virtualenv'
ve() { virtualenv "$@" ;}

######
# Ruby
######
RUBYOPT=rubygems

set +a

########################## 
########################## App-specific settings
########################## 

set -a

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

########################## 
########################## Shell functions and aliases, by category
##########################

#####################
# Password-generation functions (all but pw depend on apg).
#####################

# pw [n_chars] ("password"): urandomly generate a new password, base64-encoded.
pw() { dd if=/dev/urandom bs=$1 count=1 | base64 ;}

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

# diff *: I prefer git's color diffs. Override the diff command with 'git diff'.
alias diff='git diff'

# dos2unix FILE1 ...: translate-in-place DOS newlines to Unix newlines.
dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream: Dream of Electric Sheep.
dream() { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# ec FILE1 ...: because typing 'emacsclient' is too much work. '-c' means GUI.
ec() { emacsclient -c "$@" ;}

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

# ggrep *: grep, excluding (D)VCS metadata and other metadata.
ggrep() { grep "$@" | vrep ;}

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
gemi() {
    sudo gem install --remote \
                     --rdoc \
                     --ri \
                     "$@"
}

# gc: git commit -a (that is, add changes, but not new files, to the index).
gc() { git commit -a ;}

# gpg and gpg2 have identical UIs, so it's safe to alias them.
gpg() { gpg2 "$@" ;}

# gr REGEX ("grep recursively"): grep -ri . That's how I usually grep.
gr() { grep -ri $1 . ;}

# gi ("git init"): go through the multi-command dance to create a local git repo
gi() { git init && git add . && git commit -a -m 'Initial commit' ;}

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
ga() { git commit -a "$@" ;}

# mz: Use mozrepl to connect to Firefox for some interactive debugging.
# See http://wiki.github.com/bard/mozrepl/
mz() { socat READLINE TCP4:localhost:4242 ;}

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
if [[ $OS = Darwin ]]; then
    # Use homebrewed GNU ls.
    alias ls='gls -1 -G --group-directories-first'
elif [[ $OS = Linux ]]; then
    # GNU ls is the standard ls (unless this is some really eccentric distro).
    alias ls='ls -1 -G --group-directories-first'
else
    # Other OS'es may or may not use GNU ls. Until we learn more, play it safe
    # by omitting the GNU-specific --group-directories-first option.
    alias ls='ls -1 -G'
fi

# lsl *: list files by mtime, with permissions and ownership. I do this a lot.
lsl() { ls -lt "$@" | less ;}

# lw Perl::Module, lw Perl/Module.pm ("library which"): show the path to Perl module in PERL5LIB. FIXME: test behavior
lw() { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which"): find a perl module in PER5LIB and open it in EDITOR. #FIXME: test behavior.
# elw() { $EDITOR $(lw "$@") ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder.
# Fails if the directory contains anything besides .DS_Store.
rmds() {
    rm -rfi "$1/.DS_Store"
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
    OUTPUT=`tmux list-sessions 2>/dev/null`
    if [[ $? == 0 ]]; then
        echo $OUTPUT
    else
        echo -n
    fi
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
########################## OS-, distro-, and version-specific settings
########################## 

OS=$(uname)
if [[ $OS = Darwin ]]; then
    source ~/.bashrc.d/unixes/os-x/any-os-x.sh
elif [[ $OS = Linux ]]; then
    source ~/.bashrc.d/unixes/linux/any-linux.sh
fi

########################## 
########################## Run daemons, if installed but not already runnning.
########################## 

# Load functions for running several daemons.
if source ~/.bashrc.d/daemons.bash; then
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
