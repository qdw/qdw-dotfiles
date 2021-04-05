# LOCAL_DEV_ENV_BASHRC=~/src/dev/platform/devenv/swf-dotfile.bash
# if [[ -e $LOCAL_DEV_ENV_BASHRC ]]; then
#     source $LOCAL_DEV_ENV_BASHRC
# fi

umask 0022

set -a

if which brew > /dev/null 2>&1 && which gfind > /dev/null 2>&1; then
    alias find=gfind
fi

if (! which gpg > /dev/null 2>&1); then
    alias gpg=gpg2
fi

OS=$(uname -s)

#### bash shell completion

# Generic shell completion support.
#source /usr/local/Cellar/bash-completion/1.3_1/etc/profile.d/bash_completion.sh

# shell completion for git(1)
unset GIT_COMPLETION_ENABLED
BASH_GIT_COMPLETION_FILES="/usr/local/etc/bash_completion.d/git-completion.bash /usr/local/etc/bash_completion.d/git-prompt.sh"
for GC_FILE in $BASH_GIT_COMPLETION_FILES; do
    if [[ -f $GC_FILE ]]; then
        source $GC_FILE
        GIT_COMPLETION_ENABLED=1
    fi
done

# shell completion for golang-jira, https://github.com/go-jira/jira
if [[ -e ~/.bashrc.d/completion/jira.bash ]]; then
    source ~/.bashrc.d/completion/jira.bash
fi

# vagrant(1) shell completion.
# source ~/.bashrc.d/completion/vagrant.bash

# heroku shell completion ('heroku' installed via 'brew install heroku';
# completion installed via a gem, I think, and also available at
# https://github.com/stefansundin/heroku-bash-completion
#source ~/.heroku/plugins/heroku-bash-completion/heroku-completion.bash

# awscli
complete -C aws_completer aws

#### No, really, I like bash, so please stop telling me to use zsh, macOS.

export BASH_SILENCE_DEPRECATION_WARNING=1

#### The Go programming language #

GOPATH=~/go

############################
# PATH, MANPATH, et cetera #
############################

# My own utility code
PERSONAL_PATH=~/bin/3:~/bin:~/src/continuous:~/d/sv/bin
if [[ $OS = 'Darwin' ]]; then
    #FIXME: convert these (from shell scripts) to aliases in any-os-x.sh
    PERSONAL_PATH=$PERSONAL_PATH:~/bin/mac
fi

########
# PATH #
########
PATH=/usr/local/bin:/usr/local/postgresql-9.4/bin:/usr/local/Cellar/python/3.7.5/bin:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin:/opt/X11/bin:$GOPATH/bin:$PERSONAL_PATH
PERL5LIB=$PERL5LIB:$PERSONAL_PERL5LIB:~/perl5lib

# Platform-specific env var settings
# # if [[ $OS = Darwin ]]; then
# #   source ~/.bashrc.d/unix/os-x/any-os-x.bash
#elif [[ $OS = Linux ]]; then
# #   source ~/.bashrc.d/unix/linux/any-linux.bash
# #   if (grep ^Debian /etc/issue > /dev/null 2>&1); then
# #       source ~/.bashrc.d/unix/linux/debian-gnu-linux.bash
# #   else
# #       # Guess.
# #       DIST_FILE=~/.bashrc.d/unix/linux/unknown-linux.bash
# #       echo "Warning: unrecognized distro. Defaulting to config in $DIST_FILE"
# #       source $DIST_FILE
# #   fi
#else
# #   echo "Warning: unsupported OS $OS. Falling back to system defaults."
# # fi

#### <- Sections demarcated in hashes, like this,
#### must appear in their present order, or code
#### will break.


#### Homebrew (OS X)

# Deprecated; will be disabled
# 2017-10-31,
# at which point the caskroom will be hard-coded to 
# /usr/local/Caskroom
# For instructions on how to move existing casks to the new caskroom, see
# https://stackoverflow.com/questions/39430625/the-default-caskroom-location-has-moved-to-usr-local-caskroom
#    mv /opt/homebrew-cask/Caskroom /usr/local
# for f in ~/Applications/*.app; do
#     oldloc="$(readlink "$f")"
#     [[ -e $oldloc ]] || ln -sf "/usr/local${oldloc#/opt/homebrew-cask}" "$f"
# done
HOMEBREW_NO_ANALYTICS=1
HOMEBREW_GITHUB_API_TOKEN=83796ddb9cb9f959f16680921613282842578009

#### Perl

# perlbrew (all Unixes)
PBRC=~/perl5/perlbrew/etc/bashrc
if [[ -e $PBRC ]]; then
    source $PBRC
fi

# # Homebrewed Python 3 (macOS)
# if [[ $(uname -s) = 'Darwin' ]] && which brew > /dev/null 2>&1 && brew ls | grep 'python[@]3'; then
#     export PATH=/usr/local/Cellar/python@3.8/3.8.5:$PATH

# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
PERL_MM_USE_DEFAULT=1

#### PostgreSQL

# psql: keep all history forever
HISTFILESIZE=
HISTSIZE=

#### Python virtualenv

# One-time setup:
# easy_install pp
# pip install virtualenv
# pip install virtualenvwrapper

#### Ruby

RUBYOPT=rubygems

#### bash, emacs, less

# Add a timestamp to history(1) and ~/.bash_history
# using strftime(3) formatting.
HISTTIMEFORMAT='%Y%m%dT%H%M%S%z  '

TERM=xterm-256color

# Editor: emacsclient. The empty ALTERNATE_EDITOR setting tells emacsclient
# to start an emacs session ('emacs --daemon') if one is not running already.
VISUAL=emacsclient; EDITOR=$VISUAL
## ALTERNATE_EDITOR='' ## murr, doesn't pick up ~/.emacs correctly. Fix later.

if [[ $RUNNING_UNDER_EMACS ]]; then # this var is set by ~/.emacs.d/init_bash.sh
    # Emacs shell-mode is a dumb terminal, so don't use advanced features:

    # ANSI colors don't work. Use a non-color shell prompt.
    if [[ $GIT_COMPLETION_ENABLED ]]; then
        PS1='$(__git_ps1 "[%s] ")\u@\h:\w\$ '
    else
        PS1='\u@\h:\w\$ '
    fi
   
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
    if [[ $GIT_COMPLETION_ENABLED ]]; then
        # If I'm on a git branch, prepend the branch name in purple.
        PS1='\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;1m\]\u@\h:\w\$\[\e[0m\] '
    else
        PS1='\[\e[34;1m\]\u@\h:\w\$\[\e[0m\] '
    fi
   
    # Use my favorite pager and pager options.
    LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --no-init'
    PAGER=less; GIT_PAGER=$PAGER; ACK_PAGER=$PAGER
   
    # Use vi for quick edits inside this terminal window.
    EDITOR=vim; VISUAL=$EDITOR
fi

#### FTP

FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=''

#### Textbelt

TEXTBELT_KEY=33f76b5c08245a670cf6e41b3a2e9cb94b75133elik9G59XFQzTOjoPvA6y3crcc
PHONE_NUMBER=5105205217

set +a

######## Shell functions and aliases, by category

#### PostgreSQL-related functions.

ips() { /sbin/ifconfig -a | grep 'inet ' | grep -v 'inet6 ' ;}

ipv6() { /sbin/ifconfig -a | grep 'inet6 ' ;}

flast() {
    head -1 $1 | awk '{ print $1, $2; }'
    tail -1 $1 | awk '{ print $1, $2; }'
}

# mhost ("map [to] host(s)"): execute a command
# on a list of hostnames, with the hostname as
# the command's last arg
mhost() {
    COMMAND="$1"; shift
    for NODE in $@; do
        echo $NODE
        echo ==
        echo 
        $NODE "$COMMAND"
        echo
    done
}

# mshost ("map ssh [to] host(s)"): execute an ssh command
# on a list of hosts, with paragraphized output
mshost() {
    REMOTE_COMMAND="$1"; shift
    COMMAND="ssh $REMOTE_COMMAND"
    mhost "$COMMAND" $@
}

#### Password-generation functions (all but pw depend on apg).

# pw [n_chars] ("password"): urandomly generate a new password, base64-encoded.
pw() { /bin/dd if=/dev/urandom bs=$1 count=1 | base64 ;}

# newpassword_alphanumeric LENGTH_IN_CHARS: as required by some poorly-coded websites.
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

#### Processes #FIXME: stop using grep as a golden hammer; instead, use awk to separate fields in the output of ps(1).

# Display all processes, in a style that I like.
# If an argument is given, display only those that match that regex.
function p() { ps auxwww | head -1; ps auxwww | grep -v grep | grep $@ ;}

# Display my processes.
pu() { p $USER ;}

# grep the list of processes, but don't show any output.
# This is useful for non-interactive functions that just need
# to test whether a process is running.
pq() {
    p auxww | grep -q $1 | grep -qv grep
}

alias vi=vim
vim_less() {
    $VIMRUNTIME/macros/less.sh $@
}

# grep, excluding (D)VCS metadata and other metadata. # FIXME: implement in one grep command, not a pipeline, so that I don't have to wait for the whole thing to complete before I start seeing output.
vrep() {
    grep -v '\.git' | grep -v _MTN | grep -v '\.svn' | grep -v CVS | grep -v '\.DS_Store'
}

#### ssh-related shell functions and aliases

alias ssh-keygen='ssh-keygen -t ed25519'

# The first time I ssh to a host, configure it by installing my public
# ssh keys, my favorite dotfiles, my favorite yum packages, et cetera.

function quinnify_host() {
    # set -x
    
    if [[ ! $1 ]]; then
        echo "Error: missing argument. Usage: quinnify_host $HOSTNAME_OR_SSH_HOSTALIAS"
        return 255
    fi
    
    NODE="$1"

    DOTFILES='
        .bashrc.qweaver.bash
        .emacs
        .gitconfig
        .hushlogin
        .inputrc
        .psqlrc
        .screenrc
        .tmux.qweaver.conf
        .vimrc.qweaver
        .vim
   '

    PACKAGES='
        ack
        colordiff
        emacs-nox
        htop
        nc
        nmap
        pgcenter
        pv
        tmux
    '

    ssh-copy-id -i ~/.ssh/qweaver_switchfly_3_id_ed25519 $NODE > /dev/null 2>&1

    rsync -rptgoDL ~/...sms-me $NODE:~/

    #### Dotfiles.

    cd ~/dotfiles
    rsync -rptgoDL $DOTFILES ${NODE}:~/
    cd - > /dev/null

    ssh $NODE touch .quinnified

    # set +x
}

my_ssh() {
    NODE=$1

    if (! ssh $NODE test -e ~/.quinnified > /dev/null 2>&1); then
        quinnify_host $NODE
    fi
    
    \ssh $NODE
}

# alias ssh=my_ssh        

#### Miscellaneous shell functions and aliases, in ASCIIbetical order.

# aa graceful|start|...: because typing 'sudo apache2ctl' is too much work.
aa() { sudo apache2ctl "$@" ;}

atom() { newsbeuter "$@" ;}

bu() { brew update && brew upgrade --build-from-source ;}

# cl FILE.el ("compile LISP"): byte-compile an emacs LISP file.
cl() { emacs -nw -q -batch -f batch-byte-compile "$@" ;}

# cr /SYMLINK ("cd to referent"): cd to a symlink's referent.
# That way your $PS1 and pwd will show the full path, and autocompletion will
# work properly in emacs shell-mode.
cr() { cd $(~/bin/cr_helper $1) ;}

# d ("date"): give today's date in YYYY-MM-DD format. Useful for interpolating.
d() { date +%Y-%m-%d ;}

# dcc: do a color diff of two files.
dcc() { colordiff -u $@ | less -R ;}

# dos2unix FILE1 ...: translate-in-place DOS newlines to Unix newlines.
dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream: Dream of Electric Sheep.
dream() { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# ec FILE1 ...: because typing 'emacsclient' is too much work. '-c' means GUI.
ec() { emacsclient --alternate-editor "emacs -nw" -c "$@" ;}
# et FILE1 ...: run emacsclient in terminal mode, not GUI mode.
et() { emacsclient --alternate-editor "emacs -nw" -t "$@" ;}

# Run Cocoa Emacs from a shell. That way it inherits the shell's environment.
# If you run Emacs by clicking its icon, that doesn't happen, because OS X
# does not run ~/.bash_profile or ~/.bash_login when you log in.
COCOA_EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
if [[ -x "$COCOA_EMACS" ]]; then
    alias emacs="$COCOA_EMACS"
fi

# ew /PATH ("edit which"): find a program in $PATH and edit it.
ew() { ec `which $1` ;}

# fn SUBSTRING: find a file by case-insens name (my common find(1) case)
fn() { STAR='*'; find $1 -iname "${STAR}${2}${STAR}" ;}

# fd SUBSTRING: find file by (name) substring (next common find(1) usage).
fd() { find . -type d -name "*${1}*" ;}

# ffind *: find, excluding (D)VCS metadata and other metadata.
ffind() { find "$@" | vrep ;}

# Raise a Growl notification from an iTerm2 session.
# Requires macOS, the iTerm2 app (available at https://iterm2.com/),
# and Growl (available in the Mac App Store; http://growl.info/;
# https://itunes.apple.com/us/app/growl/id467939042?mt=12&uo=4&at=11l7hH
# ).
#
# This shell function courtesy of Sergey Markov
# (https://web.archive.org/web/20190312205851/http://www.sergeymarkov.com/blog/2013/07/using-iterm-and-growl/)
# 
# Per Markov, "This version will work under screen".
#
########################################################
#
# Addendum 2020-08-25: but actually it's totally broken!
# And Growl has been unmaintained for years.
#
########################################################
# growl() {
#       local msg="\\e]9;\n\n${*}\\007"
#       case $TERM in
#         screen*)
#           echo -ne '\eP'${msg}'\e\\' ;;
#         *)
#           echo -ne ${msg} ;;
#       esac
#       return
# }

gs() { git status "$@" ;}

gggrep() { egrep --color=auto "$@" | vrep ;}

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
gr() { egrep --color=auto -ri $1 . ;}

# gi ("git init"): go through the multi-command dance to create a local git repo
gi() { git init && git add . && git commit -a -m 'Initial commit' ;}

# cg REGEX FILE1 ... ("count grep"): show matches per file, if not 0.
cg() { vrep | egrep --color=auto -v ':0' ;}

# dfr ("df real"): show all mounted devices that are actually disks.
dfr() {
    df -h | grep ^/
}

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

ho() { grep "Host .*${1}" ~/.ssh/*conf* ;}

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

# mr ("mount real"): show all mounted devices that are actually disks. Seee also dfr
mr() {
    mount | grep ^/
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
    alias ls='gls -1AF --group-directories-first --color=auto'
elif [[ $OS = Linux ]]; then
    alias  ls='ls -1AF --color=auto'
else
    alias  ls='ls -1AF'
fi

# lw Perl::Module, lw Perl/Module.pm ("library which"): show the path to Perl module in PERL5LIB. FIXME: test behavior
lw() { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which"): find a perl module in PER5LIB and open it in VISUAL. #FIXME: test behavior.
# elw() { $VISUAL $(lw "$@") ;}

# mz: Use mozrepl to connect to Firefox for some interactive debugging.
# See http://wiki.github.com/bard/mozrepl/
mz() { socat READLINE TCP4:localhost:4242 ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder.
# Fails if the directory contains anything besides .DS_Store.
rmds() {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
}

rss() { newsbeuter "$@" ;}

# start_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
start_mysql() { mysql.server ;}

# stop_mysql: I use MySQL seldom and forget its asymmetric start/stop commands.
stop_mysql() { mysqladmin5 -u root -p shutdown ;}

alias strings='strings -a' # because http://www.jwz.org/blog/2014/10/we-live-in-a-magical-future-where-strings-is-exploitable/

# ve: short for 'virtualenv'
ve() { virtualenv "$@" ;}

# para: print any paragraph contaning the desired string.
# A paragph is defined as a collection of lines followed
# by an all-blank line (i.e., a newline).
para() {
    STRING="$1"
    shift
    awk -v ORS="\n\n" -v RS='' "/$STRING/" $@
}

conf() { para "$1" ~/.ssh/*.config | egrep --color=auto -e '^' -e "$1" | less -R --quit-if-one-screen ;}

# pb *: because typing 'perlbrew' is too much work.
pb() { perlbrew "$@" ;}

# pi ("Perl @INC"): print @INC dirs, one per line, preceded by the path to Perl.
pi() {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

# shn ("SSH hostname"): print the canonical hostname for a ~/.ssh/config alias.
shn() {
    egrep --color=auto -A 1 "$1" ~/.ssh/*config* | tail -1 | awk '{ print $3; }'
}

alias tac='gtac'    

# ta $SESSION_NAME ("Tmux Attach"): attach to named session, creating if needed.
ta() {
    if (! tmux has-session -t $1 2> /dev/null); then
        tmux new-session -s $1 -n $1
    else
        tmux -2 attach-session -t $1
    fi
}

# tn ("Tmux New"): create a new tmux session, because ta breaks on old tmux versions.
tn() {
    tmux new-session -n $1 -s $1
}

# tl ("Tmux List"): list running tmux sessions.
tl() {
    tmux list-sessions 2> /dev/null
}

timestamp() { date +%Y%m%dT%H%M%S%z ;}

# title STRING: set the window title of an xterm/iTerm2/rxvt window to STRING.
title() { echo -ne "\033]2;" $1 "\007" ;}

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

# in_minutes: run a command in the future.
# Useful when at(1) isn't installed (or you don't have permission to run it).
# Requires bash for shell arithmetic (and $(...) syntax).
#
# Examples:
#
# in_minutes 5 killall my_long_running_process
# in_minutes 5 echo "Files five minutes later:"; ls
# in_minutes 5 touch /done \
#     || echo -n "Could not touch the 'done' file at"; date

in_minutes() {
    MINUTES_TO_WAIT=$1
    shift # Now $@ contains the command to run.
    if [[ ! $MINUTES_TO_WAIT ]]; then
        echo "Usage: $0 MINUTES_TO_WAIT"
        return 1
    fi

    # Calculate when to stop in epoch seconds.
    START=$(date +%s)
    END=$(($START+$MINUTES_TO_WAIT*60))
    echo "Now is $START; then is $END"

    sleep 1
    while true; do
        NOW=$(date +%s)
        if [[ $NOW -ge $END ]]; then
            eval "$@"
            return $?
        else
            sleep 60
        fi
    done
}

########################## 
########################## Run daemons, if installed but not already runnning.
########################## Applies to my personal Mac only.
##########################

# Load functions for running several daemons.
DAEMON_HELPERS=~/.bashrc.d/daemons.bash
if [[ -f "$DAEMON_HELPERS" ]]; then
    source "$DAEMON_HELPERS"
    # run_gpg_agent_idempotently
    # run_ssh_agent_idempotently
fi

##########################
# Client-specific settings (not in git, as they may contain sensitive info).
##########################
CSFILE=~/.bashrcs.client-specific.d/Source-me.bash
if [[ -f "$CSFILE" ]]; then
    source "$CSFILE"
fi

if [[ -e /usr/local/etc/bash_completion.d/rclone ]]; then
    source /usr/local/etc/bash_completion.d/rclone
fi

alias emacs='emacs -nw'
function e() { emacs -nw $@ ;}

for OTHER_RC in ~/.bashrc--site-specific.bash ~/.bashrc--machine-specific.bash; do
    if [[ -e $OTHER_RC ]]; then
        source $OTHER_RC
    fi
done

alias pgb='/usr/local/bin/psql -p 6432 -h 127.0.0.1 pb'

# Dangerous.
#
# hu()
# {
#     brew services restart pgbouncer;
#     ps aux | head -1;
#     ps aux | grep -v grep | grep --color pgbouncer;
#     echo;
#     tail /usr/local/var/log/pgbouncer.log
# }

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if which brew > /dev/null 2>&1 && which gegrep > /dev/null 2>&1; then
    alias grep='gegrep --color=auto'
else
    alias grep='egrep --color=auto'
fi

#### ANSI color codes

BG_COLOR_MAGENTA='\[\e[45;m\]'
END_COLOR='\[\e\[-m\]'

show_time_remaining() {

    WEEKS_BETWEEN_PERFORMANCE_REVIEWS=4
    WEEK_OF_YEAR=$(date +%V)
    WEEK_OF_MONTHLY_REVIEW_CYCLE=$(echo "($WEEK_OF_YEAR - 2) % 4" | bc)
    WORK_WEEKS_REMAINING=$(echo "53 - $WEEK_OF_YEAR - 2" | bc)

    # WORKDAYS_BETWEEN_SPRINTS=10
    DAY_OF_YEAR=$(date +%j)
    DAY_OF_SPRINT=$(echo "($DAY_OF_YEAR) % 14" | bc)
    if [[ $DAY_OF_SPRINT = '0' ]]; then DAY_OF_SPRINT=14; fi

    if [[ $(hostname) != 'qweaver-mb' ]]; then
        # Meh, my ANSI escape sequences are messed up somehow
        # (quoting issues?)
        # Disable them for now.
        unset BG_COLOR_MAGENTA END_COLOR

        echo "${BG_COLOR_MAGENTA}Week ${WEEK_OF_MONTHLY_REVIEW_CYCLE}/${WEEKS_BETWEEN_PERFORMANCE_REVIEWS} of monthly review cycle${END_COLOR}."
        echo "${BG_COLOR_MAGENTA}Day ${DAY_OF_SPRINT}/14 of sprint${END_COLOR}."
        echo "${WORK_WEEKS_REMAINING} weeks left in the year${END_COLOR}."
        echo
    fi
}

# Make it so Ctrl-S and Ctrl-Q aren't interpreted as software flow control
# characters, messing up your terminal (see http://catern.com/posts/terminal_quirks.html).
# This also frees up Ctrl-Q for use as the tmux prefix key.
if (tty > /dev/null 2>&1); then # don't run when I do 'ssh MY_HOST MY_COMMAND'
    stty -ixon  > /dev/null 2>&1
    stty -ixoff > /dev/null 2>&1
fi

# if [[ ! -e ~/.tmux.conf ]]; then
#     echo 'Now run this command:'
#     echo 'sudo yum install -y emacs-nox pgcenter pv nc nmap tmux colordiff'
# fi

if which brew > /dev/null 2>&1 && which gawk > /dev/null 2>&1; then
    alias awk=gawk
fi

function job_running() {
    JENKINS_JOB_URL="$1/api/json?pretty=true"
    curl -k -L "$JENKINS_JOB_URL" 2>/dev/null | grep '"building" : ' | awk '{print $3}' | sed 's/,$//'
}

function job_notify() {
    JENKINS_JOB_URL="$1"
    
    echo "Job started before $(date)"
    until [[ $(job_running "$JENKINS_JOB_URL") == 'false' ]]; do
        echo '.'
        sleep 60
    done
    echo "Job finished at $(date)"
    ~/...sms-me "Jenkins job finished" > /dev/null 2>&1


    curl -L "$JENKINS_JOB_URL/consoleText"
}

function jn() { job_notify $@ ;}
