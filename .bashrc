#### Bash debugging settings

## Uncomment this line to run shellcheck, a mediocre and pedantic static analysis tool,
## on my Bash config files.

# shellcheck shell=bash

## Uncomment the following lines if you want to time each command in this file.
## You will also need to add 'time ' before every command in this file,
## because Bash doesn't have a good builtin time-tracing function.

# export TIMEFORMAT='    real %3R'; export PS4=''
# set -xT

## Also useful for timing:

# TIMEFORMAT='%3R'
# function time_source() {
#     echo -n "$1:  "
#     time source "$1"
#     echo
# }

# setx - run this function interactively to turn off the many
# prompt-related commands that automatically run every time I enter a
# command, then turn on tracing. This makes the trace much more
# uncluttered.

function setx() {
    unset PROMPT_COMMAND
    export PS1="\u@\h:\w\\$ "
    set -x
}

#### Basic settings

MY_OS="$(uname -s)"
MY_VIRTUALENV_DIR=~/.virtualenvs--python3

#### Settings related to editing this file itself

function cd1() { cd ~/src/1/qdw-dotfiles ;}

#### Homebrew basic settings

if [[ ! "$PATHS_SET_TIMESTAMP" ]]; then
    for HDIR in "/opt/homebrew" "/usr/local"; do
        if [[ -x "$HDIR/bin/brew" ]]; then
            set -a
            
            HOMEBREW_ROOT="$HDIR"
            HOMEBREW_MANPATH="$HDIR/share/man"
            MANPATH="$HOMEBREW_MANPATH:$MANPATH"

            HOMEBREW_BIN="$HDIR/bin"
            HOMEBREW_SBIN="$HDIR/sbin"
	    PATH="$HOMEBREW_BIN:$HOMEBREW_SBIN:$HOMEBREW_ROOT/bin/Contents/SharedSupport/bin:$PATH"
            PATH=$(echo $PATH | sed -e 's/[:]\{2,\}/:/g' -e 's/[:]$//')
	    
            set +a
            break
        fi
    done
fi

#### Bash settings

function source-if-exists() {
    if [[ $# -eq 1 ]] && [[ -f "$1" ]]; then
        source "$1"
    fi
}

# grbash - grep my Bash config files
function grbash() {
    if [[ $# -ne 1 ]]; then
        echo "grbash: error: wrong number of arguments. Usage: grbash REGEX"
    else
        grep -R "$@" ~/.bashrc ~/.bashrc.d ~/.bash_profile ~/.bash_login ~/.bash_logout ~/.site.bash.d
    fi
}

## Bash settings that shouldn't be checked into git
## (either because they contain secrets, or because of license issues)

source-if-exists ~/.bashrc.d/daemons.bash
source-if-exists ~/.site.bash.d/preamble.bash
source-if-exists ~/.secrets.bash

## Bash completion settings
## Dependencies:
##     - Homebrew itself
##     - Homebrewed bash >= 5 (set as your login shell)
##     - The following additional Homebrew formulas:
##         - bash-completion@2
##         - bash-git-prompt
##         - gh
##         - docker
##         - rclone
##         - openshift


set -a
GIT_PROMPT_THEME="Custom"
GIT_PROMPT_ONLY_IN_REPO=1
set +a


if [[ -r "$HOMEBREW_ROOT/etc/profile.d/bash_completion.sh" ]]; then
    export BASH_COMPLETION_DIR="$HOMEBREW_ROOT/etc/bash_completion.d"

    # comp - activate shell completion
    function comp() {
        sc() {
            for FILENAME in "$@"; do
                source "$BASH_COMPLETION_DIR/${FILENAME}"
            done
        }

        case "$1" in
            aws*)
                sc "aws_bash_completer"
                ;;
                
            brew)
                source "$HOMEBREW_ROOT/completions/bash/brew"
                ;;
            
            docker)
                sc docker \
                   docker-compose \
                   docker-machine-prompt.bash \
                   docker-machine-wrapper.bash \
                   docker-machine.bash
                ;;
        
            gh|github)
                sc "gh"
                ;;
        
            git)
                source "$HOMEBREW_ROOT/share/gitprompt.sh"    
                sc "gh" \
                   "git-completion.bash"
                ;;
            
            openshift|oc|kubernetes|kube|k8s)
                sc "oc" \
                   "kubecfg" \
                   "kubectl" \
                   "minikube"
                ;;

            *)
                sc "$1"
                ;;
        esac
    }

    function ag() {
        comp "git"
        export GIT_COMPLETION_ACTIVATED=1
    }
    
    # AWS command completion - pretty fast,
    # so it's OK to run this at shell startup time if you want
    function aw() {
        comp "aws_bash_completer"
    }
    
    GIT_COMPLETION_ACTIVATED=""
    export PROMPT_COMMAND=('if [[ -d "$PWD/.git" && ("$INSIDE_EMACS" || !"$GIT_COMPLETION_ACTIVATED") ]] ; then source "$HOMEBREW_ROOT/share/gitprompt.sh"; source "$BASH_COMPLETION_DIR/gh"; source "$BASH_COMPLETION_DIR/git-completion.bash"; export GIT_COMPLETION_ACTIVATED=1; fi')
    # export PROMPT_COMMAND=('if [[ ! "$GIT_COMPLETION_ACTIVATED" ]] && [[ -d "$PWD/.git" ]]; then source "$HOMEBREW_ROOT/share/gitprompt.sh"; source "$BASH_COMPLETION_DIR/gh"; source "$BASH_COMPLETION_DIR/git-completion.bash"; export GIT_COMPLETION_ACTIVATED=1; fi')
fi

#### Scheme Shell settings (keywords: Lisp, scsh)

if [[ -d "/opt/scsh" ]]; then
    SCSH_PATH="/opt/scsh/bin"
    LATEST_SCSH_VERSION="$(ls /opt/scsh/lib | sort --general-numeric-sort | tail -1)"
    SCSH_LDFLAGS="-L/opt/scsh/lib/scsh-${LATEST_SCSH_VERSION}"
fi    

#### Pager settings

if [[ "$INSIDE_EMACS" ]]; then
    # Emacs shell-mode is a dumb terminal, so use special pager settings there.
    export PAGER="cat"
    export AWS_PAGER="$PAGER"
    export PSQL_PAGER="$PAGER"
else
    # If we're a non-dumb terminal, then page fancily,
    # with the color and pagination options I like.
    export PAGER="command less --RAW-CONTROL-CHARS --quit-if-one-screen --no-init --mouse"
    if command -v jless > /dev/null; then
        export AWS_PAGER="jless"
    else
        export AWS_PAGER="$PAGER"
    fi
fi

function less() { $PAGER "$@" ;}

set -a
ACK_PAGER="$PAGER"
BAT_PAGER="less --RAW-CONTROL-CHARS --no-init --mouse"
GIT_PAGER="$PAGER"
MANPAGER="$PAGER"
set +a

#### Markdown settings

# export GLAMOUR_STYLE="Dark"
readme() {
    gh repo view
}

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub)
function bbat() {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
}

#### #FIXME settings: rewrite 'function bbat() {...}' to use 'gh' and GLAMOUR_STYLE, which should make the output look nicer

#### Linker settings

function ldd-app() {
    local APP_NAME EXECUTABLE_PATH

    APP_NAME="$1"

    for APP_DIR in /Applications ~/Applications; do
        EXECUTABLE_PATH="${APP_DIR}/${APP_NAME}.app/Contents/MacOS/${APP_NAME}"
        if [[ -f "$EXECUTABLE_PATH" ]]; then
            otool -L "$EXECUTABLE_PATH"
        fi
    done
}

function ldd() {
    if [[ "$MY_OS" = "Darwin" ]]; then
        if [[ "$1" =~ ^[[:upper:]] ]] || [[ "$1" =~ [.]app$ ]]; then
            ldd-app "$@" | $PAGER
        else
            otool -L "$@" | $PAGER
        fi
    else
        command ldd "$@"
    fi
}

# # Tell man to display pages in color (it uses the deprecated Termcap system)
# set -a
# # Section headings? (bold orange)
# LESS_TERMCAP_mb=$'\e[1;2;130m'
# # Program name? (bold purple)
# LESS_TERMCAP_md=$'\e[1;4;20m'
# # Option names (dull blue)
# LESS_TERMCAP_us=$'\e[0;9;25m'
# set +a

# Page differently in psql, where I don't want control characters interpreted
# as ANSI terminal colors, and I don't want to --quit-if-one-screen
export PSQL_PAGER="less --no-init"

#### AWS settings

function rds() {
    usage() {
        echo "Usage: rds [ls | list]"
    }

    if [[ $# -eq 0 ]]; then
        echo -n "Missing argument."; usage
    elif [[ $# -gt 1 ]]; then
        echo "Too many arguments."; usage
    elif [[ "$1" = "ls" ]] || [[ "$1" = "list" ]]; then
        shift
        rds_ls "$@"
    else
        echo "No such subcommand '$1'."; usage
    fi
}

# rds_ls ("RDS LiSt") - list RDS instances (highlighting the hostname portio nof the FQDN)
function rds_ls() {
    if [[ -n "$1" ]]; then
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*' \
            | grep "$1"
        
    else
        aws rds describe-db-instances --no-paginate \
            | jq --raw-output '.DBInstances[].Endpoint.Address' \
            | grep '^[^.]*'
    fi
}
function list-rds-instances() { rds_ls "$@" ;}

function list-secrets() { aws secretsmanager list-secrets ;}
function list-secret-ids() { list-secrets | jq --raw-output '.SecretList[].Name' | sort ;}


function get-secret() {
    aws secretsmanager get-secret-value --secret-id "$1" \
        | jq '.SecretString | fromjson'
}
function secret() { get-secret "$@" ;}

#### Editor settings

## Emacs settings

export EMACS_PLUS_VERSION="emacs-plus@28"

function brew-install-emacs-plus() {
    brew install \
         --formula \
         --build-from-source \
         "$EMACS_PLUS_VERSION" \
         --with-modern-doom3-icon \
         --with-imagemagick \
         --with-mailutils
}

function the_emacs_daemon_is_running() {
    local OUTPUT

    if [[ "$MY_OS" = "Darwin" ]]; then
        command ps auxwww | command grep -v grep | command grep "Emacs.app/Contents/MacOS/Emacs --fg-daemon" >/dev/null 2>&1
        OUTPUT=$?
    elif [[ "$MY_OS" =~ Linux ]]; then
        command ps auxwww | command grep "$USER" | command grep -v grep | command grep -E -- "emacs.*--(daemon|bg-daemon|fg-daemon)"
        OUTPUT=$?
    else
        OUTPUT="error: edit your ~/.bashrc and add support for OS '$MY_OS' to function the_emacs_daemon_is_running()"
    fi

    echo "$OUTPUT"
}

function print_instructions_for_starting_the_emacs_daemon() {
    if [[ "$MY_OS" = "Darwin" ]]; then
        echo "brew services start "$EMACS_PLUS_VERSION""
    elif [[ "$MY_OS" =~ Linux ]]; then
        echo "command emacs --bg-daemon"
    else
        echo "[error: edit your ~/.bashrc and add support for OS '$MY_OS' to function print_instructions_for_starting_the_emacs_daemon"
    fi
}

# If we're in a terminal session, start an emacsclient session inside that terminal
# (first starting the Emacs server, if it's not already running).
# 
# This looks just like a normal terminal emacs session, but starts faster
# (and can see buffers from other emacsclient sessions, but whatever).
function emacs() {
    # If, instead of running a command like crontab(1) that implicitly runs $VISUAL,
    # I directly type "emacs my-file", then...
    if [[ "$1" = "-c" && shift ]]   ||   [[ "$INSIDE_EMACS" ]]; then
        # If I'm inside an Emacs interactive-shell-mode buffer,
        # then edit my-file in the same same Emacs window.
        emacsclient --no-wait "$@"
    else
        local DAEMON_STATUS
        DAEMON_STATUS="$(the_emacs_daemon_is_running)"
        # echo "[DEBUG] in bash function emacs(), a call to bash function the_emacs_daemon_is_running() yielded output '$DAEMON_STATUS'"

        if [[ "$DAEMON_STATUS" = "0" ]]; then
            emacsclient --tty "$@"
        else
            echo "error: the emacs daemon is not running. Start it as follows:"
            print_instructions_for_starting_the_emacs_daemon
        fi
    fi
}

export VISUAL="emacs"
export EDITOR="$VISUAL"
export GIT_EDITOR="emacsclient"
export PSQL_EDITOR="emacs -nw"

#### Permission settings

umask 0022

# (This function depends on the third-party newtab() function;
# see ~/.bashrc.d/third-party.bash)
# 
# te ("TErminal window") - create a new Terminal.app window in the same directory I'm in when I run te.
function te() {
    newtab bash # -c "cd $(pwd); $@" #FIXME: get this -c thing to work
}

function nt() { newtab "$@" ;}

# Required for the corner case where you
# want to run emacs M-x shell in Terminal.app
# without getting error messages in Emacs.
# Not needed for other terminal emulators.
function update_terminal_cwd() {
    : # no-op
}

# Required to enable the Terminal.app feature
# where you can click on a directory name
# to open it in Finder... but I don't like
# that feature, so I'm comment it out for now.
#
# function update_terminal_cwd() {
#     # Identify the directory using a "file:" scheme URL,
#     # including the host name to disambiguate local vs.
#     # remote connections. Percent-escape spaces.
#     local SEARCH=' '
#     local REPLACE='%20'
#     local PWD_URL="file://$HOSTNAME${PWD//$SEARCH/$REPLACE}"
#     printf '\e]7;%s\a' "$PWD_URL" > /dev/null
# }

#### Terminal settings

export TERM=xterm-256color # used by tmux

# title STRING: set the window title of an xterm/iTerm2/rxvt window to STRING.
function title() { echo -ne "\033]2;" "$1" "\007" ;}

## iTerm2 settings

# See https://iterm2.com/documentation-captured-output.html
function cl() { printf "\e]1337;ClearCapturedOutput\e\\" ;}

function activate-iTerm2-integration() {
    # Enable "iTerm2 integration," that is, a bunch of fancy features
    # specific to the iTerm2 terminal emulator (example: click-to-download
    # over ssh plus lots more).
    #
    # It would slow down shell startup considerably if I automatically
    # sourced this code, so I've separated it into a function that you
    # can call explicitly when you want it.
    source ~/.iterm2_shell_integration.bash
}

## Bash prompt settings

# If this is my work laptop, just put "laptop" as the hostname,
# not the rather-verbose hostname assigned by DHCP.
#
# Actually, I'm disabling this; now that I'm all DevOps-y,
# I never get to ssh anywhere.
PS1_LOGO=""
if [[ -n "$LAPTOP_HOSTNAME" ]] && [[ $HOSTNAME = "$LAPTOP_HOSTNAME" ]]; then
    PS1_HOSTNAME="laptop"
else
    PS1_HOSTNAME="\\h"
fi

# Unicode logos as visual cues for different hosts/apps
if [[ "$INSIDE_EMACS" ]]; then
    PS1_LOGO=$'\u2130' # Cursive uppercase E
elif [[ -n "$LAPTOP_HOSTNAME" ]] && [[ $HOSTNAME = "$LAPTOP_HOSTNAME" ]]; then
    PS1_LOGO=$'\uF8FF' # Apple logo
fi

# Color the shell prompt ANSI blue (to distinguish commands from output).

# Remove some info, since I now live in a self-built DevOps prison sans ssh.
#export PS1="\\[\\e[34;1m\\]${PS1_LOGO}\\u@${PS1_HOSTNAME}:\\w\\$\\[\\e[0m\\] "
export PS1="\\[\\e[34;1m\\]\\w\\$\\[\\e[0m\\] "
export DEFAULT_PS1="$PS1"

# macOS: suppress nag message about Bash being deprecated in favor of zsh
export BASH_SILENCE_DEPRECATION_WARNING=1

## Bash history
set -a
# Add a timestamp to history(1) output, using strftime(3) formatting.
HISTTIMEFORMAT='  %Y-%m-%d_%H:%M:%S_%z    ' #FIXME: add milliseconds?
# Keep all history forever
HISTFILESIZE=-1
HISTSIZE=-1
set +a

#### Homebrew settings

set -a
HOMEBREW_BAT=1
HOMEBREW_BAT_CONFIG_PATH=~/.config/bat/homebrew.config

HOMEBREW_NO_ANALYTICS=1
HOMEBREW_NO_ENV_HINTS=1
set +a

function brew() {
    if [[ "$1" = 'help' ]] || [[ ${!#} = '--help' ]]; then
        command brew "$@" | $PAGER
    else
        command brew "$@"
    fi
}

function brew-deps-all() {
    brew deps --tree --install | $PAGER
}

function brew-all-deps() { brew-deps-all "$@" ;}

function brew-leaves() {
    brew-deps-all | command ggrep -E '^[:alpha]' | sort | uniq | $PAGER
}

function brew-leaf() {
    local LEAF_NUMBER
    if [[ $# -eq 0 ]]; then
        LEAF_NUMBER=1
    elif [[ $# -eq 1 ]]; then
        LEAF_NUMBER="$1";
    else
        echo "brew-leaf: error: wrong number of arguments."
        echo "Usage: brew-leaf [NUMBER] # 1-based"
        return
    fi

    brew-leaves | head "-${LEAF_NUMBER}" | tail -1
}

function brew-next-leaf() {
    local LEAF_FILE=~/.brew-next-leaf
    
    local LEAF_NUM
    if [[ -f "$LEAF_FILE" ]]; then
        LEAF_NUM=$(cat "$LEAF_FILE")
    else
        LEAF_NUM=1
    fi

        
    if brew-leaf "$LEAF_NUM"; then
        (( LEAF_NUM++ ))
        echo "$LEAF_NUM" > "$LEAF_FILE"
    fi
}
function bnl()  { brew-next-leaf "$@" ;}
function leaf() { brew-next-leaf "$@" ;}


function bi() { brew install "$@" ;}
function be() { brew edit "$@" ;}
function bl() { brew list -1 "$@" ;}
function bo() { brew update; brew outdated ;}
# function bo() { brew doctor; brew update && brew outdated ;}
function bs() { brew search "$@" ;}
function bser() { brew services "$@" ;}
function bu() { brew update && brew upgrade --formula && brew outdated ;}
function buc() {
    (brew update && brew upgrade --formula) \
        && (brew cleanup; brew doctor)
}

function brew-lock() {
    local HOMEBREW_LOCK_DIR USAGE
    HOMEBREW_LOCK_DIR="$HOMEBREW_ROOT/var/homebrew/locks"
    USAGE="Usage: brew-lock [--remove-all]"

    if [[ $# -eq 0 ]]; then
        ls "$HOMEBREW_LOCK_DIR"
    elif [[ $# -eq 1 ]]; then
        if [[ $1 = "--remove-all" ]]; then
            brew cleanup
        else
            echo "brew-lock: error: unrecognized argument '$1'. $USAGE"
        fi
    else
        echo "brew-lock: error: too many arguments. $USAGE"        
    fi
}

# brew-n-deps PACKAGE_NAME - print the number of packages
# that depend on PACKAGE_NAME
function _brew-n-deps() {
    PACKAGE_NAME="$1"
    if ! brew ls --formula "$PACKAGE_NAME" >/dev/null 2>&1; then
        echo -1
    else
        RAW_COUNT=$(brew deps --tree --installed | grep -c "$PACKAGE_NAME")
        N_DEPENDERS=$(( RAW_COUNT - 1 ))
        echo $N_DEPENDERS
    fi
}

# Just a verbose wrapper to _brew-n-deps, which see
function brew-n-deps() {
    PACKAGE_NAME="$1"
    N_DEPENDERS=$(_brew-n-deps "$PACKAGE_NAME")
    if [[ "$N_DEPENDERS" = -1 ]]; then
        echo "Package '$PACKAGE_NAME' is installed"
    elif [[ "$N_DEPENDERS" = 0 ]]; then
        echo "Package '$PACKAGE_NAME' has no dependencies"
    elif [[ "$N_DEPENDERS" = 1 ]]; then
        echo "Package '$PACKAGE_NAME' has $N_DEPENDERS dependency"
    else
        echo "Package '$PACKAGE_NAME' has $N_DEPENDERS dependencies"
    fi
}

## Homebrewed alternatives to bundled system utilities
MY_LS_BASE_OPTIONS="--color=always -1AF"
if test -e "$HOMEBREW_BIN/gls"; then
    export MY_LS=gls
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS --group-directories-first"
else
    export MY_LS=ls
    export MY_LS_OPTIONS="$MY_LS_BASE_OPTIONS"
fi

# Use a function rather than adding $HOMEBREW_BIN to my path;
# that way I can easily get the bundled macOS version of the function
# by doing "command tail" (or whatever)
# for MY_FORMULA in \
    # curl \
    # lsof \
    # man \
    # ssh \
    # ssh-add \
    # ssh-add-all \
    # ssh-agent \
    # ssh-copy-id \
    # ssh-keygen \
    # ssh-keyscan; do
#     if [[ -e "$HOMEBREW_BIN/MY_FORMULA" ]]; then
#         eval "function $MY_FORMULA() { $HOMEBREW_BIN/$MY_FORMULA \"\$@\" ;}"
#     fi
# done

# for GNU_FORMULA in cut dircolors find grep roff sed sort stat tac tail; do
#     if [[ -e "$HOMEBREW_BIN/g${GNU_FORMULA}" ]]; then
#         eval "function $GNU_FORMULA() { $HOMEBREW_BIN/g${GNU_FORMULA} \"\$@\" ;}"
#     fi
# done
    
    
if test -e "$HOMEBREW_BIN/curl"; then function curl() { "$HOMEBREW_BIN/curl" "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gcut"; then function cut() { gcut "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gfind"; then function find() { gfind "$@" ;}; fi
if test -e "$HOMEBREW_BIN/ggrep"; then export MY_GREP="ggrep -E"; else export MY_GREP=egrep; fi
if test -e "$HOMEBREW_BIN/gfgrep"; then export MY_FGREP=gfgrep; else export MY_FGREP=fgrep; fi
if test -e "$HOMEBREW_BIN/man"; then export MY_FGREP=man; else export MY_FGREP=fgrep; fi    
if test -e "$HOMEBREW_BIN/gsed"; then function sed() { gsed "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gsort"; then function sort() { gsort "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gstat"; then function stat() { gstat "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gtac"; then function tac() { gtac "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gtail"; then function tail() { gtail "$@" ;}; fi
if test -e "$HOMEBREW_BIN/gdircolors"; then function dircolors() { gdircolors "$@" ;}; fi
if test -e "$HOMEBREW_BIN/groff"; then function roff() { groff "$@" ;}; fi
if test -e "$HOMEBREW_BIN/lsof"; then function lsof() { "$HOMEBREW_BIN/lsof" "$@" ;}; fi

#### Other macOS-specific settings

function mac-globalize-fonts {
    local TERMINAL_APP_DIR
    if [[ -d "/System/Applications/Utilities/Terminal.app" ]]; then
        TERMINAL_APP_DIR="/System/Applications/Utilities/Terminal.app"
    else
        TERMINAL_APP_DIR="/Applications/Utilities/Terminal.app"
    fi

    sudo cp -R \
        "$TERMINAL_APP_DIR/Contents/Resources/Fonts" \
        "/Library/Fonts/"
}

function mac-disable-font-smoothing() {
    defaults write -g AppleFontSmoothing -int 0
    defaults write CGFontRenderingFontSmoothingDisabled -INT 1
}

function cdd() {
    local USAGE="Usage: cdd PATH"

    if [[ $# -eq 0 ]]; then
        echo "cdd: error: missing argument. $USAGE"
    elif
        [[ $# -gt 1 ]]; then
        echo "cdd: error: too many arguments. $USAGE"
    else
        cd "$(dirname "$1")"
    fi
}

function mac-do-not-switch-to--desktop-when-switching-to-app() { 
    sudo defaults write com.apple.dock workspaces-auto-swoosh -bool NO
    sudo killall Dock
}

function mac-enable-cmd-ctrl-click-to-drag-windows() {
    defaults write -g NSWindowShouldDragOnGesture -bool true
}

#### Settings for bat, a colorizing cat(1) that understands many file formats

export BAT_THEME='Solarized (dark)'
function bat() {
    command bat \
        --color always \
        --decorations='auto' \
        --style=rule,snip \
        --wrap=auto \
        "$@" \
      | $PAGER
}

# bh ("Bat Head") FILENAME NUMBER: use color bat to show a file's first n lines
function bh() {
    local FILENAME="$1"
    local NUMBER="$2"
    
    bat --line-range ":${NUMBER}" "$FILENAME"
}

# bbat - alternative pager hack for Markdown files with excessive inline hyperlinks that make them unreadable (for example, most README.md files on GitHub)
function bbat () {
    local TMPFILE
    
    TMPFILE=$(gmktemp --suffix .html /tmp/bbat.XXXXXX) \
        && pandoc --from markdown --to html --reference-links "$@" > "$TMPFILE" \
        && elinks "$TMPFILE"
}

# cdy ("CD sYmbolic link") - cd to the directory that a symlink points to (or, if it points to a non-directory file, cd to that file's parent dir).
function cdy() {
    local TARGET
    TARGET=$(readlink -- "$1")

    if [[ -d "$TARGET" ]]; then
        cd "$TARGET"
    else
        PARENT=$(echo "$TARGET" | sed 's/\(.*\)[/].*/\1/')
        cd "$PARENT"
    fi
}

# Report symlinks as symlinks
function file() { command file --no-dereference "$@" ;}

# path - given a relative path, print its relative path. Does not dereference symlinks. Does not work on absolute paths; hence does not work for tilde expansion.
function path() {
    local REL_PATH ABS_PATH
    REL_PATH="$1"
    ABS_PATH="$(pwd)/$REL_PATH"

    if [[ ! -e "$ABS_PATH" ]]; then
        echo "Error: file '$ABS_PATH' does not exist"
    else
        echo "$ABS_PATH" | tee >(pbcopy)
    fi
}

# qp ("qdw-dotfiles Push")
function qp() { cdq && git add . && git commit && git push && cd - ;}

#### ls settings

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=39;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.swp=00;90:*.tmp=00;90:*.dpkg-dist=00;90:*.dpkg-old=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:'

function ls() {
    command $MY_LS $MY_LS_OPTIONS "$@" | $PAGER
}

function lh() {
    command $MY_LS $LS_BASE_OPTIONS -t "$@" | head -5
}

#### diff settings

function dcc() { colordiff -u "$@" | $PAGER ;}

# Color legend:
# ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
# de - deleted lines (31 is SGR red, which shows as orange in my terminal)
# hd - header        (35 is SGR magenta, which shows as magenta in my term)
# ln - line numbers  (34 is SGR blue, which shows as light pink in my term)
# rs - undocumented, but the default seems fine
# 
function diff() {
    command diff \
        -u \
        --color=always \
        --palette="ad=36:de=31:hd=35:ln=34:rs=0" \
        "$@" \
      | $PAGER
}

# function diff() { difft --background dark --color always --display side-by-side --missing-as-empty --skip-unchanged "$@" | $PAGER ;}

#### FTP settings

set -a
FTP_LOGIN=ftp
FTP_PASSIVE_MODE=yes
FTP_PASSWORD=""
set +a

#### Git setttings

## General Git settings (not specific to GitHub)

export GIT_SHELL_PATH="$HOMEBREW_BIN/bash"

# gcd ("Git CD") - cd to a directory, relative to the root dir of this repo.
function gcd() {
    # gcd ("Git CD") - cd to a directory, relative to the root dir of this repo
    # For example, if you're in directory ~/src/my-project/javascript then
    # 'gcd python' will change the cwd to ~/src/my-project/python

    local DESIRED_RELATIVE_DIR
    DESIRED_RELATIVE_DIR=${1:.}

    cd "$(git rev-parse --show-toplevel)/$DESIRED_RELATIVE_DIR"
}


function git-backup {
    local REPO_NAME ORIG_DIR EXIT_STATUS
    if [[ $# -ne 1 ]]; then
        echo "git-backup: error: missing argument. Usage: git-backup REPO_NAME"
    else
        REPO_NAME="$1"
        ORIG_DIR=$(pwd)
        cd ~/src \
        && rsync -qa \
            "$REPO_NAME" \
            "$HOME/git-backups/${REPO_NAME}-$(date +%Y-%m-%d-%H%M%S%z)"

        cd "$ORIG_DIR"
    fi
}


function get-main-branch-name() {
    local MAIN_OR_MASTER
    
    MAIN_OR_MASTER="$(command git branch --remotes | command ggrep --perl-regexp --only-matching '  origin/HEAD -> origin/(?=master|main)\K.*$' | sort | head -1)"
    echo "$MAIN_OR_MASTER"
}


# gb (Git Branch) - create a new branch based on the main one, and check it out
function gb() {
    local NEW_BRANCH_NAME MAIN

    if [[ $# -eq 0 ]]; then
        git branch --show-current
    elif [[ $# -eq 1 ]]; then
        NEW_BRANCH_NAME="$1"
        MAIN=$(get-main-branch-name) \
            && git co "$MAIN" \
            && git pull --ff-only \
            && git checkout -b "$NEW_BRANCH_NAME" "$MAIN" \
                | command grep -v -E 'remote:' \
            && git push --set-upstream origin # --quiet
    else
        echo "gb: error: invalid number of arguments '$#'. Usage: gb | gb NEW_BRANCH_NAME"
    fi
}

# bc (Branch Create) - alias for gb, mnemonic because "b c" is the Magit
# command to create and switch to a branch.
# Shadows the crappy Unix built-in calculator bc(1).
function bc() {
    gb "$@"
}

# gbs (Git BrancheS) - list (local) git branches
function gbs() { git branch ;}

# gco (Git CheckOut) - what it says on the tin
function gco { git checkout "$@" ;}

# gm (Git Main) - check out the main branch (depends on an alias in my ~/.gitconfig)
function gm() { git main "$@" ;}

# gu (Git URL) - run this in any dir inside a Git working copy to show the URL
function gurl() {
    local ORIG_DIR
    ORIG_DIR=$(pwd)
    gcd \
        && command grep url .git/config | sed -e 's/^[ \t]*url[ ][=][ ]//'
    cd "$ORIG_DIR"
}

## GitHub-specific Git settings

# gist - create a gist
function gist() {
    if [[ $# -eq 0 ]]; then
        open "https://gist.github.com/$GITHUB_USERNAME"
    elif [[ $# -gt 2 ]]; then
        echo "gist: error: wrong number of arguments. Usage: gist SOURCE_FILENAME"
    else
        local REALPATH DESCRIPTION
        REALPATH="$(realpath "$1")"
        if [[ $# -eq 2 ]]; then
            DESCRIPTION="$2"
        else
            DESCRIPTION="$REALPATH"
        fi
        
        gh gist create --desc "$DESCRIPTION" "$REALPATH"
    fi
}

# gr (Git Repo) - run this in any dir inside a Git working copy to show the repo name
function gr() {
    local REPO_URL RETVAL

    REPO_URL="$(gu)"
    if [[ "$REPO_URL" =~ ^git[@]github.com[:]$GITHUB_ORG[/](.*) ]]; then
        RETVAL=${BASH_REMATCH[1]}
        echo "$RETVAL"
    else
        echo "gr: error: couldn't parse URL '$REPO_URL'" 2> /dev/null
    fi
}
# function git-repo() { gr "$@" ;}

# gtr (Git Team and Repo) - run this in any dir inside a working copy of a GitHub repo to show the team name plus repo name in GitHub notation, e.g., myorg/myrepo
function gtr() {
    local REPO_URL RETVAL
    REPO_URL="$(gu)"
    if [[ "$REPO_URL" =~ ^git[@]github.com[:](.*[/].*) ]]; then
        RETVAL=${BASH_REMATCH[1]}
        echo "$RETVAL"
    else
        echo "gtr: error: couldn't parse URL '$REPO_URL'" 2> /dev/null
    fi
}   

# gline (Git Line) - open a file in GitHub's web UI, at a given line
function gline() {
    local FILENAME_AND_LINE  USAGE  BRANCH
    USAGE="Usage: gline FILENAME:LINENO"
    BRANCH="$(gb)"
    
    if [[ $# -eq 1 ]]; then
        FILENAME_AND_LINE="$1"
    elif [[ $# -eq 2 ]]; then
        FILENAME_AND_LINE="${1}:${2}"
    else
        echo "gline: error: wrong number of arguments ($#). $USAGE"
    fi
    
    if [[ -n "$FILENAME_AND_LINE" ]]; then
        gh browse --branch "$BRANCH" "$FILENAME_AND_LINE"
    fi
}

function branch() {
    local BRANCH_NAME
    
    if [[ $# -eq 0 ]]; then
        BRANCH_NAME=$(gb)
    elif
        [[ $# -eq 1 ]]; then
        BRANCH_NAME="$1"
    else
        echo "branch: error: too many arguments. Usage: branch [BRANCH_NAME" > /dev/stderr
    fi
    
    gh browse --branch "$BRANCH_NAME"
}

function readme() {
    gh repo view
}

function ga()  { git add "$@" ;}
function gac() { git commit -a "$@" ;}
function gas() { git stash "$@" ;}
function gcm() { git commit -m "$@" ;}
function gd()  { git diff "$@" ;}
function gdo() { git diff origin HEAD ;}

function gl() {
    local N_COMMITS

    if [[ $# -eq 0 ]]; then
        N_COMMITS=3
    elif [[ $# -eq 1 ]]; then
        N_COMMITS="$1"
        shift 1
    elif [[ $# -eq 2 ]] && [[ $1 = "-n" ]]; then
        N_COMMITS="$2"
        shift 2
    fi

    git log -n "$N_COMMITS" "$@"
}

function gn()  { git diff --name-only "$@" ;}
function gs()  { git status "$@" ;}


# gi ("git init") - go through the multi-command dance to create a local git repo
function gi() {
    if git-rev-parse --is-inside-work-tree; then
        echo "gi: error: $(pwd) is already under git control"
    else
        git init \
            && git add . \
            && git commit -a -m 'Initial commit' \
            && git branch -m master main 2>/dev/null
    fi
}

## GitHub-specific settings

# gcl (Git CLone) - quickly clone a repo belonging to my organization
function gcl() {
    REPO="$1"
    cd ~/src \
        && git clone "git@github.com:${GITHUB_ORG}/${REPO}"
}

# repos - multitool for listing/searching GitHub repositories 
function repos() {
    PATTERN="$1"
    
    # With no argument, print all repos in my $GITHUB_ORG
    if [[ ! "$PATTERN" ]]; then
        PATTERN="." # match anything
    fi
    
    # Search in various places for repos matching PATTERN,
    # stopping at the first place where we find a match.
    set -o pipefail
    {
        echo "    Searching $GITHUB_ORG repos";
        gh repo list --limit 1000 "$GITHUB_ORG" | grep "$PATTERN"
    } \
    || \
    {
        echo "    Searching personal repos"
        gh repo list --limit 1000 | grep "$PATTERN"
    } \
    || \
    {
        echo "    Opening a browser and searching all repos on github.com"
        open "https://github.com/search?type=repositories&q=${PATTERN}"
    }
}

# repo - open a named GitHub named repo in a web browser
function repo() {
    REPO_NAME="$1"
    if [[ -z "$REPO_NAME" ]]; then
       gh repo view --web
    elif [[ "$REPO_NAME" = "list" ]]; then
        repos "."
    else
        # Try opening a repo of this exact name,
        # first in my org and then among repos owned by by GitHub username.
        #
        # If that fails, then do some fuzzy searching.
        if     ! gh repo view --web "$GITHUB_ORG/$REPO_NAME" >/dev/null 2>&1 \
            && ! gh repo view --web "$REPO_NAME" >/dev/null 2>&1; then
            
            echo "Couldn't find a $GITHUB_ORG repo named '$REPO_NAME'"
            echo "Searching for similarly-named repos"
            repos "$REPO_NAME"
        fi
    fi
}

# PR (Pull Request) - manipulate GitHub pull requests
# Depends on the official GitHub CLI tool, 'gh'.
# Usage:
#
#     pr        # Show my pull requests (for the repo whose directory I'm in).
#     pr v      # Same thing.
#     pr o      # Same thing.
#     pr view   # Same thing.
#
#     pr cr     # Create a PR, pushing first if necessary.
#     pr create # Same thing.
#
#     pr cro    # Create a PR as above, and open it in a browser
#               #   so that I can watch its deploy scripts run.
#     pr crv    # Same thing.
#
# PR (Pull Request) - manipulate GitHub pull requests
function pr() {
    push_it() {
        # Push to a remote branch (creating if necessary)
        local BRANCH
        BRANCH="$(git branch --show-current)"
        git push --set-upstream origin "$BRANCH" --quiet
    }

    create_pr() {
        local GIT_FETCH_STDOUTERR=$(git fetch 2>&1)
        if [[ "$GIT_FETCH_STDOUTERR" ]]; then
            # If there's stuff to merge, cowardly bail
            # and make the user merge it manually
            echo "$GIT_FETCH_STDOUTERR"
        else
            push_it
            
            local URL=""
            if [[ -n "$PR_TITLE" ]]; then
                URL=$(gh pr create --title "$PR_TITLE" | tail -1)
            else
                URL=$(gh pr create --fill | tail -1)
            fi
            
            open "$URL"
        fi
    }
    
    main() {
        local PR_NUMBER_REGEX='^[[:digit:]]+$'
        
        if [[ "$1" = "--list" ]]; then
            shift
            gh pr list "$@"
        elif [[ "$1" =~ $PR_NUMBER_REGEX ]]; then
            gh pr view --web "$1"
        else
            URL="$(create_pr "$@")"
            open "$URL"
        fi
    }

    main "$@"
}

function prs() {
    pr --list
}

# pru ("Pull Request URL") - print the URL of the latest pull request on this branch.
# Also copy it to the clipboard.
function pru() {
    gh pr view | command grep --color=never --only-matching 'https://.*' | tee >(pbcopy)
}

function pulls() {
    open "https://github.com/pulls"
}

#### grep settings

export GREP_COLORS="fn=01;36"

GREP_EXCLUDE_DIRS="--exclude-dir=.git --exclude-dir .github --exclude-dir node_modules --exclude .gitignore --exclude .npmignore"
function grep() { $MY_GREP -I --color=always $GREP_EXCLUDE_DIRS "$@" | $PAGER;}
function fgrep() { $MY_FGREP -I --color=always $GREP_EXCLUDE_DIRS "$@" | $PAGER;}
function g() { $MY_GREP -R -I --color=always $GREP_EXCLUDE_DIRS "$@" | $PAGER ;}
function gbr() { $MY_GREP -R -I --color=always $GREP_EXCLUDE_DIRS "$@" ~/.bashr* ~/.*profile* ~/.site.bash* ;}

function gfd() {
    local REGEX USAGE
    USAGE="Usage: gfd REGEX"
    
    if [[ $# -eq 0 ]]; then
	echo "gfd: error: missing argument. $USAGE"
    elif [[ $# -eq 1 ]]; then
	REGEX="$1"
	(g "$REGEX"; fd "$REGEX")
    else
        echo "gfd: error: wrong number of arguments. $USAGE"
    fi
}

# count: recursively grep the specified dirs (or '.', if no dirs specified) for a pattern, then print the number of times that pattern occurs in each matching file. Don't show files with zero matches.
function count() { 
    command grep -Rc $GREP_EXCLUDE_DIRS "$@" | command grep -v ':0$' | sed -e 's/\(.*\):\(.*\)/\2 \1/' | sort --reverse --general-numeric-sort | grep '.* ' | $PAGER
}

# occur: recursively grep the specified dirs (or '.', if no dirs specified) and print all filenames with at least one match.
function occur() {
    grep -Rl $GREP_EXCLUDE_DIRS "$@"
}

#### fd settings (fd is a faster and better replacement for find(1)

function fdd() {
    command fd \
        --exclude .git \
        --exclude .github \
        --follow \
        --full-path \
        --hidden \
        --no-ignore-vcs \
        "$@"
}

function fd() {
    fdd --color=always "$@" | $PAGER
}

function fdgrep() {
    local FILENAME_REGEX="$1"; shift
    local GREP_REGEX="${!#}"; set -- "${@:1:$(($#-1))}"
    
    local FILES=$(fdd "$FILENAME_REGEX")
    if [[ "$FILES" ]]; then
        grep -Ri "$@" "$GREP_REGEX" $FILES
    fi
}

function fdoccur() {
    local FILENAME_REGEX GREP_REGEX FILES

    local FILENAME_REGEX="$1"
    shift
    local GREP_REGEX="$1"
    shift
    
    local FILES=$(fdd "$FILENAME_REGEX")
    if [[ "$FILES" ]]; then
        # Breaks if a filename contains whitespace; what do?
        occur "$GREP_REGEX" "$@" $FILES
    fi
}

#### Kubernetes (and OpenShift) settings

function oc() {
    if [[ "$1" = "login" ]] && [[ $# -eq 1 ]]; then
        local AUTH_URL
        AUTH_URL=$(command oc login | sed s'/.*\(https.*\)/\1/')
        if [[ $? -eq 0 ]]; then
            open "$AUTH_URL"
        fi
    else
        command oc "$@" | $PAGER    
    fi
}

# Show all images in use in the current cluster; code ganked from https://kubernetes.io/docs/tasks/access-application-cluster/list-all-running-container-images/
function kube-ls-images() { 
    kubectl get pods --all-namespaces \
                      -o jsonpath="{.items[*].spec.containers[*].image}" \
        | tr -s '[:space:]' '\n' | sort | uniq -c
}

# If I'm going to make the same typo all the time, I might as well autocorrect it
function lube-aws() { kube-aws "$@" ;}
function lube-ls-images() { kube-ls-images "$@" ;}
function lubectl() { kubectl "$@" ;}
function lubecfg() { kubecfg "$@" ;}
function minilube() { minikube "$@" ;}

#### Perl settings

# Make Module::Install auto-follow dependencies. cpanm sets this by default,
# but sometimes you can't use cpanm (e.g., you're developing a Catalyst app,
# which means using MakeMaker directly).
export PERL_MM_USE_DEFAULT=1

# pb: because typing 'perlbrew' is too much work.
function pb() {
    # Don't put this in PATH, because that would interfere with the system Perl.
    ~/perl5/perlbrew/bin/perlbrew "$@"
}

#### Playdate (handheld indie console) game dev settings

function pd_encode() {
    local INPUT OUTPUT

    if ! [[ $# -eq 1 ]] || [[ $# -eq 2 ]]; then
        echo "Wrong number of arguments. Usage: pd_encode INPUT_FILENAME [OUTPUT_FILENAME"
    else
        INPUT="$1"
        OUTPUT=${2:-$INPUT.pd.wa}
        set -x
        ffmpeg -i "$INPUT" -acodec adpcm_ima_wav "$OUTPUT"
        set +x
    fi
}

#### Data risotto spatulae

function csview() { command csview "$@" | $PAGER ;}

#### PostgreSQL settings
####
#### (for additional PostgreSQL settings, see the following files):
####     ~/.psqlrc*
####     ~/.pg_service
####     ~/.pgpass

function pg() {
    local VERSION ENV_VAR_COMMAND
    if [[ $# -lt 2 ]]; then
        echo "pg: error: missing argument. Usage: pg POSTGRESQL_VERSION EXECUTABLE_NAME arguments"
        echo "    (for instance, pg 14 psql -c 'SELECT 1')"
    else
        VERSION="$1";
        EXECUTABLE_NAME="$2"
        shift
        shift
        if [[ "$EXECUTABLE_NAME" = "psql" ]] && [[ $# -eq 0 ]]; then
            ENV_VAR_COMMAND="export PGSERVICE=pg${VERSION}"
        fi
        ( $ENV_VAR_COMMAND; $HOMEBREW_ROOT/Cellar/postgresql@${VERSION}/*/bin/$EXECUTABLE_NAME "$@" )
    fi
}

# replication-role -  say whether this is a primary or a replica (suitable for inclusion in your shell and/or PSQL prompt)
function replication_role()
{
    if ! which psql; then
        echo ''
    else
        INR=$(psql -tc 'select pg_is_in_recovery()')
        if [[ $INR == ' f' ]]; then
            echo '*** PRIMARY ***'
        elif [[ $INR == ' t' ]]; then
            echo 'replica'
        else
            echo '[ERROR: COULD NOT DETERMINE REPLICATION ROLE]'
        fi
    fi
}

# View or edit my ~/.pg_service.conf
# Usage:
#
#     # To open the file in a text editor:
#     ser
#
#     # To show a [section] in the file that matches a regex:
#     ser monolith
#     # ^ prints all sections with section headings matching '[.*monolith.*]'
# 
# ser - display sections of my ~/.pg_service.conf, or the whole thing
function ser() {
    local CONF=~/.pg_service.conf

    if [[ $# -eq 0 ]]; then
        $VISUAL "$CONF"
    else
        para "\\[.*$1.*\\]" "$CONF"
    fi
}

#### IRL settings

## PurpleAir air quality monitor settings

function air() {
    set -uo pipefail
    local RESPONSE_FILE
    RESPONSE_FILE=$(mktemp "$HOME/tmp/air.XXXX")

    curl -L 'https://www.purpleair.com/json?show=119745&key=NPLQPTWKC54229MU' \
         > "$RESPONSE_FILE" \
         2> /dev/null
         
    if [[ $? -ne 0 ]]; then    
        echo "Problem contacting the PurpleAir API server; maybe rate-limiting?"
        echo "Try waiting ten minutes and then re-running."
    else
        local PM25 STATS TSLM NUMBER_OF_MINUTES INFLECTED

        PM25=$(jq --raw-output '.results[0]."PM2_5Value"' < "$RESPONSE_FILE")
        STATS=$(jq --raw-output '.results[0].Stats' < "$RESPONSE_FILE")
        TSLM=$(echo "$STATS" | jq --raw-output '.timeSinceModified')

        # Convert milliseconds to minutes
        NUMBER_OF_MINUTES=$(( TSLM / 60 ))
        
        if [[ "$NUMBER_OF_MINUTES" -eq 1 ]]; then
            INFLECTED="minute"
        else
            INFLECTED="minutes"
        fi

        echo "PM 2.5 is ${PM25}ppm (as of $NUMBER_OF_MINUTES $INFLECTED ago)"
    fi
    
    set +uo pipefail
}

## Coffee-bean-ordering settings

function coffee() {
    open -a Firefox "https://store.awakencafe.com/s/customer-accounts"
}

#### Aranet4 air quality monitoring settings

function co2 () {
    "$MY_VIRTUALENV_DIR/claranet4/bin/claranet4" read "$MY_ARANET4" 2> /dev/null | jq ".co2"
}

function co2-verbose() {
    echo "CO2 was $(co2) ppm at $(date '+%I:%M %P') on $(d)"
}

#### Python settings

# Homebrew stable Python version

set -a
PYTHON_INTERPRETER_PATH="/usr/local/bin"
PYTHON_PIPX_PATH=~/.local/bin
set +a

python() { python3 "$@" ;}
pip() { pip3 "$@" ;}

function va() {
    local VENV_NAME
    if [[ $# -ne 1 ]]; then
        echo "va: error: wrong number of arguments. Usage: va VIRTUALENV_TO_ACTIVATE"
    else
        VENV_NAME="$1"
        source "$MY_VIRTUALENV_DIR/$VENV_NAME/bin/activate"
        pip install --upgrade pip
    fi
}

function mkvirtualenv() {
    mkdir -p "$MY_VIRTUALENV_DIR"
    
    NEW_VIRTUALENV_NAME="$1"
    shift
    if [[ ! $NEW_VIRTUALENV_NAME ]]; then
        echo "Missing argument. Usage: mkvirtualenv [OPTIONS] NEW_VIRTUALENV_NAME"
    else
        NEW_VIRTUALENV_PATH="$MY_VIRTUALENV_DIR/$NEW_VIRTUALENV_NAME"
        python3 -m venv "$NEW_VIRTUALENV_PATH" \
            && va "$NEW_VIRTUALENV_NAME"
    fi
}

#### Tor settings

function tor-curl() { curl --socks4a 127.0.0.1:9050 "$@" ;}
function torcurl() { tor-curl "$@" ;}

#### TypeScript settings/Deno settings

# (You can't do this with a shebang, hence the need for a function):
# dr ("Deno Run") - run some code via the Deno runtime.
function dr() { deno run --quiet "$@" ;}

# dl ("Deno with Local permissions") - run some code in the Deno runtime, and allow the code to access the network and read the local filesystem, without prompting me for permission.
function dl() { deno run --quiet --allow-net --allow-read "$@" ;}

function dc() { deno eval --check "$@" ;}

# dcr - run Deno, but first do type-checking (no longer the default as of Deno v1.23)
function dcr() { deno run --check "$@" ;}

#### Rust settings

export RUST_PATH=~/.cargo/bin

#### Ruby settings

# Use homebrewed Ruby if installed
if [[ -d "$HOMEBREW_ROOT/opt/ruby" ]]; then
    set -a
    RUBYOPT=rubygems
    RUBY_PATH=/usr/local/opt/ruby/bin
    RUBY_LDFLAGS='-L/usr/local/opt/ruby/lib'
    RUBY_CPPFLAGS='-I/usr/local/opt/ruby/include'
    set +a
fi

# gemi GEM1 GEM2 ...: install (Ruby) gems as I think they should be installed.
function gemi() {
    sudo gem install --remote \
                     --rdoc \
                     --ri \
                     "$@"
}

#### Miscellaneous shell functions

# Change this terminal's background to the hex triplet passed in.
function bghex() {
    # Example: 'bghex ff0000' to use a red background.
    printf '\e]11;%s\a' "#${1}"
}

function ips() { /sbin/ifconfig -a | grep 'inet ' | grep -v 'inet6 ' ;}
function ipv6() { /sbin/ifconfig -a | grep 'inet6 ' ;}

# flast - print the first and last lines of a file
function flast() {
    local FILENAME
    FILENAME="$1"

    head -1 "$FILENAME" | awk '{ print $1, $2; }'
    tail -1 "$FILENAME" | awk '{ print $1, $2; }'
}

# line N FILE - show line N of file FILE
function line() {
    local LINE_NUMBER FILENAME
    LINE_NUMBER="$1"
    FILENAME="$2"

    head "-${LINE_NUMBER}" "$FILENAME" | tail -1
}

function mcd() {
    local DIRNAME
    DIRNAME="$1"
    mkdir "$DIRNAME" \
        && cd "$DIRNAME"
}

# mhost ("Map to Host(s)") - repeatedly execute a command on a list of hostnames (passing in each hostname as the command's last arg)
function mhost() {
    COMMAND="$1"; shift
    for NODE in "$@"; do
        echo "$NODE"
        echo ==
        echo
        "$NODE" "$COMMAND"
        echo
    done
}

# mshost ("Map Ssh [to] Host(s)") - execute an ssh command on a list of hosts, with paragraphized output
function mshost() {
    REMOTE_COMMAND="$1"; shift
    COMMAND="ssh $REMOTE_COMMAND"
    mhost "$COMMAND" "$@"
}

#### Password-generation settings

# Display all processes, in a style that I like. If an argument is given, interpret it as a grep regex, and display only those that match that regex.
function p() {
    local HEADER PROCS

    HEADER=$(ps auxwww | head -1)
    PROCS=$(ps auxwww | command grep -v grep | grep "$@")
    if [[ -n "$PROCS" ]]; then
        echo "$HEADER"
        echo "$PROCS"
    fi
}

#### Process-related settings

# pe (Ps Emacs-daemon): show whether the Emacs daemon is running.
function pe() {
    if [[ "$MY_OS" = "Darwin" ]]; then
        brew services | grep "$EMACS_PLUS_VERSION"
    else
        ps auxwww | grep 'emacs.*daemon'
    fi
}

# ke (Kill Emacs-daemon): kill the Emacs daemon, if running.
function ke() {
    if [[ "$MY_OS" = "Darwin" ]]; then
        brew services stop "$EMACS_PLUS_VERSION"
    else
        local EMACS_DAEMON_PID
        EMACS_DAEMON_PID="$(p 'emacs.*daemon' | awk '{print $2}')"
        kill "$EMACS_DAEMON_PID"
    fi
}

# se (Start Emacs-daemon): start the Emacs daemon.
function se() {
    if [[ "$MY_OS" = "Darwin" ]]; then
        brew services start "$EMACS_PLUS_VERSION"
        # Give it time to load my ~/.emacs.d/init.el
        sleep 2
    else
        emacs --bg-daemon
    fi
}

# re (Restart Emacs-daemon): stop it, then start it.
function re() {
    ke && se
}

# pbc ('(p)aste(b)oard (c)opy') - macOS only: copy file to the system clipboard
function pbc() { 
    local FILENAME
    FILENAME="$1"
    if [[ $FILENAME ]]; then
        pbcopy < "$FILENAME"
    else
        echo "pbc: error: missing argument. Usage: pbc FILENAME"
    fi
}

function pstree { command pstree -g 3 -u $USER "$@" | $PAGER ;}

# Display my processes.
function pu() { p $USER ;}

function _pq() {
    # grep the list of processes, but don't show any output.
    # This is useful for non-interactive functions that just need
    # to test whether a process is running.
    p auxww | grep -q $1 | grep -qv grep
}

#### vim settings

# vl ("Vim Less") - use vim as a colorizing pager
function vl() { $VIMRUNTIME/macros/less.sh "$@" ;}

#### Set operations

## All these operators take a list of two or more files,
## where each file consists of one or more lines.
## Each file is treated as a set,
## and each line is treated as an element of that set.
function set-intersection() {
    if [[ $# -eq 2 ]]; then
        command grep -xF -f "$1" "$2"
    else
        echo "$0: error: wrong number of arguments ($#). Usage: $0 SET_FILE_1 SET_FILE_2"
    fi
}
function set-intersect() { intersection "$@" ;}


function set-difference() {
     cat "$@" | sort | uniq -u
}
function set-disjunctive-union() { set-difference "$@" ;}

# [Broken] recursive version, for arbitrary numbers of sets
#
# function intersection() {
#     if [[ $# -lt 2 ]]; then
#         echo "$0: error: wrong number of arguments ($#). Usage: $0 FILE_1 FILE_2 [...]"
#     else
#         while [[ $# -gt 2]]; do
#             intersection $(intersection "$1" "$2" "$@")
#         done
#     fi
# }


#### SSH settings

function ssh-keygen() { command ssh-keygen -t ed25519 "$@" ;}

function ssh-add-all() {
    for KEY in $(find ~/.ssh -maxdepth 1 -type f -name '*_ed25519'); do
        ssh-add "$KEY"
    done
}

# [FIXME: needs work] When I first ssh to a host, configure it by installing my public ssh keys, my favorite dotfiles, my favorite yum packages, et cetera.
function quinnify_host() {
    local DOTFILES NODE PACKAGES

    # set -x

    if [[ ! $1 ]]; then
        echo "Error: missing argument. Usage: quinnify_host $HOSTNAME_OR_SSH_HOSTALIAS"
        return
    fi

    NODE="$1"

    DOTFILES='
        .bashrc.qweaver.bash
        .emacs
        .emacs.d
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
        wemux
    '

    ssh-copy-id -i ~/.ssh/qweaver_switchfly_3_id_ed25519 "$NODE" > /dev/null 2>&1

    rsync -rptgoDL ~/...sms-me "${NODE}:~/"

    #### Dotfiles.

    cd ~/dotfiles \
       && rsync -rptgoDL $DOTFILES ${NODE}:~/
    cd - > /dev/null

    ssh "$NODE" touch .quinnified

    # set +x
}

my_ssh() {
    NODE=$1

    if (! ssh "$NODE" test -e ~/.quinnified > /dev/null 2>&1); then
        quinnify_host "$NODE"
    fi

    ssh "$NODE"
}

# aa graceful|start|... -  because typing 'sudo apache2ctl' is too much work.
function aa() { sudo apache2ctl "$@" ;}

function atom() { newsbeuter "$@" ;}

# cel FILE.el ("compile LISP") - byte-compile an emacs LISP file.
function cel() { emacs -nw -q -batch -f batch-byte-compile "$@" ;}

# d ("date") - give today's date in YYYY-MM-DD format.
function d() { date +%Y-%m-%d ;}

# dos2unix FILE1 ... - translate-in-place DOS newlines to Unix newlines.
function dos2unix() { perl -pi -e 's{ \r\n | \n | \r }{ \n }gx' "$@" ;}

# dream - run the Electric Sheep fractal generation daemon
function dream() { /System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background ;}

# dfr ("df real") - show all mounted devices that are actually disks (see also mr).
function dfr() {
    df -h | grep ^/
}

# ff [PROFILE_NAME] - run Firefox with a profile. No args? Open the dialog box to list/create profiles.
function ff() {
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

function ho() { grep "Host .*${1}" ~/.ssh/*conf* ;}

function js() {
    FILE="$1"
    shift
    jsl -process "$FILE" "$@"
}

# mr ("mount real") - show all mounted devices that are actually disks (see also dfr).
function mr() { mount | grep ^/ ;}

# lw Perl::Module, lw Perl/Module.pm ("library which") - show the path to Perl module in PERL5LIB.
function lw() { perldoc -l "$1" ;}

# elw Perl::Module, elw Perl/Module.pm ("edit library which") - find a perl module in PER5LIB and open it in VISUAL. #FIXME: test behavior.
# elw() { $VISUAL $(lw "$@") ;}

# (See http://wiki.github.com/bard/mozrepl/)
# mz: Use mozrepl to connect to Firefox for some interactive debugging.
function mz() { socat READLINE TCP4:localhost:4242 ;}

# rmds: remove a directory, including .DS_Store droppings left by OS X Finder. Fails if the directory contains anything besides .DS_Store.
function rmds() {
    rm -fi "$1/.DS_Store"
    rmdir "$1"
}

# Invoke strings(1) safely, because http://www.jwz.org/blog/2014/10/we-live-in-a-magical-future-where-strings-is-exploitable/
function strings() { command strings -a "$@"  | $PAGER ;}

# para: print any paragraph contaning the desired string. A paragraph is defined as a collection of lines followed by an all-blank line (i.e., a newline).
function para() {
    STRING="$1"
    shift
    awk -v ORS="\n\n" -v RS='' "/$STRING/" "$@"
}

function conf() { para "$1" ~/.ssh/*.config | grep -e '^' -e "$1" ;}

# pi ("Perl @INC") - print @INC dirs, one per line, preceded by the path to Perl.
function pi() {
    echo -n "perl = "; which perl
    echo
    perl -e 'print (join "\n", @INC); print "\n";'
}

# shn ("SSH hostname") - print the canonical hostname for a ~/.ssh/config alias.
function shn() {
    grep -A 1 "$1" ~/.ssh/*config* | tail -1 | awk '{ print $3; }'
}

## tmux

function _tmux_is_new() {
    local LEADING_DIGIT_OF_VERSION_NUMBER
    LEADING_DIGIT_OF_VERSION_NUMBER="$(tmux -V | sed 's/^tmux[ ]\([0123456789]\).*/\1/')"

    if [[ "$LEADING_DIGIT_OF_VERSION_NUMBER" -ge 3 ]]; then
        return 0
    elif [[ "$LEADING_DIGIT_OF_VERSION_NUMBER" -lt 3 ]]; then
        return 1
    else
        echo "_tmux_is_new: error: version number starts with invalid character '$LEADING_DIGIT_OF_VERSION_NUMBER'"
        return 2
    fi
}

# ta $SESSION_NAME ("Tmux Attach") - attach to named session, creating if needed
function ta() {
    local SESSION_NAME TMUX_CONF
    
    if [[ $# -ne 1 ]]; then
        echo "ta: error: wrong number of arguments. Usage: ta SESSION_NAME"
    else
        SESSION_NAME="$1"

        if _tmux_is_new; then
            local TMUX_CONF=~/.tmux.new-versions.conf
        else
            local TMUX_CONF=~/.tmux.old-versions.conf
        fi

        if (! tmux has-session -t "$SESSION_NAME" 2> /dev/null); then
	    tn "$SESSION_NAME"
        else
            tmux -f "$TMUX_CONF" -2 attach-session -t "$SESSION_NAME"
        fi
    fi
}

# tn ("Tmux New") - create a new tmux session
function tn() {
    local SESSION_NAME TMUX_CONF
    
    if [[ $# -ne 1 ]]; then
        echo "tn: error: wrong number of arguments. Usage: tn SESSION_NAME"
    else
        SESSION_NAME="$1"

        if _tmux_is_new; then
            TMUX_CONF=~/.tmux.new-versions.conf
        else
            TMUX_CONF=~/.tmux.old-versions.conf
        fi

        tmux -f "$TMUX_CONF" new-session -s "$SESSION_NAME"
    fi
}

# tl ("Tmux List") - list running tmux sessions.
function tl() {
    if _tmux_is_new; then
        local TMUX_CONF=~/.tmux.new-versions.conf
    else
        local TMUX_CONF=~/.tmux.old-versions.conf
    fi

    tmux -f "$TMUX_CONF" list-sessions 2> /dev/null
}

function timestamp() { date +%Y%m%dT%H%M%S%z ;}
function ts() { timestamp "$@" ;}

# tor_wget URL1 [DEST_PATH]: get the contents of a URL, using Tor for anonymity.
function tor_wget() {
    HTTP_PROXY=http://127.0.0.1:8118/ \
    http_proxy=$HTTP_PROXY \
      wget --mirror --convert-links --html-extension --domains "$1" "$2"
}

# tor_on: start proxying all this shell's HTTP traffic through Tor.
function tor_on() { export HTTP_PROXY="http://127.0.0.1:8118/"; export http_proxy="$HTTP_PROXY" ;}

# tor_off: stop proxying this shell's HTTP traffic through Tor.
function tor_off() { unset HTTP_PROXY http_proxy ;}

# unix2dos FILE1 ...: translate-in-place newlines to the DOS convention (CRLF).
function unix2dos() { /usr/bin/perl -pi -e ' s{$}{\r}x; ' "$@" ;}

# unrar_plural FILE1 ...: because unraring files one by one is tiresome.
function unrar_plural() { for f in *rar; do unrar x -o+ "$f" .; done ;}

# xb ("xbindkeys") - reload custom (xbindkeys) X11 keybindings.
function xb() { killall --user "$USER" xbindkeys && xbindkeys ;}

# Make it so Ctrl-S and Ctrl-Q aren't interpreted as software flow control
# characters, messing up your terminal (see http://catern.com/posts/terminal_quirks.html).
# This also frees up Ctrl-Q for use as the tmux prefix key.
if (shopt -q login_shell   &&  tty > /dev/null 2>&1); then
    stty -ixon -ixoff > /dev/null 2>&1
fi

#### Perlbrew settings (this must come after the part in this file where PATH is set):

function activate-perlbrew() { source ~/perl5/perlbrew/etc/bashrc ;}
function perlbrew-activate() { activate-perlbrew ;}

# Uncomment if you want to stop timing how long each command in this
# file takes to run.
# set +xT

function psql-tips() { links ~/src/psql-tips/html/psql_tips_all.html ;}
function tips() { psql-tips "$@" ;}
function mutt() { neomutt "$@" ;}

#### Game development settings

function sd2lcompile() {
    gcc -o "$@"  `sdl2-config --cflags --libs`
}
function sdd() { sdl2compile "$@" ;}

#### Web settings

function brow() { defaultbrowser "$@" ;}
function brc() { brow "chrome" ;}
function brs() { brow "safari" ;}

# Adapted from a function posted by user Chubler_XL, in a comment on
# https://www.unix.com/shell-programming-and-scripting/277083-url-encoding-string-using-sed.html

function urlencode() {
    # URL encode string and optionally store in shell variable
    #
    # usage: url_var <string> [var]
    local LC_ALL=C
    local encoded=""
    local i c

    for (( i=0 ; i<${#1} ; i++ )); do
        c=${1:$i:1}
        case "$c" in
            [a-zA-Z0-9/_.~-] ) encoded="${encoded}$c" ;;
            * ) printf -v encoded "%s%%%02x" "$encoded" "'${c}" ;;
        esac
    done
    
    if [[ $# -gt 1 ]]; then
        printf -v "$2" "%s" "${encoded}"
    else
        printf "%s\n" "${encoded}"
    fi
}

function gmail-search() {
    local QUERY  BASE_URL
    BASE_URL="https://mail.google.com/mail/u/0/#search"
    
    if [[ $# -ne 1 ]]; then
        echo "gmail-search: error: wrong number of arguments. Usage: gmail-search QUERY_STRING"
    else
        QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$QUERY")
        open -u "$BASE_URL/$URLENCODED_QUERY"
    fi
}

function gmail() {
    if [[ $# -eq 0 ]]; then
        open -u "https://mail.google.com/"
    elif [[ $# -eq 1 ]]; then
        gmail-search "$1"
    else
        echo "gmail: error: wrong number of arguments. Usage: gmail [SEARCH_STRING]"
    fi
}

function google() {
    local BASE_URL="http://www.google.com/search?hl=en&ie=UTF-8&oe=UTF-8&q="

    if [[ $# -ne 1 ]]; then
        echo "google: error: wrong number of arguments. Usage: google QUERY"
    else
        local INPUT_QUERY MASSAGED_QUERY
        INPUT_QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$INPUT_QUERY")
        open -u "${BASE_URL}${URLENCODED_QUERY}"
    fi
}

function goog() { google "$@" ;}

#### Java settings

export JPATH="/usr/local/opt/openjdk/bin"

#### JavaScript settings

export NODE_PATH="/opt/node/bin"
export NODE_MANPATH="/opt/node/share/man:/opt/node/lib/node_modules/npm/man/"

#### Jenkins settings

function jenkins-fetch-output() {
    local URL
    if [[ $# -eq 1 ]]; then
        URL="$1"
        osascript ~/bin/jenkins-fetch-console-output.osascript "$URL"
    else
        echo "jenkins-fetch-output: error: missing argument. Usage: jenkins-fetch-console-output URL" > /dev/stderr
    fi
}

function jenkins-repo() {
    local REPOFILE
    REPOFILE="/tmp/jenkins-repo"

    if [[ $# -eq 0 ]]; then
        cat "$REPOFILE"
    elif [[ $# -eq 1 ]]; then
        echo "$1" > "$REPOFILE"
    else
        echo "jenkins-repo: error: too many arguments" > /dev/stderr
    fi
}

function jenkins-branch() {
    local BRANCHFILE
    BRANCHFILE="/tmp/jenkins-branch"

    if [[ $# -eq 0 ]]; then
        cat "$BRANCHFILE"
    elif [[ $# -eq 1 ]]; then
        echo "$1" > "$BRANCHFILE"
    else
        echo "jenkins-branch: error: too many arguments" > /dev/stderr
    fi
}

function j() {
    echo "Don't use this function; it's broken."
    echo "Instead, inspect the build log via the GitHub web page for your PR."
    return

    local REPO  BRANCH  JENKINS_OUTPUT  DOCKER_IMAGE_URL  SHORT_MESSAGE  LOGFILE
    
    REPO="$(jenkins-repo)"
    BRANCH="$(jenkins-branch)"
    
    if [[ -x "$JENKINS_BASE_URL" ]]; then
        echo 'jenkins: error: first export $JENKINS_BASE_URL'
    elif [[ -z "$REPO" ]]; then
        echo 'jenkins: error: first call jenkins-repo $REPO_NAME'
    elif [[ -z "$BRANCH" ]]; then
        echo 'jenkins: error: first call jenkins-branch $BRANCH_NAME'
    else
        JENKINS_OUTPUT_URL="$JENKINS_BASE_URL/job/$REPO/job/$BRANCH/lastBuild/consoleText"
        jenkins-fetch-output "$JENKINS_OUTPUT_URL"
        LOGFILE=$(ls -t /tmp/jenkins-output* | head -1)
        
        if command grep --quiet SUCCESS "$LOGFILE" >/dev/null; then
            SHORT_MESSAGE="Successfully built repo '$REPO', branch '$BRANCH'"
            DOCKER_IMAGE_URL="$(command grep --color=never --only-matching 'harbor.*:git-.*$' "$LOGFILE" | command uniq)"
            echo "$SHORT_MESSAGE"
            echo "Docker image URL copied to clipboard:"            
            echo "$DOCKER_IMAGE_URL" | tee >(pbcopy)
        elif command grep --quiet 'Log in with OpenShift' "$LOGFILE" >/dev/null; then
            SHORT_MESSAGE="Jenkins login needed"
            echo "First log in at $JENKINS_BASE_URL"; open -u "$JENKINS_BASE_URL"
        elif command grep --quiet 'FAILURE' "$LOGFILE" >/dev/null; then
            SHORT_MESSAGE="Failed to build repo '$REPO', branch '$BRANCH'"
            cat "$LOGFILE"
            echo
            echo "$SHORT_MESSAGE"
        else
            SHORT_MESSAGE="Unrecognized Jenkins output for repo '$REPO', branch '$BRANCH'"
            cat "$LOGFILE"
            echo
            echo "^ $SHORT_MESSAGE"
        fi
            
        if [[ -n "$JENKINS_SMS_ME" ]]; then
            ~/...sms-me "$SHORT_MESSAGE"
        fi
    fi
}

function jenkins-log() {
    local LATEST_LOGFILE
    LATEST_LOGFILE="$(ls -t /tmp/jenkins-output.* | head -1)"
    command bat --color=always "$LATEST_LOGFILE" | $PAGER
}
function jl() { jenkins-log "$@" ;}

#### Jira settings, regrettably

## For these to work, JIRA_BASE_URL must be set

# ji [ISSUE_TYPE] [PROJECT_ID]: open the page to create a new JIRA issue
#
# ISSUE_TYPE is an integer: 1 (bug), 2 (story), or 3 (task).
#
# PROJECT_ID is the integer ID of the project in which you want to create the issue.
# To get a list of project IDs, go to https://${YOUR_HOST}.atlassian.net/jira/projects
# and then click on the project you want.
#
# Thanks to Keri Savoca for documenting this URL format:
# https://kerisavoca.medium.com/how-to-make-a-link-that-creates-a-new-issue-in-jira-54ff93c61e24

function jira-create-ticket() {
    local USAGE ISSUE_TYPE PROJECT_ID ERROR_OCCURRED

    USAGE="jira-create-ticket [ISSUE_TYPE [PROJECT_ID]]
You may omit the arguments if you define env vars JIRA_DEFAULT_ISSUE_TYPE and JIRA_DEFAULT_ISSUE_ID"
    ISSUE_TYPE="$1"; if [[ ! "$ISSUE_TYPE" ]]; then ISSUE_TYPE="$JIRA_DEFAULT_ISSUE_TYPE"; fi
    PROJECT_ID="$2"; if [[ ! "$PROJECT_ID" ]]; then PROJECT_ID="$JIRA_DEFAULT_PROJECT_ID"; fi
    
    err() {
        ERROR_OCCURED=1
        echo "jira-create-ticket: error: $@"
    }
    
    [[ -n "$JIRA_BASE_URL" ]] || err "missing environment variable JIRA_BASE_URL"
    [[ -n "$ISSUE_TYPE" ]] || err "missing argument ISSUE_TYPE"
    [[ -n "$PROJECT_ID" ]] || err "missing argument PROJECT_ID"
    
    if [[ ! "$ERROR_OCCURRED" ]]; then
         open "${JIRA_BASE_URL}issues/secure/CreateIssue.jspa?issuetype=${ISSUE_TYPE}&pid=${PROJECT_ID}"
    fi
}

function jc() {
    jira-create-ticket > /dev/null
}

function jira-open-existing-ticket() {
    local TICKET_NUMBER
    
    if [[ $# -ne 1 ]]; then
        echo "jira-open-existing-ticket: error: wrong number of arguments. Usage: jira-open-existing-ticket TICKET_NUMBER"
    elif [[ -z "$JIRA_BASE_URL" ]]; then
        echo "jira-open-existing-ticket: error: you must set the JIRA_BASE_URL environment variable"
    else
        TICKET_NUMBER="$1"

        if [[ "$TICKET_NUMBER" =~ [[:digit:]]+ ]]; then
            echo "Match"
            if [[ -z "$JIRA_DEFAULT_PROJECT" ]]; then
                echo "jira-open-existing-ticket: error: numeric ticket ID given but JIRA_DEFAULT_PROCESS not set"
                return
            else
                TICKET_NUMBER="${JIRA_DEFAULT_PROJECT}-${TICKET_NUMBER}"
            fi
        else
            echo "No match"
        fi

        open -u "${JIRA_BASE_URL}/browse/${TICKET_NUMBER}"
    fi
}

function jira-browse-site {
    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    else
        open "$JIRA_BASE_URL"
    fi
}

function jira() {
    if [[ $# -eq 0 ]]; then
        jira-browse-site
    elif [[ $# -eq 1 ]]; then
        jira-open-existing-ticket "$@"
    elif [[ $# -eq 2 ]] && [[ $1 -eq "create" ]]; then
        shift
        jira-create-ticket "$@"
    else
        echo "j: invalid number of arguments '$#'. Doing nothing"
    fi
}

function jql() {
    local QUERY JQL_BASE_URL FULL_URL

    if [[ -z "$JIRA_BASE_URL" ]]; then
        echo "Error: you must set the JIRA_BASE_URL environment variable"
    elif [[ $# -ne 1 ]]; then
        echo "jql: error: wrong number of arguments. Usage: jql QUERY_STRING"
    else
        JQL_BASE_URL="${JIRA_BASE_URL}/issues/?jql="
        QUERY="$1"
        URLENCODED_QUERY=$(urlencode "$QUERY") || return
        FULL_URL="${JQL_BASE_URL}${URLENCODED_QUERY}"
        open "$FULL_URL"
    fi
}

function epics() {
    jql '(creator = currentUser() OR assignee = currentUser()) AND issuetype = Epic'
}

function jmine() {
    jql '(creator = currentUser() OR assignee = currentUser()) AND resolution = Unresolved order by updated DESC'
}

#### Music-related settings

# zo (Zophar) - search for Nintendo Entertainment System music
function zo() {
    open https://mail.zophar.net/music/nintendo-nes-nsf
}

# [DOESN'T WORK YET] mus (Music) - open an Apple Music URL in Music.app
function mus() {
    if [[ $# -ne 1 ]]; then
        echo "mus: error: missing argument. Usage: mus URL"
    else
        osascript ~/bin/open-url-in-the-apple-music-app.scpt "$URL"
    fi
}

#### YAML and JSON settings

# ys ("YAML Sort")" - take a file and sort its YAML keys alphabetically
function ys() {
    local FILE="$1"
    yq -P "sort_keys(..)" "$FILE"
}

# yd ("YAML Diff") - diff YAML files, sorting their keys first. Does not change the original files.
function yd() {
    for FILE in "$1" "$2"; do
        ys "$FILE"
    done
    
    diff "$1" "$2"
}

function json2yaml() {
    if [[ $# -eq 0 ]]; then
        echo "json2yaml: error: missing filename argument"
    else
        local JSON_FILENAME="$1"
        shift
        yq eval -p json -o yaml "$JSON_FILENAME" "$@"
    fi
}
    
function yaml2json() {
    if [[ $# -eq 0 ]]; then
        echo "yaml2json: error: missing filename argument"
    else
        local YAML_FILENAME="$1"
        shift
        yq eval -p yaml -o json "$YAML_FILENAME" "$@"
    fi
}

#### path settings (global)

if [[ ! "$PATHS_SET_TIMESTAMP" ]]; then
    set -a
    PATHS_SET_TIMESTAMP="$(date)"

    ## executable PATH
    PATH="$HOME/.umt:/usr/local/opt/ccache/libexec:$JAVA_PATH:$NODE_PATH:$PATH:$RUBY_PATH:$PYTHON_INTERPRETER_PATH:$PYTHON_PIPX_PATH:$RUST_PATH:$PGPATH:/usr/local/bin:/opt/tmux/bin:/usr/local/sbin:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:/usr/local/opt/ncurses/bin:/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources:$SCSH_PATH"
    
    ## C settings/dynamic library path settings
    CPPFLAGS="$RUBY_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$RUBY_LDFLAGS $SCSH_LDFLAGS $LDFLAGS"
    PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}:${PKG_CONFIG_PATH}"
    export SDKROOT="`xcrun --show-sdk-path`"
    
    # If some of those variables that I interpolated in my PATH are undefined,
    # then I will wind up with '::' in my PATH. That's equivalent to having
    # '.' (the current working directory) in one's path, which is a security
    # risk, because there's a silly trick that hackers can use where they
    # put a malicious binary named ls or grep or something inside a directory
    # and a privileged user cd's there and accidentally runs it.
    # So remove any instances of two (or more) consecutive colons.
    # Same goes for a single trailing colon.
    PATH=$(echo $PATH | sed -e 's/[:]\{2,\}/:/g' -e 's/[:]$//')

    ## man page path
    MANPATH="$NODE_MANPATH:$MANPATH"

    ## dynamic library path settings
    LDFLAGS="$RUBY_LDFLAGS $SCSH_LDFLAGS $LDFLAGS"
    CPPFLAGS="$RUBY_CPPFLAGS $CPPFLAGS"
    PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}:${PKG_CONFIG_PATH}"

    ## golang path
    GOPATH="$HOME/go"
    
    set +a
fi

source-if-exists ~/.site.bash.d/postamble.bash
