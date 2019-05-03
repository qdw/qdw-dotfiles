
#### Other settings

# umask 0022

# Make it so Ctrl-S and Ctrl-Q aren't interpreted as software flow control
# characters, messing up your terminal. See http://catern.com/posts/terminal_quirks.html
stty -ixon
stty -ixoff

#### Environment variables

set -a

#PS1="\u@\h:\w\\$ "


set +a

#### Aliases and shell fun
#### ctions

# ta $SESSION_NAME ("Tmux Attach"): attach to named session, creating if needed.
ta() {
    if (! tmux has-session -t $1 2> /dev/null); then
        tmux new-session -d -s $1 -n $1
    fi
    tmux -2 attach-session -t $1
}

# tl ("Tmux List"): list running tmux sessions.
tl() {
    tmux list-sessions 2> /dev/null
}

if [[ $OS = Darwin ]] && [[ -d /usr/local/opt/coreutils/libexec/gnubin ]]; then
    PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
    MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
    alias ls='ls -1 --color=auto'
elif [[ $OS = Linux ]]; then
    alias ls='ls -1 --color=auto'
else
    alias ls='ls -1 -G'
fi

source ~/.git-completion.bash

if [[ $RUNNING_UNDER_EMACS ]]; then # this var is set by ~/.emacs.d/init_bash.sh
    # Emacs shell-mode is a dumb terminal, so don't use advanced features:
    
    # ANSI colors don't work. Use a non-color shell prompt.
    PS1='$(__git_ps1 "[%s] ")\u@\H:\w \$ '
    
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
    PS1='\[\e[35;m\]$(__git_ps1 "[%s] ")\[\e[0m\]\[\e[34;1m\]\u@\H:\w \$\[\e[0m\] '
    
    # Use my favorite pager and pager options.
    LESS='--quit-if-one-screen --RAW-CONTROL-CHARS --no-init'
    PAGER=less; GIT_PAGER=$PAGER; ACK_PAGER=$PAGER
fi
