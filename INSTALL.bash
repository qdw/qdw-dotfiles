#!/usr/bin/env bash

function main {
    local GIT_WORKING_COPY="$(dirname "$0")"

    # Install only some dotfiles by default - not all.
    local FILES_FOR_HOME_DIR="
.bash_logout
.bash_profile
.bashrc
.git-prompt-colors.sh
.hushlogin
.inputrc
.inputrc--vi-keybindings
.psqlrc
.psqlrc-9.4.1
.screenrc
.ssh-config-template
"
    for DOTFILE in $FILES_FOR_HOME_DIR; do
        ln -si "$GIT_WORKING_COPY/$DOTFILE" "$HOME"
    done

    # Special case: $HOME/.ssh/config must go in a directory with the right mode,
    # and it must be copied, not symlinked. This prevents certain exploits.
    mkdir -p "$HOME/.ssh"
    chmod 0700 "$HOME/.ssh"
    cp -ia "$GIT_WORKING_COPY/.ssh/global.config" "$HOME/.ssh/"

    local CONFIG_DIR="{$XDG_CONFIG_HOME:-$HOME/.config}"
    local DIRS_FOR_XDG_CONFIG="
alacritty/
bat/
git/
tmux/
vim/
"
    for DIR in $DIRS_FOR_XDG_CONFIG; do
        ln -is "$GIT_WORKING_COPY/$DIR" "$CONFIG_DIR"
    done
}

main
