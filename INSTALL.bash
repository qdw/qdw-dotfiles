#!/usr/bin/env bash

GIT_WORKING_COPY=$(dirname $0)

# Install only some dotfiles by default - not all.
FILES_TO_INSTALL='
.bashrc
.bashrc.d
.git-prompt-colors.sh
.bash_logout
.gitconfig
.gitignore
.hushlogin
.inputrc
.psqlrc
.screenrc
.tmux.new-versions.conf
.tmux.old-versions.conf
.vimrc'

for DOTFILE in $FILES_TO_INSTALL; do
    ln -si "$GIT_WORKING_COPY/$DOTFILE" ~
done

# Special case: ~/.ssh/config must go in a directory with the right mode
# and it must be copied, not symlinked. This prevents certain exploits.
mkdir -p ~/.ssh
chmod 0700 ~/.ssh
cp -ai "$GIT_WORKING_COPY/.ssh/global.config" ~/.ssh/
