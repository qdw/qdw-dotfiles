#!/usr/bin/env bash

# Install only some dotfiles by default - not all.
FILES_TO_INSTALL='
.bashrc
.bashrc.d
.bash_logout
.git-completion.bash
.gitconfig
.gitignore
.newsbeuter
.psqlrc
.psqlrc-9.2
.tmux.conf'

for DOTFILE in $FILES_TO_INSTALL; do
    COMMAND="ln -sf ~/dotfiles/$DOTFILE ~"
    echo "*Not* running command '$COMMAND'"    
done

# Special case: ~/.ssh/config must go in a directory with the right permissions
# and it must be copied, not symlinked. These requirements prevent exploits.
mkdir -p ~/.ssh
chmod 0700 ~/.ssh
cp ~/dotfiles/.ssh/config ~/.ssh/config
chmod 600 ~/.ssh/config
