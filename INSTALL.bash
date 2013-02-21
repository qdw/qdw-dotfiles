#!/usr/bin/env bash

# Install only some dotfiles by default - not all.
DEFAULTS='.bashrc .bashrc.d .bash_logout .git-completion.bash .gitconfig .gitignore .psqlrc .psqlrc-9.2 .tmux.conf'

DIR=`pwd`
cd
for DOTFILE in $DEFAULTS; do
    ln -s $DIR/$DOTFILE .
done
cd -

# Special case: ~/.ssh/config must go in a directory with the write permissions,
# and it must be copied, not symlinked. These requirements prevent exploits.
mkdir -p ~/.ssh
chmod 0700 ~/.ssh
cp $DIR/.ssh/config ~/.ssh/config
chmod 600 ~/.ssh/config
