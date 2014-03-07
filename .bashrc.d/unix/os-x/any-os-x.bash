##########################
########################## OS X-specific settings and functions
##########################

os_x_show_full_path_in_finder_titlebar() {
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool YES
    killall Finder
}

os_x_hide_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles FALSE
    killall Finder 
}

os_x_show_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles TRUE
    killall Finder 
}

# Start (homebrewed) PostgreSQL.
export PGDATA=/usr/local/var/postgres
os_x_start_postgres_idempotently() {
    if (! ps -ef | grep postgres | grep -v grep > /dev/null 2>&1); then
        if [[ -d $PGDATA ]]; then
            $HOMEBREW_ROOT/bin/pg_ctl -D $PGDATA start > /dev/null
        fi
    fi
}

os_x_start_postgres_idempotently

# Because I'm used to typing GCC's 'ldd' and not OS X's 'otool'
ldd() { otool -L "$@" ;}
