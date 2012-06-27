##########################
########################## OS X-specific settings and functions
##########################

os_x_hide_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles FALSE
    killall Finder 
}

os_x_show_hidden_files() {
    defaults write com.apple.finder AppleShowAllFiles TRUE
    killall Finder 
}

# ldd LIBRARY: because I'm used to Linux's 'ldd' and not OS X's 'otool'
alias ldd='otool -L'
