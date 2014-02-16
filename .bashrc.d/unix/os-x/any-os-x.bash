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

# Because I'm used to typing GCC's 'ldd' and not OS X's 'otool'
ldd() { otool -L "$@" ;}
