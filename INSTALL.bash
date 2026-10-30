#!/bin/bash

set -eu

QDIR="$HOME/src/1/qdw-dotfiles"

function inst_main {
    install_xcode
    install_dotfiles
    install_homebrew
    source "$HOME/.bashrc"
    install_homebrewed_emacs_plus
    # configure_iterm2
    # configure_macos
    # configure_sublime_text
    print_airpods_pairing_instructions
}

function print_airpods_pairing_instructions {
    echo "Go to Settings > Bluetooth,
open your AirPods case,
and hold down the button on the case."
}

function configure_macos { #FIXME
    :
#     dark
#     wallpaper set solid-color "#000000"
#     mkdir -p ~/wallpaper/light ~/wallpaper/dark
#     touch ~/wallpaper/light/current-favorite
#     touch ~/wallpaper/dark/current-favorite
# 
#     dock {
#         System Settings > Dock > Position on screen > left
#         System Settings > Dock > Automatically hide and show the dock -> on
#         System Settings > Dock > Double-click a window\'s title bar to -> zoom
#         ## System Settings > Screen Saver >        
#     }
#     trackpad {
#         double-click-rate third-slowest
#         pointer-speed
#         System Settings > Appearance > Show scroll bars > Automatically based on mouse or trackpad
#         System Settings > Appearance > Click in the scroll bar to > Jump to the next page
#     }
#     keyboard {
#         caps lock -> control
#         fn -> option
#         System Settings > Keyboard > Keyboard Shortcuts > Turn Dock hiding on/off > off
#         System Settings > Keyboard > Key repeat rate > max
#         System Settings > Keyboard > Delay until repeat > min
#     }
#     notifications {
#         System Settings > Notifications > Show previews > When unlocked
#         System Settings > Notifications > Allow notifications when the display is sleeping -> off
#         System Settings > Notifications > Allow notifications when the display is locked -> off
#         System Settings > Notifications > Summarize notifications > on
#     }
#     sound {
#         # all sound effects -> off
#         System Settings > Accessibility > Audio > Flash the screen when an alert sound occurs -> on
#         System Settings > Sounds > Alert volume > 0
#         System Settings > Sounds > Play sound on startup > off
#         System Settings > Sounds > Play user interface sound effects > off
#         System Settings > Sounds > Play feedback when volume is changed > off
#     }
}

function configure_sublime_text { #FIXME
    :
}

function configure_iterm2 { #FIXME
    :
    # programatically-iterm2-import-profiles "$QDIR/iTerm2/Profiles.json"
    # somehow-import-non-profile-settings
}

function install_xcode {
    xcode-select --install
}

function install_dotfiles {
    mkdir -p "$HOME/src/1"
    cd $_
    git clone https://github.com/qdw/qdw-dotfiles

    # FIXME: get straight.el working
    cd
    mkdir .emacs.d
    cd $_
    local EMDIR="$QDIR/.emacs.d"
    for FILE in $EMDIR/.gitignore $EMDIR/*.el $EMDIR/*.sh $EMDIR/*.scpt; do
        ln "$FILE"
    done
    for DIR in $EMDIR/3 $EMDIR/qdw $EMDIR/qdw-modules $EMDIR/qdw-themes; do
        ln "$DIR"
    done
    cd
    ln -s .emacs.d/init.el

    mkdir -p ~/src/forks
    cd $_
    git clone https://github.com/qdw/mariana-theme
    cd
    ln -s ~/src/forks/mariana-theme/mariana-theme.el
            
    cd
    mkdir -p .config
    cd $_
    local CDIR="$QDIR/.config"
    for DIR in bat git tmux vim; do
        ln -s "$QDIR/DIR"
    done
    ln -s "$QDIR/.git-prompt-colors"

    cd
    for BSH in .bash_logout .bash_profile .bashrc.d; do
        ln "$BSH"
    done
    ln -s ".bashrc.d/.bashrc"
    mkdir .site.bash.d

    cd
    mkdir .ssh
    chmod 00700 $_
    cd $_
    ln -s "$QDIR/.ssh/global.config"

    cd
    touch .hushlogin
    for MISC_FILE in .inputrc .inputrc--vi-keybindings .psqlrc .psqlrc-19 .screenrc .shellcheckrc bin; do
        ln "$MISC_FILE"
    done

    for CSK in gcloud-cli sublime-text; do
        brew install --cask "$CSK"
    done

    brew install --formula bash bash-completion@2 bash-git-prompt bat bfs bgrep binwalk black cjson clang-format cmake coreutils defaultbrowser diffutils duf dust fd findutils fontconfig fzf gcc gcloud-cli gh git git-delta git-filter-repo glib grep html2text icann-rdap ifstat jless jq json-c k9s kubecfg kubernetes-cli less libpcap libpq lld llvm lsof m4 make makedepend man-db markdown minikube moreutils ngrep nmap openssh ossp-uuid pandoc pcre2 pgbadger pgbouncer pgcli pgvector pgxnclient pkgconf postgresql@15 postgresql@16 postgresql@17 postgresql@18 procs pstree psutils pv rclone readline ripgrep ripgrep-all rsync sf-symbols stunnel sublime-text tmux trash tree-sitter unixodbc unzip wallpaper watch yq
    
function install_homebrew {
    local BR
    BR="https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"    
    /bin/bash -c "$(curl -fsSL $BR)"
}

function install_homebrewed_emacs_plus {
    cd
    source "$HOME/.bashrc"
    local EMACS_TAP="d12frosted/emacs-plus"
    local EMACS_VER="31"

    brew tap "d12frosted/emacs-plus"
    brew trust "d12frosted/emacs-plus/emacs-plus@${EMACS_VER}"
    install-emacs-plus "$EMACS_VER"
}

inst_main

set +eu
