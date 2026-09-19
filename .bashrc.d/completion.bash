## Bash completion
## Dependencies:
##     - Homebrew itself
##     - ./0x-homebrew.bash
##     - ./terminal.bash
##     - Homebrewed bash >= 5 (set as your login shell)
##     - The following additional Homebrew formulas:
##         - bash-completion@2
##         - gh
##         - docker
##         - rclone
##         - openshift

BASH_COMPLETION_DIR="$HOMEBREW_ROOT/etc/bash_completion.d"

# comp - run this function (inside an interacticve shell)
# when you want to pull in the third-party Bash completion library
# for a particular CLI tool. I recommend against calling it
# in your ~/.bashrc, since some third-party Bash completion libraries
# are time-consuming to source. They add up.

function comp {
    # sc - source completion file (from installed homebrew bash completions)
    function sc {
        for FILENAME in "$@"; do
            # shellcheck disable=SC1090
            source "$BASH_COMPLETION_DIR/$FILENAME"
        done
    }

    case "$1" in
        alacritty)
            if [[ "$ALACRITTY_ROOT" ]]; then # if installed, var set in terminal.bash
                # shellcheck disable=SC1091
                source "$ALACRITTY_ROOT/Contents/Resources/completions/alacritty.bash"
            else
                err "idk where alacritty.bash is; query your package manager"
            fi;;
        aws) sc "aws_bash_completer";;
        brew) my-source "$HOMEBREW_ROOT/completions/bash/brew";;
        docker) sc "docker" "docker-compose" "docker-machine-prompt.bash" \
                   "docker-machine-wrapper.bash" "docker-machine.bash";;
        git|github) sc "gh" "git-completion.bash" "git-prompt.sh";; 
        git-prompt) comp git; use-gitprompt;; # defined in prompt.bash
        m|macos|macOS) sc "m"; export GIT_COMPLETION_ACTIVATED=1;;
        openshift|oc|kubernetes|k8s) sc "oc" "kubecfg" "kubectl" "minikube";;
        *) sc "$1" ;;
    esac
}
