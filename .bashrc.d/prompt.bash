#### Bash prompt settings

if [[ "$BASH_PROMPT_USE_PREFIX_EMOJI" ]]; then
    if [[ "$INSIDE_EMACS" ]]; then
        PS1_LOGO=$'\u2130 ' # cursive uppercase E
    elif [[ "$HOSTNAME" = "$LAPTOP_HOSTNAME" ]]; then # define in your .bashrc
        PS1_LOGO=$'\uF8FF ' # Apple logo
    fi
fi

export PS1="\\[\\e[33m\\]${PS1_LOGO}\\u@\\h:\\w\\$\\[\\e[0m\\] "
