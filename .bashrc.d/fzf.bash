# shellcheck disable=SC2016

# set -a
# FZF_CTRL_T_OPTS="..."        # configure the behavior of Ctrl-T
# FZF_CTRL_R_OPTS="..."        # configure the behavior of Ctrl-R
# FZF_ALT_C_OPTS="..."         # configure the behavior of Alt-C
# set +a

MY_FZF="$HOMEBREW_BIN/fzf"
if [[ -x "$MY_FZF" ]]; then
    eval "$("$MY_FZF" --bash)"

    # Restore bash/readline key bindings that were just clobbered by fzf

    bind -r '\C-r'
    bind -m emacs-standard '"\C-r": reverse-search-history'
    
    bind -r '\C-t'
    bind -m emacs-standard '"\C-t": transpose-chars'

    # ... and then remap the fzf bindings to other keys.

    # ESC-o
    bind -m emacs-standard '"\eo": fzf-file-widget'

    # ESC-v: fzf paste
    bind -m emacs-standard '"\ev": " \C-b\C-k \C-u`__fzf_select__`\e\C-e\C-\e(\C-a\C-y\C-h\C-e\e \C-y\ey\C-x\C-x\C-f\C-y\ey\C-_"'

    # works: ESC-?: isearch backward in history
    bind -r '\e?'
    bind -m emacs-standard '"\e?": "\C-e \C-u\C-y\ey\C-u`__fzf_history__`\e\C-e\C-\e("'

    # work: ESC-c: cd to the selected dir
    bind -r '\ec'
    bind -m emacs-standard '"\ec":" \C-b\C-k \C-u`__fzf_cd__`\e\C-e\C-\e(\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d\C-y\ey\C-_"'
fi
