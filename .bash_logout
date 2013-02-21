# ~/.bash_logout: executed by bash(1) when login shell exits.

# When leaving the console, clear the screen to enhance privacy.

if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
