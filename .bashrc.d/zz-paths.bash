## Executables

export PATH="$CURL_PATH:$SUBL_PATH:$RUST_PATH:/usr/local/opt/ccache/libexec:$LUA51_PATH:/bin:$LUAROCKS_PATH:$NODE_PATH:$RUBY_PATH:$PYTHON_INTERPRETER_PATH:$PYTHON_PIPX_PATH:$PGPATH:/usr/local/bin:/usr/local/sbin:$HASKELL_PATH:$MY_HOMEBREW_PATH:/usr/bin:/usr/sbin:/bin:/sbin:$GOPATH/bin:$HOME/bin:$HOME/bin/color:$HOME/bin/iterm2:$HOME/bin/macos:$HOME/bin/emacs:$HOME/bin/3:$HOME/.local/bin:/usr/local/opt/ncurses/bin:$HOME/bin/site:$HOME/bin/site/upsa:$SCSH_PATH:$HOME/ai/bin:/opt/sidplayfp/bin:$GITLAWB_PATH:$QT_PATH:$ALACRITTY_PATH"

# Account for the fact that some of those interpolated env vars may be empty.

function print-without-empty-paths {
    echo "$1" | sed 's/:\{2,\}/:/g; s/^://; s/:$//'
}

export PATH="$(print-without-empty-paths "$PATH")"

## C libraries

# "CPP" in this case means "C preprocesor," not C++.
export CPPFLAGS="$LUA51_CPPFLAGS $RUBY_CPPFLAGS"
export LDFLAGS="$LUA51_LDFLAGS $SCSH_LDFLAGS $RUBY_LDFLAGS"
export PKG_CONFIG_PATH="${RUBY_PKG_CONFIG_PATH}"

# if on-macos; then
#     export SDKROOT="$(xcrun --show-sdk-path)"
# fi

## man pages

export MANPATH="$HOME/.local/share/man:$HOMEBREW_MANPATH:/usr/share/openssl/man:$HOME/share/man:/usr/share/man:/usr/local/share/man:/usr/local/man:$ALACRITTY_MANPATH"

export MANPATH="$(print-without-empty-paths "$MANPATH")"
