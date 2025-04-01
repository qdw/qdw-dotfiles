#### grep settings

set -a

# Prefer ripgrep if installed.
if installed "rg"; then
    MY_GREP="rg"
    MY_GREP_OPTIONS="--color=always" # --no-line-number"
    # MY_GREP_OPTIONS="$MY_GREP_OPTIONS -H --column -n"
    GREP_RECURSE="--no-heading --follow --no-one-file-system --no-line-number"
    GREP_EXCLUDE="" # rg uses a .ignore file, not command-line flags
    GREP_COLOR_FLAGS="--colors path:fg:blue --colors line:fg:yellow --colors column:fg:cyan --colors match:fg:red"
else
    if on-macos && installed "ggrep"; then
        MY_GREP="ggrep"
    else
        MY_GREP="command grep"
    fi
    
    MY_GREP_OPTIONS="-n -I --color=always"
    GREP_RECURSE="-R"
    GREP_EXCLUDE="--exclude-dir=.git --exclude-dir=.github --exclude-dir=cdk.out --exclude-dir=node_modules --exclude=.npmignore --exclude=package-lock.json --exclude-dir=__snapshots__ --exclude '*.js'"
    
    GREEN="32"
    YELLOW="33"
    FILENAME_COLOR="fn=${GREEN}"
    LINE_NUMBER_COLOR="ln=${YELLOW}"
    GREP_COLOR_FLAGS=""
    GREP_COLORS="${FILENAME_COLOR}:${LINE_NUMBER_COLOR}"
fi

set +a
