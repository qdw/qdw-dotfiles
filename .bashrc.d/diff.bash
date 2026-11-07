function dcc { colordiff -u "$@" | $PAGER ;}

HOMEBREW_DIFF="$HOMEBREW_ROOT/opt/diffutils/bin/diff"
if [[ -f "$HOMEBREW_DIFF" ]]; then
    WE_HAVE_GNU_DIFF=0
    export DIFF_PATH="/opt/homebrew/opt/diffutils/bin"

    # Use custom colors, which are supported by GNU grep but not by macOS grep:
    # 
    # ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
    # de - deleted lines (31 is SGR red, which shows as orange in my terminal)
    # hd - header        (35 is SGR magenta, which shows as magenta in my term)
    # ln - line numbers  (34 is SGR blue, which shows as blue in my term)
    # rs - undocumented, but the default seems fine
    #

    # shellcheck disable=SC2089
    MY_GNU_DIFF_OPTIONS='--palette="ad=36:de=31:hd=35:ln=34:rs=0"'
    function gdiff { "$DIFF_PATH/diff" "$@" ;}
    function diff-gnu { gdiff "$@" ;}
else
    WE_HAVE_GNU_DIFF=1
fi

function diff-vanilla {
    local COMMON_DIFF_OPTIONS="-u --color=always"
    if $WE_HAVE_GNU_DIFF; then
        # shellcheck disable=SC2090
        command diff $MY_GNU_DIFF_OPTIONS $COMMON_DIFF_OPTIONS "$@" | $PAGER
    else
        # shellcheck disable=SC2090
        command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
    fi
}

function diff-cdk-code {
    diff-vanilla -Nur \
        --exclude 'node_modules' \
        --exclude 'package*.json' \
        --exclude 'cdk.json' \
        --exclude 'cdk.out' \
        --exclude 'CODEOWNERS' \
        --exclude 'initialize-repo.sh' \
        --exclude '.gitignore' \
        --exclude '.husky' \
        --exclude '.eslintrc.yml' \
        --exclude '.mergify.yml' \
        --exclude '.npmignore' \
        --exclude '.tool-versions' \
        --exclude 'README.md' \
        --exclude 'tsconfig.json' \
        --exclude '*example*' \
     "$@"
}

# function alt-diff
#     # Git diff
#     git diff --no-index "$@"
    
#     ### Other available diff tools, FYI
#     ### local COMMON_DIFF_OPTIONS="-u --color=always"
#     # 
#     ## Recent GNU diff
#     # 
#     #   Color legend:
#     #   ad - added lines   (36 is SGR cyan, which in fact shows as cyan in my term)
#     #   de - deleted lines (31 is SGR red, which shows as orange in my terminal)
#     #   hd - header        (35 is SGR magenta, which shows as magenta in my term)
#     #   ln - line numbers  (34 is SGR blue, which shows as blue in my term)
#     #   rs - undocumented, but the default seems fine
#     # 
#     # command diff $COMMON_DIFF_OPTIONS --palette="ad=36:de=31:hd=35:ln=34:rs=0" "$@" | $PAGER
#     # 
#     ## Old GNU diff (shipped with macOS as /usr/bin/diff)
#     #
#     # command diff $COMMON_DIFF_OPTIONS "$@" | $PAGER
# }

GIT_MERGE_HELP_URL="https://howchoo.com/git/git-merge-conflicts-rebase-ours-theirs"

function ours {
    open "$GIT_MERGE_HELP_URL"
}

function theirs {
    ours "$@"
}

function diff {
    # I just like git diff's syntax coloring better.
    # --no-index allows you to use it anywhere on files,
    # without regard to their history, just like
    # regular diff.
    git diff --no-index "$@"
}
