for APP in Photos; do
    LOWERCASE=${APP,,}
    eval "function $LOWERCASE {
        open '/System/Applications/$APP.app'
    }"
done
