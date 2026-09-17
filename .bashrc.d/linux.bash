if command-exists "xdg-open"; then
    function open {
        xdg-open "$@"
    }
fi

function which {
    type -P "$@"
}
