#### Improvements on/pure-bash implementations of core Unix commands

function basename {
    echo "${1##*/}"    
}

if command-exists rsync; then
    function cp {
        check-args \
            2 varargs \
            'cp FILE_1 FILE_2 | cp FILE_1 [FILE_2 [...]] DIR # uses rsync' \
            "$@" \
        || return 1
        
        rsync -aq "$@"
    }
fi

