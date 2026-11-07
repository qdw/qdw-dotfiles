#### Other programming languages

function lisps {
    (
        brew ls \
            | command grep -i \
                      --color=never \
                      -E 'lisp|scheme|guile|plt|racket|sbcl'
        
        for DIR in "$HOME/src-3/sectorlisp" "$HOME/src-3/scheme.rs"; do
            local LISP_NAME="$(basename "$(ls -d "$DIR" 2>/dev/null)")"
            if [[ "$LISP_NAME" ]]; then
                echo "$LISP_NAME (in $DIR/)"
            fi
        done
    ) \
    | sort
}
