#!/bin/sh

find ~/.elisp -type f -name \*.el \
    | xargs emacs -nw -q -batch -f batch-byte-compile
