(use-package cc-mode
    :config

    (c-add-style
     "qdw"
     '("stroustrup"
       (c-offsets-alist
        ;; Indent case statements 1 level beyond the enclosing 'switch() {' line
        (case-label . +)
        (statement-case-intro . +)
        (statement-case-open . 0)

        ;; When breaking stuff onto multiple lines, always indent 1 level
        (arglist-cont . +)
        (arglist-cont-nonempty . +)
        (arglist-intro . +)
    
        (statement-cont . +)
        (topmost-intro-cont . +)
        (arglist-cont . +)
        (arglist-cont-nonempty . +)
        (statement-cont . +)
        (topmost-intro-cont . +)
        (brace-list-entry . +)
        (brace-list-intro . +)
        (brace-entry-open . +))

       ;; Keep the opening brace of a function body
       ;; on the same line as the function signature:
       ;; 
       ;; int foo(int arg1) {
       (c-hanging-braces-alist
        (defun-open . (nil . t)))))   ;; no newline before {, newline after

    (setopt c-default-style "qdw"))
