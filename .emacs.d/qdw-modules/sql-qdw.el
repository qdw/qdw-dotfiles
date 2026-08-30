;; -*- lexical-binding: t; no-native-compile: t; -*-

(use-package sql
    :config (font-lock-add-keywords
             'sql-mode
             '(("\\_<\\(?:FROM\\|JOIN\\|ON\\)\\_>" .
               (0 font-lock-builtin-face t)))
             'append))

    ;; (sql-add-product-keywords
    ;;  :postgresql
    ;;  '((MATCH-HIGHLIGHT "\\<FROM\\>"   font-lock-keyword-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<JOIN\\>"   font-lock-keyword-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<ON\\>"     font-lock-keyword-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<WHERE\\>"  font-lock-keyword-face 't 't)
          ;;;;;;;;;;;;;;; (more MATCH-* lists go here...)
    ;;    (MATCH-HIGHLIGHT "\\<SELECT\\>" font-lock-builtin-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<AND\\>"    font-lock-builtin-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<OR\\>"     font-lock-builtin-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<IN\\>"     font-lock-builtin-face 't 't)
    ;;    (MATCH-HIGHLIGHT "\\<ON\\>" font-lock-builtin-face 't 't))
    ;;   'set)

(use-package sql-indent
    :straight (:host github
               :repo "alex-hhh/emacs-sql-indent"
               :local-repo "sql-indent")
    :hook ((sql-mode . sqlind-minor-mode)))
   ;; :config (...))
