(load-file (expand-file-name "~/.emacs.d/qdw-themes/qdw-colors.el"))

(setq-default
  org-catch-invisible-edits "show"
  org-todo-keywords '((sequence
                       "TODO(t)"
                       "NEXT_ACTION(n)"
                       "IN_PROGRESS(i)"
                       "GET_HELP(h)"
                       "AWAITING_RESPONSE(a)"          
                       "QUESTION(q)"
                       "YAK_SHAVING(y)"
                       "INFO(f)"
                       "PROJECT(r)"
                       "SUBPROJECT(s)"
                       "POSTPONED(p)"
                       
                       "|"

                       "DONE(d)"
                       "WONTFIX(w)"
                       )))

(when (or (member 'tritium custom-enabled-themes)
          (member 'mariana custom-enabled-themes))
  (setq org-keyword-to-color-qdw
        `(("TODO" . ,qdw-bright-magenta)
          ("NEXT_ACTION" . ,qdw-bright-red)
          ("IN_PROGRESS" . ,qdw-bright-yellow)
          ("GET_HELP" . ,qdw-bright-red)
          ("AWAITING_RESPONSE" . ,qdw-bright-magenta)
          ("QUESTION" . ,qdw-magenta)
          ("YAK_SHAVING" . ,qdw-yellow)

          ("DONE" . ,qdw-green)
          ("WONTFIX" . ,qdw-green)
          ("POSTPONED" . ,qdw-yellow)
          ("INFO" . ,qdw-yellow)
          ("PROJECT" . ,qdw-blue)
          ("SUBPROJECT" . ,qdw-bright-cyan)
          ))
  (setq org-todo-keyword-faces
        (mapc (lambda (pair) (add-to-list 'org-todo-keyword-faces pair))
              org-keyword-to-color-qdw)))

;; (custom-set-faces
;;  ( `(*SPEC*) 't)
;;  ( `(*SPEC*) 't))
