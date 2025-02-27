(load-file (expand-file-name "~/.emacs.d/qdw-themes/qdw-colors.el"))
(custom-set-faces `(org-level-1 ((t (:foreground ,qdw-bright-white)))))
(custom-set-faces `(org-headline-done ((t (:foreground ,qdw-green)))))
 

(when (or (member 'tritium custom-enabled-themes)
          (member 'mariana custom-enabled-themes))
  
  (setq org-keyword-to-color-qdw
        `(("TODO" . ,qdw-bright-magenta)
          ("GET_HELP" . ,qdw-bright-red)
          ("NEXT_ACTION" . ,qdw-bright-red)
          ("AWAITING_RESPONSE" . ,qdw-bright-magenta)
          ("QUESTION" . ,qdw-magenta)

          ("POSTPONED" . ,qdw-yellow)
          ("INFO" . ,qdw-yellow)
          ("PROJECT" . ,qdw-blue)
          ("SUBPROJECT" . ,qdw-bright-cyan)
          ("IN_PROGRESS" . ,qdw-bright-yellow)
          ("DONE" . ,qdw-green)))


                  

  (setq org-todo-keyword-faces
        (mapc (lambda (pair) (add-to-list 'org-todo-keyword-faces pair))
              org-keyword-to-color-qdw)))
