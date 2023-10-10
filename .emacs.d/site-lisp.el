(load (expand-file-name "~/.emacs.site.d/upstart.el") 't)
(load (expand-file-name "~/.emacs.site.d/au.el") 't)
(setq-default
  org-todo-keywords
  '((sequence
     "TODO(t)"
     "QUESTION(q)"
     "NEXT_ACTION(n)"
     "AWAITING_RESPONSE(a)"
     "IN_PROGRESS(i)"
     "GET_HELP(h)"
     
     "|"
  
     "DONE(d)"
     "POSTPONED(p)"
     "WONTFIX(w)"
     "JIRAFIED"
     "INFO"
     ))

  ;; Always use colors compatible with TERM=xterm-256colors,
  ;; and always specify them as hex.
  ;; 
  ;; You can browse colors and copy their hex values here:
  ;; https://www.ditig.com/256-colors-cheat-sheet

  ;; FIXME: assign hex codes to color names,
  ;; either here using (let ...
  ;; or in the subatomic-256.el theme file.
  org-todo-keyword-faces
  '(("GET_HELP" . "#ff00af")
    ("NEXT_ACTION" . "#ff00af")
    ("AWAITING_RESPONSE" . "#ffff00")
    ("QUESTION" . "#ffff00")
    ("POSTPONED" . "#ffff00")
    ("INFO" . "#ffff00")
    )
  )
