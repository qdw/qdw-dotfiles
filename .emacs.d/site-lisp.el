(setq-default org-todo-keywords '((sequence
  "TODO(t)"
  "NEXT_ACTION(n)"
  "AWAITING_RESPONSE(a)"
  
  "|"
  
  "DONE(d)"
  "POSTPONED(p)"
  "NG(g)"
  "WONTFIX(w)"
  )))

;; Always use colors compatible with TERM=xterm-256colors,
;; and always specify them as hex.
;; 
;; You can browse colors and copy their hex values here:
;; https://www.ditig.com/256-colors-cheat-sheet

;; FIXME: assign hex codes to color names,
;; either here using (let ...
;; or in the subatomic-256.el theme file.
(setq-default org-todo-keyword-faces '(
  ("NEXT_ACTION" . "#ff00af")
  ("AWAITING_REPONSE" . "#ffff00")
  ("POSTPONED" . "#ffff00")
  ("NG": "#af5f00")
  ))

(add-to-list 'load-path
             "/opt/homebrew/Cellar/clang-format/16.0.2/share/clang/"
             't))

(require 'clang-format)

