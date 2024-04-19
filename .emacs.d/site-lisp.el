(load (expand-file-name "~/.emacs.site.d/upstart.el") 't)
(load (expand-file-name "~/.emacs.site.d/au.el") 't)

(nconc org-todo-keywords
       '((sequence "EPIC(e)" "TASK(k)" "|")))

(let ((project-color
       (cdr (assoc "PROJECT" org-todo-keyword-faces))))
  (nconc org-todo-keyword-faces
         (list (cons "EPIC" project-color))))
