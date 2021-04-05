(require 'desktop)

(let ((pwd (getenv "PWD")))
  (cond
    ((string-equal pwd (expand-file-name "~/src/devops"))
     (desktop-read)
     (add-hook 'kill-emacs-query-functions (lambda () (desktop-save pwd))))))
