;; -*- lexical-binding: t; -*-

(use-package dired
    :init
    (require 'ls-lisp)
    (setopt ls-lisp-use-insert-directory-program nil            
            ls-lisp-use-localized-time-format t
            ls-lisp-format-time-list '("%H:%M %Y-%m-%d "
                                       "%H:%M %Y-%m-%d "))

    (defun dired-find-file-qdw ()
      "In Dired, visit the file or directory on the cursor.
If the file is a directory, use `dired-find-alternate-file'
to avoid creating many buffers. If the file is a regular file,
use `dired-find-file' to open it in a new buffer."
      (interactive)
      (let ((is-dir (file-directory-p (dired-get-file-for-visit))))
        (if is-dir
            (dired-find-alternate-file)
            (dired-find-file)))))
