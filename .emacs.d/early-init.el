;; -*- lexical-binding: t; -*-

;;;; Dependency settings

;; The built-in Emacs package system (package.el)

(setopt package-load-list '(all (org nil))
        package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa"  . "https://melpa.org/packages/")))

;; straight.el, a more sophisticated, incompatible replacement for package.el
;; I am converting from package.el to straight.el one package at a time,
;; starting with org-mode.

(setopt straight-check-for-modifications nil)

(let ((bootstrap-version 7))
  (load (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          user-emacs-directory)
        nil
        'nomessage))
