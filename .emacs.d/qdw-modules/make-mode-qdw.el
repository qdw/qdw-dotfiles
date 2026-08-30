;; -*- lexical-binding: t; no-native-compile: t; -*-

(use-package make-mode
    ;; :mode ("\\`Makefile\\'" . makefile-mode)
    :hook (makefile-mode . (lambda () (setq-local indent-tabs-mode t))))
