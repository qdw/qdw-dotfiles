;; -*- lexical-binding: t; no-native-compile: t; -*-

(use-package ghostel
    ;; :straight (:host github :repo "dhakra/ghostel" :local-repo "ghostel")
    ;; :bind (("M-s" . ghostel)
    :config (*add-hook* 'ghostel-mode-hook #'ghostel-line-mode))
