;; -*- lexical-binding: t; -*-

(use-package cperl-mode
  ;; :after (tritium)
  :mode ("\\.pl\\'" . cperl-mode)
  :interpreter ("perl" . cperl-mode)
  :custom (cperl-indent-level 4))
