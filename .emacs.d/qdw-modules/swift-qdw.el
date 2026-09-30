;; -*- lexical-binding: t; no-native-compile: t; -*-

;; Broken. tree-sitter in Emacs 31 sucks. Wait till 32 or later.
;; 
;; (use-package swift-ts-mode
;;     :straight (:host github :repo "rechsteiner/swift-ts-mode")
;;     :mode "\\.swift\\'"
;;     :init
;;     (add-to-list 'treesit-language-source-alist
;;                  '(swift "https://github.com/alex-pinkus/tree-sitter-swift"))
;;     (setopt treesit-auto-install-grammar 'always))

(use-package swift-mode
    :straight (:host github :repo "swift-emacs/swift-mode")
    :mode "\\.swift\\'")
