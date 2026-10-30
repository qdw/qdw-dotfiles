(defgroup myntax nil
  "Hide verbose punctuation like braces and parens."
  :group 'convenience)

(defconst myntax-visibility 'myntax-hidden)

;; chars to hide (in all contexts). The ?\ notation is how Emacs does
;; character literals.
(defcustom myntax-hide-chars '(?\{ ?\} ?\;)
  "List of characters to hide when the mode is enabled."
  :type '(repeat character)
  :local t)

(defun myntax-build-regexp ()
  (regexp-opt-charset myntax-hide-chars))

(defun myntax-matcher (limit)
  (re-search-forward (myntax-build-regexp) limit t))

(defvar myntax-font-lock-keywords
  `((myntax-matcher
     (0 '(face nil invisible ,myntax-visibility) prepend))))

;;;###autoload
(define-minor-mode myntax-mode
  "Toggle display of verbose syntax characters such as braces and parens."
  :lighter " Myntax"
  (if myntax-mode
      (progn
        (add-to-invisibility-spec myntax-visibility)
        (font-lock-add-keywords nil myntax-font-lock-keywords)
        (font-lock-flush))
    
    (remove-from-invisibility-spec myntax-visibility)
    (font-lock-remove-keywords nil myntax-font-lock-keywords)
    (font-lock-flush)))

(provide 'myntax)
