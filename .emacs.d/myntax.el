(require 'cl-lib)

(defgroup myntax nil
  "Hide verbose punctuation like braces and parens."
  :group 'convenience)

(defconst myntax-visibility 'myntax-hidden)

;; alist of (MODE-LIST . CHAR-LIST). MODE-LIST is matched against the
;; buffer's major mode with `derived-mode-p', so derived/variant modes
;; (e.g. c-ts-mode under c-mode's ts sibling, or any mode you derive
;; from one of these) pick up the right entry without extra listing.
(defcustom myntax-hide-chars-alist
  '(((c-mode c++-mode objc-mode c-ts-mode c++-ts-mode) . (?\{ ?\} ?\;))
    ((js-mode js2-mode typescript-mode typescript-ts-mode) . (?\{ ?\} ?\;))
    ((json-mode json-ts-mode) . (?\{ ?\}))
    ((lua-mode) . (?\;))
    ((sh-mode) . ()))
  "Alist mapping a list of major modes to the characters myntax
should hide in buffers of those modes. The first entry whose
mode list contains a mode that `derived-mode-p' matches wins."
  :type '(repeat (cons (repeat symbol) (repeat character))))

(defvar myntax-font-lock-keywords
  `((myntax-matcher
     (0 '(face nil invisible ,myntax-visibility) prepend))))

;; Buffer-local resolved char list, set when the mode is turned on.
;; Kept separate from `myntax-hide-chars-alist' itself so the matcher
;; doesn't have to re-walk the alist on every search.
(defvar-local myntax-chars nil)

(defun myntax-chars-for-mode ()
  "Return the configured char list for the current buffer's major mode,
or nil if no entry matches."
  (cl-loop for (modes . chars) in myntax-hide-chars-alist
           when (apply #'derived-mode-p modes)
           return chars))

(defun myntax-build-regexp ()
  (and myntax-chars (regexp-opt-charset myntax-chars)))

(defun myntax-matcher (limit)
  (let ((re (myntax-build-regexp)))
    (and re (re-search-forward re limit t))))

(defun myntax-collapse-blank-lines (beg end)
  "If a line is all-invisible after myntax hiding,
then hide its trailing newline too"
  (goto-char beg)
  (let ((line-beg (line-beginning-position))
        (line-end (progn (goto-char end) (line-end-position))))
    (goto-char line-beg)
    (while (< (point) line-end)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (nl-end (min (1+ eol) (point-max))))
        (when (< eol nl-end)
          ;; Clear any stale collapse before recomputing.
          (remove-text-properties eol nl-end '(invisible nil))
          (when (and (> eol bol)
                     (not (text-property-not-all
                           bol eol 'invisible myntax-visibility)))
            (put-text-property eol nl-end 'invisible myntax-visibility))))
      (forward-line 1))
    (cons line-beg line-end)))

(defun myntax-rule-if-parens (limit)
  "Search forward to LIMIT for `if (...)'. On a match, set
match-data with four groups: 1=open paren, 2=leading whitespace
inside it, 3=trailing whitespace inside it, 4=close paren.
The condition itself is left out of the match entirely."
  (catch 'myntax-match
    (while (re-search-forward "\\_<if\\_>[ \t\n]*\\(\(\\)" limit t)
      (let ((open-start (match-beginning 1))
            (open-end (match-end 1)))
        ;; Skip "if" occurring inside a string/comment.
        (unless (nth 8 (syntax-ppss open-start))
          (let ((close-end (condition-case nil
                                (scan-sexps open-start 1)
                              (scan-error nil))))
            (when close-end
              (let* ((close-start (1- close-end))
                     (ws1-end (save-excursion
                                (goto-char open-end)
                                (skip-chars-forward " \t\n" close-start)
                                (point)))
                     (ws2-start (save-excursion
                                  (goto-char close-start)
                                  (skip-chars-backward " \t\n" ws1-end)
                                  (point))))
                (set-match-data
                 (list open-start close-end
                       open-start open-end     ; 1: (
                       open-end   ws1-end       ; 2: leading ws
                       ws2-start  close-start   ; 3: trailing ws
                       close-start close-end))  ; 4: )
                (goto-char close-end)
                (throw 'myntax-match t)))))))
    nil))

(defvar myntax-rule-if-parens-keyword
  `((myntax-rule-if-parens
     (1 '(face nil invisible ,myntax-visibility) prepend)
     (2 '(face nil invisible ,myntax-visibility) prepend)
     (3 '(face nil invisible ,myntax-visibility) prepend)
     (4 '(face nil invisible ,myntax-visibility) prepend))))

;;;###autoload
(define-minor-mode myntax-mode
  "Toggle display of verbose syntax characters such as braces and parens."
  (if myntax-mode
      (progn
        (setq myntax-chars (myntax-chars-for-mode))
        (add-to-invisibility-spec myntax-visibility)
        (font-lock-add-keywords nil myntax-font-lock-keywords)
        (jit-lock-register #'myntax-collapse-blank-lines)
        (font-lock-flush))
    
    (remove-from-invisibility-spec myntax-visibility)
    (font-lock-remove-keywords nil myntax-font-lock-keywords)
    (jit-lock-unregister #'myntax-collapse-blank-lines)
    (font-lock-flush)))

(provide 'myntax)
