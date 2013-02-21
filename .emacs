;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/findr")
;(autoload 'findr-query-replace "findr" "Replace text in files." t)
;(define-key global-map [(meta control r)] 'findr-query-replace)


;; (require 'desire)
;; FIXME: reimplement use-module terms of desire.

(setq font-lock-maximum-decoration '((t 1))) ;; default: use the minimum level

(defun use-module (module-name)
  (load-file (concat (expand-file-name "~/.emacs.d/elisp/00-dot-emacs-modules/")
                     module-name
                     ".el")))

;; (use-module "javascript")




;; (add-hook 'text-mode-hook
;;           (lamba ()
;;             (setq 'indent-tabs-mode nil)))







;; When I split a buffer (C-x 2), don't start both new buffers at the top
;; of the file; just split the buffer in half along the viewport boundary.
(load-file (expand-file-name "~/.emacs.d/elisp/slowsplit/slowsplit.elc"))


;; FIXME:  Figure out why M-s doesn't work right in text buffers, and fix;
;; FIXME:  Move all mode-specific requires to using something else... autoload?
;; FIXME:  Make man's "near point" functionality Respect::Perl::Modules.
;; FIXME:  Add feature:  When I open certain files (e.g. ~/Blog), do
;; (end-of-buffer)
;; FIXME:  Add a hook to run (dirs) after every shell command
;; (because emacs's not being able to figure out the working dir is inexcusably
;; lame)


;; Aquamacs-specific stuff
(if (and (featurep 'aquamacs) (eq aquamacs-version "1.9"))
    ;; To indicate lines that are longer than the buffer is wide,
    ;; show a "fringe" at the edge of the buffer.
    ;; I've chosen to have no fringe (0) at the left and a 24-pixel-wide
    ;; fringe (24) at the right.
    (set-fringe-mode '(0 24))
  (custom-set-variables
   '(custom-file "~/.emacs")
   '(default-frame-alist (width . 80) (height . 45))
   '(aquamacs-styles-mode nil nil (color-theme))
   '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
   '(x-select-enable-clipboard t)
   )
  )

;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/desire"))

;; Use cperl mode instead of the default perl mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;;;;;;;;;;
;; Appearance:  basics
;;
(blink-cursor-mode 0)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 151 t)
 '(aquamacs-default-styles (quote ((default (color-theme color-theme-snapshot ((CUA-mode-read-only-cursor-color . "white") (background-color . "gray80") (background-mode . light) (background-toolbar-color . "#cccccccccccc") (border-color . "#000000000000") (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a") (cua-global-mark-cursor-color . "cyan") (cua-normal-cursor-color . "red") (cua-overwrite-cursor-color . "yellow") (cua-read-only-cursor-color . "darkgreen") (cursor-color . "Red3") (foreground-color . "black") (mouse-color . "black") (semantic-which-function-use-color . t) (senator-eldoc-use-color . t) (top-toolbar-shadow-color . "#f5f5f5f5f5f5") (vc-annotate-very-old-color . "#3F3FFF") (viper-insert-state-cursor-color . "Green") (viper-replace-overlay-cursor-color . "Red") (viper-saved-cursor-color-in-replace-mode . "Red3")) ((Man-overstrike-face . bold) (Man-reverse-face . highlight) (Man-underline-face . underline) (compilation-message-face . underline) (cperl-here-face . font-lock-string-face) (cperl-invalid-face . underline) (cperl-pod-face . font-lock-comment-face) (cperl-pod-head-face . font-lock-variable-name-face) (cua-global-mark-cursor-color . "cyan") (cua-normal-cursor-color . "red") (cua-overwrite-cursor-color . "yellow") (cua-read-only-cursor-color . "darkgreen") (info-lookup-highlight-face . match) (list-matching-lines-buffer-name-face . bold) (list-matching-lines-face . bold) (rmail-highlight-face . font-lock-function-name-face) (vc-annotate-very-old-color . "#3F3FFF") (view-highlight-face . highlight) (widget-mouse-face . highlight)) (default ((t (:bold t :stipple nil :background "gray80" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 140 :width normal :family "bitstream vera sans mono")))) (OrangeRed ((t (nil)))) (antlr-font-lock-keyword-face ((t (nil)))) (antlr-font-lock-literal-face ((t (nil)))) (antlr-font-lock-ruledef-face ((t (nil)))) (antlr-font-lock-ruleref-face ((t (nil)))) (antlr-font-lock-tokendef-face ((t (nil)))) (antlr-font-lock-tokenref-face ((t (nil)))) (aquamacs-variable-width ((t (:height 120 :family "Lucida Grande")))) (bbdb-company ((t (:italic t :slant italic)))) (bbdb-field-name ((t (:bold t :weight bold)))) (bbdb-field-value ((t (nil)))) (bbdb-name ((t (:underline t)))) (bg:erc-color-face0 ((t (nil)))) (bg:erc-color-face1 ((t (nil)))) (bg:erc-color-face10 ((t (nil)))) (bg:erc-color-face11 ((t (nil)))) (bg:erc-color-face12 ((t (nil)))) (bg:erc-color-face13 ((t (nil)))) (bg:erc-color-face14 ((t (nil)))) (bg:erc-color-face15 ((t (nil)))) (bg:erc-color-face2 ((t (nil)))) (bg:erc-color-face3 ((t (nil)))) (bg:erc-color-face4 ((t (nil)))) (bg:erc-color-face5 ((t (nil)))) (bg:erc-color-face6 ((t (nil)))) (bg:erc-color-face7 ((t (nil)))) (bg:erc-color-face8 ((t (nil)))) (bg:erc-color-face9 ((t (nil)))) (blank-newline ((t (:foreground "lightgrey" :weight normal)))) (blank-space-face ((t (:background "LightGray")))) (blank-tab-face ((t (:background "green" :foreground "black")))) (blue ((t (:foreground "blue")))) (bold ((t (:bold t :weight bold)))) (bold-italic ((t (:italic t :bold t :slant italic :weight bold)))) (border ((t (:background "#000000000000")))) (border-glyph ((t (nil)))) (buffer-menu-buffer ((t (:bold t :weight bold)))) (buffers-tab ((t (nil)))) (button ((t (:underline t)))) (calendar-today-face ((t (nil)))) (change-log-acknowledgement-face ((t (nil)))) (change-log-conditionals-face ((t (nil)))) (change-log-date-face ((t (nil)))) (change-log-email-face ((t (nil)))) (change-log-file-face ((t (nil)))) (change-log-function-face ((t (nil)))) (change-log-list-face ((t (nil)))) (change-log-name-face ((t (nil)))) (color-mode-face-@ ((t (nil)))) (color-mode-face-a ((t (nil)))) (color-mode-face-b ((t (nil)))) (color-mode-face-c ((t (nil)))) (color-mode-face-d ((t (nil)))) (color-mode-face-e ((t (nil)))) (color-mode-face-f ((t (nil)))) (color-mode-face-g ((t (nil)))) (color-mode-face-h ((t (nil)))) (comint-highlight-input ((t (:bold t :weight bold)))) (comint-highlight-prompt ((t (:foreground "dark blue")))) (comint-input-face ((t (:foreground "deepskyblue")))) (compilation-column-number ((t (:foreground "ForestGreen")))) (compilation-error ((t (:bold t :weight bold :foreground "Red1")))) (compilation-info ((t (:bold t :foreground "Green3" :weight bold)))) (compilation-line-number ((t (:foreground "DarkGoldenrod")))) (compilation-warning ((t (:bold t :foreground "Orange" :weight bold)))) (completions-common-part ((t (:bold t :family "bitstream vera sans mono" :width normal :weight bold :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "gray80" :stipple nil :height 140)))) (completions-first-difference ((t (:bold t :weight bold)))) (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold)))) (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold)))) (cperl-here-face ((t (:foreground "green4")))) (cperl-invalid-face ((t (nil)))) (cperl-nonoverridable-face ((t (:foreground "chartreuse3")))) (cperl-pod-face ((t (:foreground "brown4")))) (cperl-pod-head-face ((t (:foreground "steelblue")))) (cua-global-mark ((t (:background "yellow1" :foreground "black")))) (cua-rectangle ((t (:background "maroon" :foreground "white")))) (cua-rectangle-noselect ((t (:background "dimgray" :foreground "white")))) (cursor ((t (:background "Red3")))) (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button))))) (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button))))) (custom-button-pressed ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button))))) (custom-button-pressed-unraised ((t (:underline t :foreground "magenta4")))) (custom-button-unraised ((t (:underline t)))) (custom-changed ((t (:background "blue1" :foreground "white")))) (custom-comment ((t (:background "gray85")))) (custom-comment-tag ((t (:foreground "blue4")))) (custom-documentation ((t (nil)))) (custom-face-tag ((t (:bold t :family "helv" :weight bold :height 1.2)))) (custom-group-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2)))) (custom-group-tag-1 ((t (:bold t :family "helv" :foreground "red1" :weight bold :height 1.2)))) (custom-invalid ((t (:background "red1" :foreground "yellow1")))) (custom-link ((t (:underline t :foreground "blue1")))) (custom-modified ((t (:background "blue1" :foreground "white")))) (custom-rogue ((t (:background "black" :foreground "pink")))) (custom-saved ((t (:underline t)))) (custom-set ((t (:background "white" :foreground "blue1")))) (custom-state ((t (:foreground "dark green")))) (custom-themed ((t (:background "blue1" :foreground "white")))) (custom-variable-button ((t (:bold t :underline t :weight bold)))) (custom-variable-tag ((t (:bold t :family "helv" :foreground "blue1" :weight bold :height 1.2)))) (cvs-filename-face ((t (nil)))) (cvs-handled-face ((t (nil)))) (cvs-header-face ((t (nil)))) (cvs-marked-face ((t (nil)))) (cvs-msg-face ((t (nil)))) (cvs-need-action-face ((t (nil)))) (cvs-unknown-face ((t (nil)))) (cyan ((t (nil)))) (diary-face ((t (nil)))) (diff-added ((t (nil)))) (diff-added-face ((t (nil)))) (diff-changed ((t (nil)))) (diff-changed-face ((t (nil)))) (diff-context ((t (:foreground "grey50")))) (diff-context-face ((t (:foreground "grey50")))) (diff-file-header ((t (:bold t :background "grey70" :weight bold)))) (diff-file-header-face ((t (:bold t :background "grey70" :weight bold)))) (diff-function ((t (:background "grey85")))) (diff-function-face ((t (:background "grey85")))) (diff-header ((t (:background "grey85")))) (diff-header-face ((t (:background "grey85")))) (diff-hunk-header ((t (:background "grey85")))) (diff-hunk-header-face ((t (:background "grey85")))) (diff-index ((t (:bold t :background "grey70" :weight bold)))) (diff-index-face ((t (:bold t :background "grey70" :weight bold)))) (diff-indicator-added ((t (nil)))) (diff-indicator-changed ((t (nil)))) (diff-indicator-removed ((t (nil)))) (diff-nonexistent ((t (:bold t :background "grey70" :weight bold)))) (diff-nonexistent-face ((t (:bold t :background "grey70" :weight bold)))) (diff-removed ((t (nil)))) (diff-removed-face ((t (nil)))) (dired-directory ((t (:foreground "Blue1")))) (dired-face-boring ((t (:foreground "Gray65")))) (dired-face-directory ((t (:bold t :weight bold)))) (dired-face-executable ((t (:foreground "SeaGreen")))) (dired-face-flagged ((t (:background "LightSlateGray")))) (dired-face-header ((t (nil)))) (dired-face-marked ((t (:background "PaleVioletRed")))) (dired-face-permissions ((t (:background "grey75" :foreground "black")))) (dired-face-setuid ((t (:foreground "Red")))) (dired-face-socket ((t (:foreground "magenta")))) (dired-face-symlink ((t (:foreground "cyan")))) (dired-flagged ((t (:bold t :weight bold :foreground "Red1")))) (dired-header ((t (:foreground "ForestGreen")))) (dired-ignored ((t (:foreground "grey50")))) (dired-mark ((t (:foreground "CadetBlue")))) (dired-marked ((t (:bold t :weight bold :foreground "Red1")))) (dired-perm-write ((t (:foreground "Firebrick")))) (dired-symlink ((t (:foreground "Purple")))) (dired-warning ((t (:bold t :weight bold :foreground "Red1")))) (display-time-mail-balloon-enhance-face ((t (:background "orange")))) (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue")))) (display-time-time-balloon-face ((t (:foreground "red")))) (ebrowse-default-face ((t (nil)))) (ebrowse-file-name-face ((t (nil)))) (ebrowse-member-attribute-face ((t (nil)))) (ebrowse-member-class-face ((t (nil)))) (ebrowse-progress-face ((t (nil)))) (ebrowse-root-class-face ((t (nil)))) (ebrowse-tree-mark-face ((t (nil)))) (ecb-sources-face ((t (nil)))) (edb-inter-field-face ((t (nil)))) (edb-normal-summary-face ((t (nil)))) (ediff-current-diff-face-A ((t (:background "pale green" :foreground "firebrick")))) (ediff-current-diff-face-Ancestor ((t (:background "VioletRed" :foreground "Black")))) (ediff-current-diff-face-B ((t (:background "Yellow" :foreground "DarkOrchid")))) (ediff-current-diff-face-C ((t (:background "Pink" :foreground "Navy")))) (ediff-even-diff-face-A ((t (:background "light grey" :foreground "Black")))) (ediff-even-diff-face-Ancestor ((t (:background "Grey" :foreground "White")))) (ediff-even-diff-face-B ((t (:background "Grey" :foreground "White")))) (ediff-even-diff-face-C ((t (:background "light grey" :foreground "Black")))) (ediff-fine-diff-face-A ((t (:background "sky blue" :foreground "Navy")))) (ediff-fine-diff-face-Ancestor ((t (:background "Green" :foreground "Black")))) (ediff-fine-diff-face-B ((t (:background "cyan" :foreground "Black")))) (ediff-fine-diff-face-C ((t (:background "Turquoise" :foreground "Black")))) (ediff-odd-diff-face-A ((t (:background "Grey" :foreground "White")))) (ediff-odd-diff-face-Ancestor ((t (:background "light grey" :foreground "Black")))) (ediff-odd-diff-face-B ((t (:background "light grey" :foreground "Black")))) (ediff-odd-diff-face-C ((t (:background "Grey" :foreground "White")))) (emacs-wiki-bad-link-face ((t (nil)))) (emacs-wiki-link-face ((t (nil)))) (erc-action-face ((t (nil)))) (erc-bold-face ((t (nil)))) (erc-current-nick-face ((t (nil)))) (erc-default-face ((t (nil)))) (erc-direct-msg-face ((t (nil)))) (erc-error-face ((t (nil)))) (erc-highlight-face ((t (nil)))) (erc-input-face ((t (nil)))) (erc-inverse-face ((t (nil)))) (erc-keyword-face ((t (nil)))) (erc-notice-face ((t (nil)))) (erc-pal-face ((t (nil)))) (erc-prompt-face ((t (nil)))) (erc-timestamp-face ((t (nil)))) (erc-underline-face ((t (nil)))) (escape-glyph ((t (:foreground "brown")))) (eshell-ls-archive-face ((t (nil)))) (eshell-ls-backup-face ((t (nil)))) (eshell-ls-clutter-face ((t (nil)))) (eshell-ls-directory-face ((t (nil)))) (eshell-ls-executable-face ((t (nil)))) (eshell-ls-missing-face ((t (nil)))) (eshell-ls-picture-face ((t (nil)))) (eshell-ls-product-face ((t (nil)))) (eshell-ls-readonly-face ((t (nil)))) (eshell-ls-special-face ((t (nil)))) (eshell-ls-symlink-face ((t (nil)))) (eshell-ls-text-face ((t (nil)))) (eshell-ls-todo-face ((t (nil)))) (eshell-ls-unreadable-face ((t (nil)))) (eshell-prompt-face ((t (nil)))) (eshell-test-failed-face ((t (nil)))) (eshell-test-ok-face ((t (nil)))) (excerpt ((t (nil)))) (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue" :weight bold)))) (fg:black ((t (nil)))) (fg:erc-color-face0 ((t (nil)))) (fg:erc-color-face1 ((t (nil)))) (fg:erc-color-face10 ((t (nil)))) (fg:erc-color-face11 ((t (nil)))) (fg:erc-color-face12 ((t (nil)))) (fg:erc-color-face13 ((t (nil)))) (fg:erc-color-face14 ((t (nil)))) (fg:erc-color-face15 ((t (nil)))) (fg:erc-color-face2 ((t (nil)))) (fg:erc-color-face3 ((t (nil)))) (fg:erc-color-face4 ((t (nil)))) (fg:erc-color-face5 ((t (nil)))) (fg:erc-color-face6 ((t (nil)))) (fg:erc-color-face7 ((t (nil)))) (fg:erc-color-face8 ((t (nil)))) (fg:erc-color-face9 ((t (nil)))) (file-name-shadow ((t (:foreground "grey50")))) (fixed ((t (nil)))) (fixed-pitch ((t (:family "courier")))) (fl-comment-face ((t (nil)))) (fl-doc-string-face ((t (nil)))) (fl-function-name-face ((t (nil)))) (fl-keyword-face ((t (nil)))) (fl-string-face ((t (nil)))) (fl-type-face ((t (nil)))) (flyspell-duplicate-face ((t (nil)))) (flyspell-incorrect-face ((t (nil)))) (font-latex-bold-face ((t (:bold t :foreground "OliveDrab" :weight bold)))) (font-latex-italic-face ((t (:italic t :foreground "OliveDrab" :slant italic)))) (font-latex-math-face ((t (:foreground "burlywood")))) (font-latex-sedate-face ((t (:foreground "LightGray")))) (font-latex-string-face ((t (:foreground "LightSalmon")))) (font-latex-warning-face ((t (:foreground "Pink")))) (font-lock-builtin-face ((t (:foreground "Orchid")))) (font-lock-comment-delimiter-face ((t (:foreground "Firebrick")))) (font-lock-comment-face ((t (:foreground "Firebrick")))) (font-lock-constant-face ((t (:foreground "CadetBlue")))) (font-lock-doc-face ((t (:foreground "RosyBrown")))) (font-lock-doc-string-face ((t (:foreground "mediumvioletred")))) (font-lock-emphasized-face ((t (:bold t :background "lightyellow2" :weight bold)))) (font-lock-exit-face ((t (nil)))) (font-lock-function-name-face ((t (:foreground "Blue1")))) (font-lock-keyword-face ((t (:foreground "Purple")))) (font-lock-negation-char-face ((t (nil)))) (font-lock-other-emphasized-face ((t (:italic t :bold t :background "lightyellow2" :slant italic :weight bold)))) (font-lock-other-type-face ((t (:bold t :foreground "orange3" :weight bold)))) (font-lock-preprocessor-face ((t (:foreground "Orchid")))) (font-lock-reference-face ((t (:foreground "red3")))) (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold)))) (font-lock-regexp-grouping-construct ((t (:bold t :weight bold)))) (font-lock-string-face ((t (:foreground "RosyBrown")))) (font-lock-type-face ((t (:foreground "ForestGreen")))) (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod")))) (font-lock-warning-face ((t (:bold t :foreground "Red1" :weight bold)))) (fringe ((t (:background "grey95")))) (gnu-cite-face-3 ((t (nil)))) (gnu-cite-face-4 ((t (nil)))) (gnus-cite-attribution-face ((t (:italic t :slant italic)))) (gnus-cite-face-1 ((t (:foreground "MidnightBlue")))) (gnus-cite-face-10 ((t (:foreground "medium purple")))) (gnus-cite-face-11 ((t (:foreground "turquoise")))) (gnus-cite-face-2 ((t (:foreground "firebrick")))) (gnus-cite-face-3 ((t (:foreground "dark green")))) (gnus-cite-face-4 ((t (:foreground "OrangeRed")))) (gnus-cite-face-5 ((t (:foreground "dark khaki")))) (gnus-cite-face-6 ((t (:foreground "dark violet")))) (gnus-cite-face-7 ((t (:foreground "SteelBlue4")))) (gnus-cite-face-8 ((t (:foreground "magenta")))) (gnus-cite-face-9 ((t (:foreground "violet")))) (gnus-emphasis-bold ((t (:bold t :weight bold)))) (gnus-emphasis-bold-italic ((t (:bold t :weight bold)))) (gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow")))) (gnus-emphasis-italic ((t (:italic t :slant italic)))) (gnus-emphasis-underline ((t (:underline t)))) (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold)))) (gnus-emphasis-underline-bold-italic ((t (:bold t :underline t :weight bold)))) (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic)))) (gnus-filterhist-face-1 ((t (nil)))) (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3")))) (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3" :weight bold)))) (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3")))) (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3" :weight bold)))) (gnus-group-mail-3-empty-face ((t (:foreground "magenta4")))) (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4" :weight bold)))) (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4")))) (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4" :weight bold)))) (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen")))) (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen" :weight bold)))) (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4")))) (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4" :weight bold)))) (gnus-group-news-3-empty-face ((t (nil)))) (gnus-group-news-3-face ((t (:bold t :weight bold)))) (gnus-group-news-4-empty-face ((t (nil)))) (gnus-group-news-4-face ((t (:bold t :weight bold)))) (gnus-group-news-5-empty-face ((t (nil)))) (gnus-group-news-5-face ((t (:bold t :weight bold)))) (gnus-group-news-6-empty-face ((t (nil)))) (gnus-group-news-6-face ((t (:bold t :weight bold)))) (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen")))) (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen" :weight bold)))) (gnus-header-content-face ((t (:italic t :foreground "indianred4" :slant italic)))) (gnus-header-from-face ((t (:foreground "red3")))) (gnus-header-name-face ((t (:foreground "maroon")))) (gnus-header-newsgroups-face ((t (:italic t :foreground "MidnightBlue" :slant italic)))) (gnus-header-subject-face ((t (:foreground "red4")))) (gnus-picon-face ((t (nil)))) (gnus-picon-xbm-face ((t (nil)))) (gnus-picons-face ((t (:background "white" :foreground "black")))) (gnus-picons-xbm-face ((t (:background "white" :foreground "black")))) (gnus-signature-face ((t (:italic t :slant italic)))) (gnus-splash ((t (nil)))) (gnus-splash-face ((t (:foreground "Brown")))) (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow")))) (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue" :weight bold)))) (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold)))) (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick" :weight bold)))) (gnus-summary-high-unread-face ((t (:bold t :weight bold)))) (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue" :slant italic)))) (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen" :slant italic)))) (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick" :slant italic)))) (gnus-summary-low-unread-face ((t (:italic t :slant italic)))) (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue")))) (gnus-summary-normal-read-face ((t (:foreground "DarkGreen")))) (gnus-summary-normal-ticked-face ((t (:foreground "firebrick")))) (gnus-summary-normal-unread-face ((t (nil)))) (gnus-summary-selected-face ((t (:underline t)))) (gnus-x-face ((t (:background "white" :foreground "black")))) (green ((t (:foreground "green")))) (gui-button-face ((t (:background "grey75" :foreground "black")))) (gui-element ((t (nil)))) (header-line ((t (:width normal :weight normal :slant normal :underline nil :strike-through nil :box (:line-width -1 :style released-button) :family "Lucida Grande" :background "grey90" :foreground "grey20" :box nil :height 120)))) (help-argument-name ((t (:italic t :slant italic)))) (hi-black-b ((t (nil)))) (hi-black-hb ((t (nil)))) (hi-blue ((t (nil)))) (hi-blue-b ((t (nil)))) (hi-green ((t (nil)))) (hi-green-b ((t (nil)))) (hi-pink ((t (nil)))) (hi-red-b ((t (nil)))) (hi-yellow ((t (nil)))) (highlight ((t (:background "darkseagreen2")))) (highlight-changes-delete-face ((t (nil)))) (highlight-changes-face ((t (nil)))) (highline-face ((t (nil)))) (holiday-face ((t (nil)))) (html-helper-bold-face ((t (nil)))) (html-helper-bold-italic-face ((t (nil)))) (html-helper-builtin-face ((t (nil)))) (html-helper-italic-face ((t (nil)))) (html-helper-underline-face ((t (nil)))) (html-tag-face ((t (nil)))) (hyper-apropos-documentation ((t (nil)))) (hyper-apropos-heading ((t (nil)))) (hyper-apropos-hyperlink ((t (nil)))) (hyper-apropos-major-heading ((t (nil)))) (hyper-apropos-section-heading ((t (nil)))) (hyper-apropos-warning ((t (nil)))) (ibuffer-marked-face ((t (nil)))) (idlwave-help-link-face ((t (nil)))) (idlwave-shell-bp-face ((t (nil)))) (ido-subdir-face ((t (nil)))) (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown")))) (info-header-xref ((t (:foreground "blue1" :underline t)))) (info-menu-6 ((t (nil)))) (info-menu-header ((t (:bold t :family "helv" :weight bold)))) (info-menu-star ((t (:foreground "red1")))) (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold)))) (info-title-1 ((t (:bold t :weight bold :family "helv" :height 1.728)))) (info-title-2 ((t (:bold t :family "helv" :weight bold :height 1.44)))) (info-title-3 ((t (:bold t :weight bold :family "helv" :height 1.2)))) (info-title-4 ((t (:bold t :family "helv" :weight bold)))) (info-xref ((t (:underline t :foreground "blue1")))) (info-xref-visited ((t (:foreground "magenta4" :underline t)))) (isearch ((t (:background "magenta3" :foreground "lightskyblue1")))) (isearch-secondary ((t (:foreground "red3")))) (ispell-face ((t (:bold t :background "#3454b4" :foreground "yellow" :weight bold)))) (italic ((t (:italic t :slant italic)))) (jde-bug-breakpoint-cursor ((t (nil)))) (jde-bug-breakpoint-marker ((t (nil)))) (jde-db-active-breakpoint-face ((t (nil)))) (jde-db-requested-breakpoint-face ((t (nil)))) (jde-db-spec-breakpoint-face ((t (nil)))) (jde-java-font-lock-api-face ((t (nil)))) (jde-java-font-lock-bold-face ((t (nil)))) (jde-java-font-lock-code-face ((t (nil)))) (jde-java-font-lock-constant-face ((t (nil)))) (jde-java-font-lock-doc-tag-face ((t (nil)))) (jde-java-font-lock-italic-face ((t (nil)))) (jde-java-font-lock-link-face ((t (nil)))) (jde-java-font-lock-modifier-face ((t (nil)))) (jde-java-font-lock-number-face ((t (nil)))) (jde-java-font-lock-operator-face ((t (nil)))) (jde-java-font-lock-package-face ((t (nil)))) (jde-java-font-lock-pre-face ((t (nil)))) (jde-java-font-lock-underline-face ((t (nil)))) (js2-error-face ((t (:foreground "red")))) (js2-external-variable-face ((t (:foreground "orange")))) (js2-function-param-face ((t (:foreground "SeaGreen")))) (js2-instance-member-face ((t (:foreground "DarkOrchid")))) (js2-jsdoc-html-tag-delimiter-face ((t (:foreground "dark khaki")))) (js2-jsdoc-html-tag-name-face ((t (:foreground "rosybrown")))) (js2-jsdoc-tag-face ((t (:foreground "SlateGray")))) (js2-jsdoc-type-face ((t (:foreground "SteelBlue")))) (js2-jsdoc-value-face ((t (:foreground "PeachPuff3")))) (js2-magic-paren-face ((t (:underline t)))) (js2-private-function-call-face ((t (:foreground "goldenrod")))) (js2-private-member-face ((t (:foreground "PeachPuff3")))) (js2-warning-face ((t (:underline "orange")))) (kai-gnus-cite-face-1 ((t (nil)))) (kai-gnus-cite-face-2 ((t (nil)))) (kai-gnus-cite-face-3 ((t (nil)))) (kai-gnus-group-mail-face ((t (nil)))) (kai-gnus-group-nonempty-mail-face ((t (nil)))) (kai-gnus-group-starred-face ((t (nil)))) (lazy-highlight ((t (:background "paleturquoise")))) (lazy-highlight-face ((t (nil)))) (left-margin ((t (nil)))) (linemenu-face ((t (nil)))) (link ((t (:foreground "blue1" :underline t)))) (link-visited ((t (:underline t :foreground "magenta4")))) (list-mode-item-selected ((t (:background "gray68" :foreground "black")))) (log-view-file-face ((t (nil)))) (log-view-message-face ((t (nil)))) (mac-ts-block-fill-text ((t (:underline t)))) (mac-ts-caret-position ((t (nil)))) (mac-ts-converted-text ((t (:underline "gray80")))) (mac-ts-no-hilite ((t (:bold t :family "bitstream vera sans mono" :width normal :weight bold :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "gray80" :stipple nil :height 140)))) (mac-ts-outline-text ((t (:underline t)))) (mac-ts-raw-text ((t (:underline t)))) (mac-ts-selected-converted-text ((t (:underline t)))) (mac-ts-selected-raw-text ((t (:underline t)))) (mac-ts-selected-text ((t (:underline t)))) (magenta ((t (nil)))) (makefile-space-face ((t (:background "hotpink")))) (man-bold ((t (nil)))) (man-heading ((t (nil)))) (man-italic ((t (nil)))) (man-xref ((t (nil)))) (match ((t (:background "yellow1")))) (menu ((t (nil)))) (message-cited-text ((t (:foreground "slategrey")))) (message-cited-text-face ((t (:foreground "red")))) (message-header-cc-face ((t (:foreground "MidnightBlue")))) (message-header-contents ((t (:italic t :slant italic)))) (message-header-name-face ((t (:foreground "cornflower blue")))) (message-header-newsgroups-face ((t (:bold t :foreground "blue4" :weight bold)))) (message-header-other-face ((t (:foreground "steel blue")))) (message-header-subject-face ((t (:bold t :foreground "navy blue" :weight bold)))) (message-header-to-face ((t (:bold t :foreground "MidnightBlue" :weight bold)))) (message-header-xheader-face ((t (:foreground "blue")))) (message-headers ((t (:bold t :weight bold)))) (message-highlighted-header-contents ((t (:bold t :weight bold)))) (message-mml-face ((t (:foreground "ForestGreen")))) (message-separator-face ((t (:foreground "brown")))) (message-url ((t (:bold t :weight bold)))) (minibuffer-prompt ((t (:foreground "medium blue")))) (mmm-cleanup-submode-face ((t (:background "Wheat")))) (mmm-code-submode-face ((t (:background "LightGray")))) (mmm-comment-submode-face ((t (:background "SkyBlue")))) (mmm-declaration-submode-face ((t (:background "Aquamarine")))) (mmm-default-submode-face ((t (:background "gray85")))) (mmm-delimiter-face ((t (nil)))) (mmm-face ((t (nil)))) (mmm-init-submode-face ((t (:background "Pink")))) (mmm-output-submode-face ((t (:background "Plum")))) (mmm-special-submode-face ((t (:background "MediumSpringGreen")))) (mode-line ((t (:family "Lucida Grande" :background "grey75" :foreground "black" :box (:line-width -1 :style released-button) :strike-through nil :underline nil :slant normal :weight normal :width normal :height 120)))) (mode-line-buffer-id ((t (:bold t :weight bold)))) (mode-line-flags ((t (:family "Monaco")))) (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button))))) (mode-line-inactive ((t (:family "Lucida Grande" :background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :strike-through nil :underline nil :slant normal :weight normal :width normal :height 120)))) (modeline-mousable ((t (:background "Gray80" :foreground "firebrick")))) (modeline-mousable-minor-mode ((t (:background "Gray80" :foreground "green4")))) (mouse ((t (:background "black")))) (mpg123-face-cur ((t (nil)))) (mpg123-face-slider ((t (nil)))) (my-summary-highlight-face ((t (nil)))) (my-url-face ((t (nil)))) (next-error ((t (:background "lightgoldenrod2")))) (nil ((t (nil)))) (nobreak-space ((t (:foreground "brown" :underline t)))) (notify-user-of-mode ((t (:foreground "dark blue")))) (nxml-attribute-colon-face ((t (:foreground "#257A25")))) (nxml-attribute-local-name-face ((t (:foreground "#257A25")))) (nxml-attribute-prefix-face ((t (:foreground "#257A25")))) (nxml-attribute-value-delimiter-face ((t (:foreground "#9292C9")))) (nxml-attribute-value-face ((t (:foreground "#3A3A7B")))) (nxml-cdata-section-CDATA-face ((t (:foreground "#257A25")))) (nxml-cdata-section-content-face ((t (nil)))) (nxml-cdata-section-delimiter-face ((t (:foreground "#9292C9")))) (nxml-char-ref-delimiter-face ((t (:foreground "#9292C9")))) (nxml-char-ref-number-face ((t (:foreground "#9292C9")))) (nxml-comment-content-face ((t (:italic t :slant italic)))) (nxml-comment-delimiter-face ((t (:foreground "#9292C9")))) (nxml-delimited-data-face ((t (:foreground "#3A3A7B")))) (nxml-delimiter-face ((t (:foreground "#9292C9")))) (nxml-element-colon-face ((t (:foreground "#257A25")))) (nxml-element-local-name-face ((t (:foreground "#257A25")))) (nxml-element-prefix-face ((t (:foreground "#257A25")))) (nxml-entity-ref-delimiter-face ((t (:foreground "#9292C9")))) (nxml-entity-ref-name-face ((t (:foreground "#9292C9")))) (nxml-glyph-face ((t (:background "light grey" :foreground "black" :slant normal :weight normal)))) (nxml-hash-face ((t (:foreground "#257A25")))) (nxml-heading-face ((t (:bold t :weight bold)))) (nxml-markup-declaration-delimiter-face ((t (:foreground "#9292C9")))) (nxml-name-face ((t (:foreground "#257A25")))) (nxml-namespace-attribute-colon-face ((t (:foreground "#257A25")))) (nxml-namespace-attribute-prefix-face ((t (:foreground "#257A25")))) (nxml-namespace-attribute-value-delimiter-face ((t (:foreground "#9292C9")))) (nxml-namespace-attribute-value-face ((t (:foreground "#3A3A7B")))) (nxml-namespace-attribute-xmlns-face ((t (:foreground "#257A25")))) (nxml-outline-active-indicator-face ((t (:bold t :stipple nil :background "gray80" :foreground "black" :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :width normal :family "bitstream vera sans mono" :box 1 :height 140)))) (nxml-outline-ellipsis-face ((t (:bold t :family "bitstream vera sans mono" :width normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "gray80" :stipple nil :weight bold :height 140)))) (nxml-outline-indicator-face ((t (:bold t :family "bitstream vera sans mono" :width normal :weight bold :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "gray80" :stipple nil :height 140)))) (nxml-processing-instruction-content-face ((t (:foreground "#3A3A7B")))) (nxml-processing-instruction-delimiter-face ((t (:foreground "#9292C9")))) (nxml-processing-instruction-target-face ((t (:foreground "#257A25")))) (nxml-prolog-keyword-face ((t (:foreground "#257A25")))) (nxml-prolog-literal-content-face ((t (:foreground "#3A3A7B")))) (nxml-prolog-literal-delimiter-face ((t (:foreground "#9292C9")))) (nxml-ref-face ((t (:foreground "#9292C9")))) (nxml-tag-delimiter-face ((t (:foreground "#9292C9")))) (nxml-tag-slash-face ((t (:foreground "#257A25")))) (nxml-text-face ((t (nil)))) (overlay-empty-face ((t (nil)))) (p4-depot-added-face ((t (nil)))) (p4-depot-deleted-face ((t (nil)))) (p4-depot-unmapped-face ((t (nil)))) (p4-diff-change-face ((t (nil)))) (p4-diff-del-face ((t (nil)))) (p4-diff-file-face ((t (nil)))) (p4-diff-head-face ((t (nil)))) (p4-diff-ins-face ((t (nil)))) (pabbrev-debug-display-label-face ((t (:underline "navy")))) (pabbrev-suggestions-face ((t (:foreground "ForestGreen")))) (pabbrev-suggestions-label-face ((t (nil)))) (paren-blink-off ((t (:foreground "lightgrey")))) (paren-face-match ((t (:background "turquoise")))) (paren-face-mismatch ((t (:background "purple" :foreground "white")))) (paren-face-no-match ((t (:background "yellow" :foreground "black")))) (paren-match ((t (:background "darkseagreen2")))) (paren-mismatch ((t (:background "DeepPink" :foreground "black")))) (paren-mismatch-face ((t (:background "DeepPink")))) (paren-no-match-face ((t (:background "yellow")))) (pointer ((t (nil)))) (primary-selection ((t (:background "gray65")))) (py-builtins-face ((t (:foreground "Purple")))) (py-decorators-face ((t (:foreground "Purple")))) (py-pseudo-keyword-face ((t (:foreground "Purple")))) (qt-classes-face ((t (nil)))) (query-replace ((t (:foreground "lightskyblue1" :background "magenta3")))) (reb-match-0 ((t (nil)))) (reb-match-1 ((t (nil)))) (reb-match-2 ((t (nil)))) (reb-match-3 ((t (nil)))) (red ((t (:foreground "red")))) (region ((t (:background "lightgoldenrod2")))) (right-margin ((t (nil)))) (rng-error-face ((t (:underline "red")))) (rpm-spec-dir-face ((t (nil)))) (rpm-spec-doc-face ((t (nil)))) (rpm-spec-ghost-face ((t (nil)))) (rpm-spec-macro-face ((t (nil)))) (rpm-spec-package-face ((t (nil)))) (rpm-spec-tag-face ((t (nil)))) (rpm-spec-var-face ((t (nil)))) (scroll-bar ((t (:background "grey75" :foreground "#000000")))) (searchm-buffer ((t (:bold t :background "white" :foreground "red" :weight bold)))) (searchm-button ((t (:bold t :background "CadetBlue" :foreground "white" :weight bold)))) (searchm-field ((t (:background "grey89")))) (searchm-field-label ((t (:bold t :weight bold)))) (searchm-highlight ((t (:bold t :background "darkseagreen2" :foreground "black" :weight bold)))) (secondary-selection ((t (:background "yellow1")))) (semantic-dirty-token-face ((t (nil)))) (semantic-intangible-face ((t (nil)))) (semantic-read-only-face ((t (nil)))) (semantic-unmatched-syntax-face ((t (nil)))) (senator-intangible-face ((t (nil)))) (senator-momentary-highlight-face ((t (nil)))) (senator-read-only-face ((t (nil)))) (sgml-comment-face ((t (nil)))) (sgml-doctype-face ((t (nil)))) (sgml-end-tag-face ((t (nil)))) (sgml-entity-face ((t (nil)))) (sgml-ignored-face ((t (nil)))) (sgml-ms-end-face ((t (nil)))) (sgml-ms-start-face ((t (nil)))) (sgml-pi-face ((t (nil)))) (sgml-sgml-face ((t (nil)))) (sgml-short-ref-face ((t (nil)))) (sgml-shortref-face ((t (nil)))) (sgml-start-tag-face ((t (nil)))) (sh-escaped-newline ((t (:foreground "RosyBrown")))) (sh-heredoc ((t (:foreground "tan")))) (sh-heredoc-face ((t (:foreground "tan")))) (sh-quoted-exec ((t (:foreground "magenta")))) (shadow ((t (:foreground "grey50")))) (shell-option-face ((t (:background "darkblue" :foreground "cyan2")))) (shell-output-2-face ((t (:background "darkblue" :foreground "darkseagreen")))) (shell-output-3-face ((t (:background "darkblue" :foreground "lightgrey")))) (shell-output-face ((t (:background "darkblue" :foreground "white")))) (shell-prompt-face ((t (:background "darkblue" :foreground "red")))) (show-paren-match ((t (:background "turquoise")))) (show-paren-mismatch ((t (:background "purple" :foreground "white")))) (show-tabs-space-face ((t (nil)))) (show-tabs-tab-face ((t (nil)))) (smerge-base-face ((t (nil)))) (smerge-markers-face ((t (nil)))) (smerge-mine-face ((t (nil)))) (smerge-other-face ((t (nil)))) (speedbar-button-face ((t (nil)))) (speedbar-directory-face ((t (nil)))) (speedbar-file-face ((t (nil)))) (speedbar-highlight-face ((t (nil)))) (speedbar-selected-face ((t (nil)))) (speedbar-separator-face ((t (nil)))) (speedbar-tag-face ((t (nil)))) (strokes-char-face ((t (nil)))) (swbuff-current-buffer-face ((t (nil)))) (template-message-face ((t (:background "pink")))) (term-black ((t (nil)))) (term-blackbg ((t (nil)))) (term-blue ((t (nil)))) (term-blue-bold-face ((t (nil)))) (term-blue-face ((t (nil)))) (term-blue-inv-face ((t (nil)))) (term-blue-ul-face ((t (nil)))) (term-bluebg ((t (nil)))) (term-bold ((t (nil)))) (term-cyan ((t (nil)))) (term-cyan-bold-face ((t (nil)))) (term-cyan-face ((t (nil)))) (term-cyan-inv-face ((t (nil)))) (term-cyan-ul-face ((t (nil)))) (term-cyanbg ((t (nil)))) (term-default ((t (nil)))) (term-default-bg ((t (nil)))) (term-default-bg-inv ((t (nil)))) (term-default-bold-face ((t (nil)))) (term-default-face ((t (nil)))) (term-default-fg ((t (nil)))) (term-default-fg-inv ((t (nil)))) (term-default-inv-face ((t (nil)))) (term-default-ul-face ((t (nil)))) (term-green ((t (nil)))) (term-green-bold-face ((t (nil)))) (term-green-face ((t (nil)))) (term-green-inv-face ((t (nil)))) (term-green-ul-face ((t (nil)))) (term-greenbg ((t (nil)))) (term-invisible ((t (nil)))) (term-invisible-inv ((t (nil)))) (term-magenta ((t (nil)))) (term-magenta-bold-face ((t (nil)))) (term-magenta-face ((t (nil)))) (term-magenta-inv-face ((t (nil)))) (term-magenta-ul-face ((t (nil)))) (term-magentabg ((t (nil)))) (term-red ((t (nil)))) (term-red-bold-face ((t (nil)))) (term-red-face ((t (nil)))) (term-red-inv-face ((t (nil)))) (term-red-ul-face ((t (nil)))) (term-redbg ((t (nil)))) (term-underline ((t (nil)))) (term-white ((t (nil)))) (term-white-bold-face ((t (nil)))) (term-white-face ((t (nil)))) (term-white-inv-face ((t (nil)))) (term-white-ul-face ((t (nil)))) (term-whitebg ((t (nil)))) (term-yellow ((t (nil)))) (term-yellow-bold-face ((t (nil)))) (term-yellow-face ((t (nil)))) (term-yellow-inv-face ((t (nil)))) (term-yellow-ul-face ((t (nil)))) (term-yellowbg ((t (nil)))) (tex-math-face ((t (nil)))) (texinfo-heading-face ((t (nil)))) (text-cursor ((t (:background "Red3" :foreground "gray80")))) (tool-bar ((t (:background "darkslategray")))) (tooltip ((t (:background "lightyellow" :foreground "black" :height 100 :family "lucida sans")))) (trailing-whitespace ((t (:background "red1")))) (underline ((t (:underline t)))) (variable-pitch ((t (:family "helv")))) (vc-annotate-face-0046FF ((t (nil)))) (vcursor ((t (nil)))) (vertical-border ((t (nil)))) (vertical-divider ((t (nil)))) (vhdl-font-lock-attribute-face ((t (nil)))) (vhdl-font-lock-directive-face ((t (nil)))) (vhdl-font-lock-enumvalue-face ((t (nil)))) (vhdl-font-lock-function-face ((t (nil)))) (vhdl-font-lock-prompt-face ((t (nil)))) (vhdl-font-lock-reserved-words-face ((t (nil)))) (vhdl-font-lock-translate-off-face ((t (nil)))) (vhdl-speedbar-architecture-face ((t (nil)))) (vhdl-speedbar-architecture-selected-face ((t (nil)))) (vhdl-speedbar-configuration-face ((t (nil)))) (vhdl-speedbar-configuration-selected-face ((t (nil)))) (vhdl-speedbar-entity-face ((t (nil)))) (vhdl-speedbar-entity-selected-face ((t (nil)))) (vhdl-speedbar-instantiation-face ((t (nil)))) (vhdl-speedbar-instantiation-selected-face ((t (nil)))) (vhdl-speedbar-package-face ((t (nil)))) (vhdl-speedbar-package-selected-face ((t (nil)))) (viper-minibuffer-emacs-face ((t (:background "gray80" :foreground "black")))) (viper-minibuffer-insert-face ((t (:background "gray80" :foreground "black")))) (viper-minibuffer-vi-face ((t (:background "gray80" :foreground "black")))) (viper-replace-overlay-face ((t (:background "black" :foreground "white")))) (viper-search-face ((t (:background "black" :foreground "white")))) (vm-header-content-face ((t (nil)))) (vm-header-from-face ((t (nil)))) (vm-header-name-face ((t (nil)))) (vm-header-subject-face ((t (nil)))) (vm-header-to-face ((t (nil)))) (vm-message-cited-face ((t (nil)))) (vm-summary-face-1 ((t (nil)))) (vm-summary-face-2 ((t (nil)))) (vm-summary-face-3 ((t (nil)))) (vm-summary-face-4 ((t (nil)))) (vm-summary-highlight-face ((t (nil)))) (vm-xface ((t (nil)))) (vmpc-pre-sig-face ((t (nil)))) (vmpc-sig-face ((t (nil)))) (vvb-face ((t (:background "pink" :foreground "black")))) (w3m-anchor-face ((t (nil)))) (w3m-arrived-anchor-face ((t (nil)))) (w3m-header-line-location-content-face ((t (nil)))) (w3m-header-line-location-title-face ((t (nil)))) (white ((t (nil)))) (widget ((t (nil)))) (widget-button ((t (:bold t :weight bold)))) (widget-button-pressed ((t (:foreground "red1")))) (widget-documentation ((t (:foreground "dark green")))) (widget-field ((t (:background "gray85")))) (widget-inactive ((t (:foreground "grey50")))) (widget-single-line-field ((t (:background "gray85")))) (woman-addition-face ((t (nil)))) (woman-bold-face ((t (nil)))) (woman-italic-face ((t (nil)))) (woman-unknown-face ((t (nil)))) (x-face ((t (:background "white" :foreground "black")))) (x-symbol-adobe-fontspecific-face ((t (nil)))) (x-symbol-face ((t (nil)))) (x-symbol-heading-face ((t (:bold t :foreground "green4" :underline t :weight bold)))) (x-symbol-info-face ((t (:foreground "green4")))) (x-symbol-invisible-face ((t (nil)))) (x-symbol-revealed-face ((t (:background "pink")))) (xrdb-option-name-face ((t (nil)))) (xref-keyword-face ((t (nil)))) (xref-list-default-face ((t (nil)))) (xref-list-pilot-face ((t (nil)))) (xref-list-symbol-face ((t (nil)))) (xxml-emph-1-face ((t (nil)))) (xxml-emph-2-face ((t (nil)))) (xxml-header-1-face ((t (nil)))) (xxml-header-2-face ((t (nil)))) (xxml-header-3-face ((t (nil)))) (xxml-header-4-face ((t (nil)))) (xxml-interaction-face ((t (nil)))) (xxml-rug-face ((t (nil)))) (xxml-sparkle-face ((t (nil)))) (xxml-unbreakable-space-face ((t (nil)))) (yaml-tab-face ((t (:bold t :background "red" :foreground "red" :weight bold)))) (yellow ((t (:foreground "yellow")))) (zmacs-region ((t (:background "black" :foreground "white"))))) (font . "-apple-bitstream vera sans mono-bold-r-normal--14-0-72-72-m-0-iso10646-1") (tool-bar-lines . 0)))))
 '(aquamacs-styles-mode t nil (color-theme))
 '(column-number-mode nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 '(cursor-type (quote box))
 '(custom-file "~/.emacs")
 '(default-frame-alist (width . 80))
 '(default-input-method "greek-babel")
 '(desktop-load-locked-desktop t)
 '(fill-column 78)
 '(font-lock-maximum-decoration ((shell-mode nil) (t . t)))
 '(global-font-lock-mode nil)
 '(inhibit-startup-screen t)
 '(js2-auto-indent-p nil)
 '(js2-highlight-level 1)
 '(mac-option-modifier nil)
 '(one-buffer-one-frame-mode nil nil (aquamacs-frame-setup))
 '(perlnow-minimum-perl-version "v5.8" t)
 '(perlnow-pm-location "/home/quinn/lib")
 '(perlnow-test-policy-dot-definition "incspot")
 '(perlnow-test-policy-naming-style "hyphenized")
 '(perlnow-test-policy-test-location "~/t")
 '(text-mode-hook (quote ((lambda nil (local-set-key "\363" (quote shell))))))
 '(transient-mark-mode t)
 '(x-select-enable-clipboard t))
(setq-default visible-bell 't)
(require 'tool-bar)
(tool-bar-mode -1)
(require 'menu-bar)
(menu-bar-mode -1)
(require 'scroll-bar)
(scroll-bar-mode -1)

;;;;;;;;;;
;; Appearance:  colors, fonts, and window sizes
;;

;; Fonts
;;
;; My favorite fixed-width font is Bitstream Vera Sans Mono,
;; a TrueType font that Bitstream gives away for free.
;; It is available in various places on the Internet, e.g. here:
;; http://www.gnome.org/fonts/
;;
;; Under X11, TrueType fonts are set in ~/.Xresources, which see.
;;
;; Under Aquamacs (the awesome Mac OS X native port of Emacs),
;; you can still use TrueType fonts, but you have to do it manually:
;; Go to Options -> Show Fonts (this Frame), then set the font
;; using the nice Mac GUI, then select Options -> Frame Appearance
;; Styles -> Use Current Style as Default, and then select Options ->
;; Save Options.

;; Unfortunately, GNU emacs can't really do computations with fonts, so it can't
;; figure out on its own how many lines of text in a given font will fill up
;; the screen).
(defun maximize-vertically ()
  (set-frame-size (selected-frame) 80 57))


;; (define
;;     afternoon
;;     (lambda ()
;;       (set-background-color "")
;;       (set-foreground-color "")))


;; Note that this requires that you first install the Bitstream Vera fonts,
;; which are freely available on the Web.

;; Update:  I now 
;;
;; Set my color theme.  The goals are easy-on-the-eyes readability and
;; esthetics, in that order.
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/color-theme"))
;(require 'color-theme)
;(color-theme-initialize)

;; My favorite color theme, an XEmacs clone with a slightly darker modeline.
;; Its grey background makes text very easy on the eyes.
;(load (expand-file-name "~/.emacs.d/elisp/LOCAL/my-theme.elc"))
;(color-theme-xemacs-tweaked)

;; Other good color themes:
;(color-theme-xemacs)                 ;; high-contrast, yet not too bright
;(set-background-color "FloralWhite") ;; tweak with lighter background
;(set-background-color "WhiteSmoke")  ;; tweak with lighter background
;
;(color-theme-standard) ;; high-contrast, but white bg is a little too bright.
;(color-theme-gray30)  ;; easy on the eyes, but grey background is depressing.
;(color-theme-classic) ;; easy on the eyes, but grey background is depressing.

;; Color themes that are close, but a little too hard on the eyes:
;(color-theme-snowish)
;(color-theme-deep-blue)
;(color-theme-gnome)
;(color-theme-subtle-blue)
;(color-theme-white-on-grey)

;;;;;;;;;;
;; Utility functions and settings, used later in this file
;;

;; from http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
;; comment out current line
(defun line-comment()
  "Comments out current line."
  (interactive)
  (comment-region (line-beginning-position) (+ 1 (line-end-position))))

;; from http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
;; Uncomment current line
(defun line-uncomment()
  "Uncomments current line."
  (interactive)
  (uncomment-region (line-beginning-position) (+ 1 (line-end-position))))

;; from http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
;; Comment out a paragraph and duplicate it
(defun para-comment-and-duplicate()
  "Comment out a paragraph and duplicate it"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion (forward-paragraph) (point))))
    (copy-region-as-kill beg end)
    (para-comment-forward)
    (yank)))

(defun indenture ()
  (interactive)
  "Forcibly indent the whole buffer to my preferences."
  (indent-region (point-min) (point-max) 't))

; FIXME:  Abstract this into a macro, so it tells you if an arbitrary
; expression is true for any element in a sequence.
(defun matches-any-regexp-p (string list-of-regexps)
  (let ((index  0)
        (len    (length list-of-regexps))
        (retval nil))
    (while (and (< index len)
                (not retval))
           (if (string-match (nth index list-of-regexps)
                             string)
               (setq retval 't)
               (setq index (1+ index))))
    retval))

;;;;;;;;;;;;;;;;;;;
;; Global Editor MACroS (a.k.a. keybindings)

;; Make C-u delete the whole current line (regardless of the initial
;; position of the point), replacing the line with "~/".
;;
;; I use this to clear the minibuffer when I want to find a file in
;; my home directory, not in the current working directory.
(global-set-key "\C-u" (lambda ()
                         (interactive)
                         (if (string-match "^23" emacs-version)
                             (move-beginning-of-line nil) ; same as C-a
                             (beginning-of-line nil))     ; same as C-a
                         (kill-line)                      ; same as C-k
                         (insert "~/")))

;; Turn autoindenting on (code from Perl Best Practices)
(global-set-key "\r" 'newline-and-indent)

;; Use % to match various kinds of brackets (code from Perl Best Practices)
;; (global-set-key "%" 'match-paren)

;; Use C-; to switch buffers; the default C-x b is too cumbersome for this
;; frequently used command.
;;
;; We have to use a vector to represent this key sequence,
;; because ; in a string is interpreted as a LISP comment.
;; See http://www.gnu.org/software/emacs/elisp-manual/html_node/Strings-of-Events.html
(global-set-key [67108923] 'switch-to-buffer)

;; Load up the kill ring with a monotone ID (one of those crazy-long SHA
;; checksums that identify revisions) so that I can easily yank it.
(global-set-key "\C-x\C-z" 'monotone-grab-id)

;; Convenience keybindings
;;
;; Unfortunately, buffer-local bindings shadow global ones,
;; and minor-mode bindings shadow buffer-local ones.
;; So I have to modify some minor modes to make these keys work everywhere.
;; See M-s below for an example...
(global-set-key "\M-c"  'compile)
(global-set-key "\C-f"  'goto-line)
(global-set-key "\M-m"  'man)
(global-set-key "\M-p"  'perlnow-debug)
(global-set-key "\M-r"  'perlnow-run)
(global-set-key "\M-s"  'shell)
(add-hook 'text-mode-hook (lambda () (local-set-key "\M-s" 'shell)))

(add-hook 'shell-mode-hook
          (lambda ()
            (font-lock-mode nil)

            ;; When exiting emacs, kill the shell silently;
            ;; don't ask the user whether or not to kill it.
            (process-kill-without-query (get-process "shell"))))

(global-unset-key "\M-t") ; Prevent accidental damage when I meant to hit M-r
(global-set-key '[f1]   'perlnow-object-module)
(global-set-key '[f2]   'perlnow-edit-test-file)
(global-set-key '[f2]   'perlnow-toggle-between-module-and-test)
(global-set-key '[f3]   'perlnow-module)
(global-set-key '[f4]   'perlnow-script)

;; Use C-o to switch windows; the default C-x o is too cumbersome,
;; and the default binding for C-o just inserts a newline,
;; which you can do with Enter.
(global-set-key "\C-o" 'other-window)
(add-hook 'dired-mode-hook  (lambda () (local-set-key "\C-o" 'other-window)))

;;;;;;;;;;;;;;;;;;;
;; General Configuration

;; Make kill and yank use the X clipboard.  That is:
;;
;; - Killing text "foo" in emacs (e.g by using Ctrl-k) sticks "foo" on the
;;   X clipboard.  Doing "paste" in another application then calls up "foo".
;;
;; - Cutting or copying text "bar" in another application, then doing
;;   a yank in emacs (e.g. by using Ctrl-y) calls up "bar".
;;
;; This is not the default behavior in emacs.  The default is for emacs
;; to maintain its own private clipboard, ignoring the X clipboard,
;; _unless you use the mouse_ to select text.  I find that confusing.
;; This is much better:  just one clipboard to think about.  Thanks
;; to whoever implemented this option! :)
;; 
(setq x-select-enable-clipboard 't)

;; Restore state (files, positions in files) from my last session.

; Work around a bug in desktop.el where it thinks the desktop file is
; locked because this very emacs instance is using it(!)  The workaround
; is always to load without prompting, even when the desktop appears
; to be locked.  (Bug still exists as of CVS revision 1.125 of desktop.el.)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/desktop"))
(load "desktop")
(desktop-save-mode 1)
(desktop-read "~")

;; Accept files sent to us via emacsclient. FIXME: use gnuclient instead.
(server-start 't)
;; Automatically save files from emacsclient when I close them.
(set 'server-temp-file-regexp ".") ; i.e. match all files.

;; When opening a compressed file (e.g. FAQ.gz), uncompress it on the fly
;; and let me see the uncompressed version.  This makes is much easier
;; to read docs from /usr/share/doc, Ubuntu insists on keeping in compressed
;; form.
(auto-compression-mode 1)

;; When opening a GPG-encrypted file, decrypt automatically (prompting
;; me for the passphrase if necessary)
(defvar pgg-gpg-user-id "Nobody")
(autoload 'pgg-make-temp-file "pgg" "PGG")
(autoload 'pgg-gpg-decrypt-region "pgg-gpg" "PGG GnuPG")
(define-generic-mode 'gpg-file-mode
  (list ?#)
  nil nil
  '(".gpg\\'" ".gpg-encrypted\\'")
  (list (lambda ()
	    (add-hook 'before-save-hook
                      (lambda () 
                        (let ((pgg-output-buffer (current-buffer)))
                          (pgg-gpg-encrypt-region (point-min) (point-max)
                                                  (list pgg-gpg-user-id))))
                      nil t)
	    (add-hook 'after-save-hook 
		      (lambda ()
                        (let ((pgg-output-buffer (current-buffer)))
                          (pgg-gpg-decrypt-region (point-min) (point-max)))
			(set-buffer-modified-p nil)
			(auto-save-mode nil))
		      nil t)
            (let ((pgg-output-buffer (current-buffer)))
              (pgg-gpg-decrypt-region (point-min) (point-max)))
	    (auto-save-mode nil)
	    (set-buffer-modified-p nil)))
  "Mode for gpg encrypted files")

;; Use Tramp for transparent access to remote files (and version-control
;; repositories)

; Actually, just use the version that comes pre-installed with Aquamacs.
;(add-to-list 'load-path
;             (expand-file-name "~/.emacs.d/elisp/tramp/install/share/emacs/site-lisp"))


(require 'tramp)

(setq tramp-default-method "sshx")

;; Convenience code for sudo edit-tramp
;; Copied from http://www.emacswiki.org/cgi-bin/wiki/TrampMode
;; written by Erik Bourget, http://www.emacswiki.org/cgi-bin/wiki/ErikBourget
;;
;; The overall effect of this code is that you can C-x C-r filename
;; to edit a file as root.
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
        "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* (;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

;;;;;;;;;;;;;;;;;;;
;; Writing:  human languages
(prefer-coding-system 'utf-8)
(set-input-method     'greek-babel)
(inactivate-input-method) ; C-\ will activate it.

;;;;;;;;;;;;;;;;;;;
;; The rest of this file defines the behavior for coding in various languages.

;;;;;;;;;;;;;;;;;;
;; Version control

;; I don't use Emacs for version control.
;; Keep Emacs out of my way as much as possible.

;; Don't prompt me about whether to follow symlinks to version-controlled code;
;; just do it.
(setq-default vc-follow-symlinks 't)

;; Coding: general
(use-module "coding-general")

;; Coding:  Apache config files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/apache-mode"))
(autoload 'apache-mode "apache-mode" "autoloaded" t)

(setq catalyst-controller-regexp "Controller/.*\\.pm")

(setq apache-config-file-regexps (list
                                  ;; catalyst-controller-regexp
                                  "\\.htaccess$"
                                  "httpd\\.conf$"
                                  "srm\\.conf$"
                                  "access\\.conf$"))

(dolist (regexp apache-config-file-regexps)
  (add-to-list 'auto-mode-alist (cons regexp 'apache-mode)))

(defun apache-config-file-p ()
  (interactive)
  (matches-any-regexp-p buffer-file-name apache-config-file-regexps))

(defun catalyst-controller-p ()
  (interactive)
  (string-match catalyst-controller-regexp buffer-file-name))

(defun restart-apache-if-apache-config-file ()
  "If this file is an Apache config file, then restart Apache.

Add this function to `after-save-hook' to make it happen automatically
when you save such files."
  (if (apache-config-file-p)
      ;FIXME:  Report success/failure somehow?
      (call-process "/usr/bin/sudo" nil nil nil "/etc/init.d/apache" "restart")))

(add-hook 'after-save-hook 'restart-apache-if-apache-config-file)

(defun restart-catalyst-server-if-controller ()
  "If this file is a catalyst controller, then restart Catalyst.

Add this function to `after-save-hook' to make it happen automatically
when you save such files."
  (if (catalyst-controller-p)
      ;FIXME:  Report success/failure somehow?
      (call-process "/home/quinn/bin/restart_catalyst_server" nil 0 nil)))

;(add-hook 'after-save-hook 'restart-catalyst-server-if-controller)

;;;;;;;;;;;;;;;;;;
;; Coding:  XHTML

; Use nxml-mode for all XHTML files and templates.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/nxml-mode"))
(require 'nxml-mode)
(defalias 'html-mode 'nxml-mode)
(defalias 'html-helper-mode 'nxml-mode)

; Also use nxml-mode for Template Toolkit templates.
(add-to-list 'auto-mode-alist
             '("\\.tt2$" . nxml-mode))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/ecmascript-mode"))
(require 'ecmascript-mode)

; Use four-space indentation, as Nature and Nature's God intended:
(setq nxml-child-indent 4)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding:  Asterisk config files

(defun asterisk-config-file-p ()
  (string-match "/extensions.conf$" buffer-file-name))

(defun hup-asterisk-if-asterisk-config-file ()
  "If this file is an Asterisk config file, then tell Asterisk to re-read
its config files (e.g. extensions.conf).

Add this function to `after-save-hook' to make it happen automatically
when you save such files."
  (if (asterisk-config-file-p)
      ;FIXME:  Report success/failure to a buffer?
      (call-process "/bin/sh" nil nil nil
                   "-c" "/usr/bin/sudo kill -HUP `cat /var/run/asterisk.pid`")))

(add-hook 'after-save-hook 'hup-asterisk-if-asterisk-config-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding:  C-like languages

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "ellemtel")

            ;; Get rid of the behavior of c-electric-colon,
            ;; which left-justifies the line whenever you type a colon :/
            (global-set-key ":" 'self-insert-command)))

;;;;;;;;;;;;;
;; Coding:  C

;; Help me find the definitions of C functions when I run across them in code
;; (commented out because it litters semantic.cache files all over the place).
;(require 'cedet)

;; Here is one possible fix for the semantic.cache problem.  It tells CEDET
;; to place all semantic.cache files in one directory, rather than placing
;; them next to files you are editing.  Thanks to
;; http://blog.ox.cx/archives/113-Getting-rid-of-semantic.caches.html
;; for the tip.
(setq semanticdb-default-save-directory
      (expand-file-name "~/tmp/semantic.cache"))

(setq c-indent-level 4)

;;;;;;;;;;;;;;;;;
;; Coding:  elisp

; Is this an Emacs LISP file?
(defun elisp-file-p ()
  (string-match "\\.el$" buffer-file-name))

(defun byte-compile-if-elisp-file ()
  "If this file is Emacs LISP source code, then compile it.  Also \"load\" it
in the interactive interpreter, so it starts affecting Emacs's behavior
right now.

Add this function to `after-save-hook' to make it happen automatically when
you save elisp code.  This can be a life-saver; there's nothing more annoying
than fixing a bug using the interactive interpreter, then seeing a regression
the next time you run emacs because you forgot to compile your code."
  (if (elisp-file-p)
      (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-if-elisp-file)

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;;;;;;;;;;;;;;;;
;; Coding:  Perl

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/perlcritic"))
(autoload 'perlcritic          "perlcritic" "" t)
(autoload 'perlcritic-region   "perlcritic" "" t)
(autoload 'perlcritic-mode     "perlcritic" "" t)
(add-hook 'cperl-mode-hook      (lambda ()
                                 (local-set-key "\C-q"
                                                'perlcritic)

                                 (local-set-key "\M-q"
                                                (lambda ()
                                                  (interactive)
                                                  (query-replace "\"" "'")))))

;(set      'perlcritic-profile  "/usr/local/share/codenazi/cfg/perlcritic.cfg")
(set      'perlcritic-severity 1)

;; Recognize .t files as Perl code; open them in perl-mode
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode)
                            auto-mode-alist))

;; The following code was lifted (selectively) from Appendix C of
;;_Perl Best Practices_.

;; Use % to match various kinds of brackets...
;; See: http://www.lifl.fr/~hodique/uploads/Perso/patches.el
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

;; end of code from _Perl Best Practices_
;

;;;;;;;;;;;;;;;;
;; Coding:  Perl
(use-module "perl")

;;;;;;;;;;;;;;;;;;
;; Coding:  Python
(use-module "python")

;;;;;;;;;;;;;;;;;;
;; Coding: Ruby

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/pabbrev"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/mmm-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/ruby-elisp"))
(require 'ruby-mode)
(require 'pabbrev)
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

;; Ruby debugger
;; Not installed by default. Upgrade to Emacs 22 to get it, or use emacsmirror?
;; (require 'rdebug)

(setq ruby-indent-level 2)

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

(add-hook
 'ruby-mode-hook
 (lambda ()
   (local-set-key [return] 'ruby-reindent-then-newline-and-indent))
 't
)

; (defun ruby-eval-buffer ()
;   (interactive)
;   "Evaluate the buffer with ruby"
;   (shell-command-on-region (point-min) (point-max) "ruby"))
  
;;;;;;;;;;;;;;;;;;
;; Coding:  SQL

; The variables sql-user and sql-password do not play properly
; with postgres.  Instead, you have to specify them in
; sql-postgres-options (you may use ~/.pgpass for the
; password, which I have elected to do here).
(setq sql-user "build")
(setq sql-database "")
(setq sql-host "")
(setq sql-postgres-options '("-U" "build" "pager=off"))

;;;;; Some databases
;; The local sqlite database for Tracks (my GTD system)
;(setq sql-sqlite-program "sqlite3")
;(setq sql-database (expand-file-name "~/tracks/db/tracks-104.db"))

;; (add-hook 'sql-mode-hook
;;           (lambda ()
;;             ;; sql-indent doesn't do what I want it to do (its regexes are too limited).
;;             ;; Must I hack it, or can I find something better out there?
;;             (add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/sql-indent"))
;;             (require 'sql-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding:  XML (and SGML)

;; Disable sgml-mode and James Clark's nxml-mode; instead, use psgml-mode,
;; which supports code-folding. :)
;(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

;;;;;;;;;;;;;;;;
;; Coding:  YAML

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/yaml-mode/trunk"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Indent four spaces
(setq yaml-indent-offset 4)

;; When I hit enter, auto-indent the next line.
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar ((t (:background "darkslategray")))))

;; (maximize-vertically)

;; for compatibility with older Aquamacs versions
(defvar aquamacs-140-custom-file-upgraded t)
(unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))
