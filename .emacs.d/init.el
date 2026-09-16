;; -*- lexical-binding: t; no-native-compile: t; -*-

;;;; Debugging this init file

(defun bisect-load-init (fraction)
  "Load the first FRACTION (0.0–1.0) of init.el (for debugging purposes)"
  (interactive "nFraction to load (0.0–1.0): ")
  (let* ((init-file (expand-file-name "~/.emacs.d/init.el"))
         (tmpfile (make-temp-file "emacs-bisect" nil ".el")))
    (with-temp-file tmpfile
      (insert-file-contents init-file)
      (let* ((total-lines (count-lines (point-min) (point-max)))
             (target-line (round (* total-lines fraction))))
        (goto-char (point-min))
        (forward-line target-line)

        ;; Advance to the end of the current top-level form (if any)
        ;; so we don't truncate mid-expression.
        (condition-case nil
          (beginning-of-defun 1)
          (forward-sexp 1)
          (error nil))
        (delete-region (point) (point-max))
        (message "Loading first ~%d/%d lines of init.el (temp file %s)..."
                 target-line
                 total-lines
                 tmpfile)
        (load-file tmpfile)))))

(let* ((val (getenv "BISECT_EMACS")))
  (when (and val (not (equal "" val)))
    (bisect-load-init (string-to-number val))))

;;;; GUI/TUI settings (keywords: 'tty')  -*- lexical-binding: t; -*-

(defun for-every-session-qdw (func)
  "Some built-in Emacs functions must be called once per frame,
after the frame is created. But they must also be called once
when Emacs first runs.

`for-every-session' does that for you. It takes a single argument,
FUNCTION, then 1) runs that function and 2) adds that function
to `after-make-frame-functions'.

This applies equally to GUI Emacs (where frames are OS windows)
and TUI emacsclient (where frames are emacsclient sessions;
this includes the scenario where you run Emacs in --bg-daemon
or --fg-daemon mode and then connect to the daemon using emacsclient)."
  (funcall func)
  (add-hook 'server-after-make-frame-hook func))

;; TTY settings. If you're running GUI Emacs, these are no-ops.

(setopt multiple-terminals-merge-keyboards t) ;; fewer lock-ups

;; They work by telling Emacs to interpret certain magic
;; escape sequences iff your terminal emulator sends them.
;; This code has two effects:
;; 
;; 1) Enable special mouse buttons if your terminal emulator supports them.
;;
;; 2) Automagically, transiently turn off electric-indent-mode and
;;    enable bracketed paste whenever you paste from the OS
;;    clipboard (e.g., when you do Cmd-V on a Mac).
;;
;;    This behavior is the equivalent of vim's :set paste,
;;    except that you don't need to turn it on and off manually.

(for-every-session-qdw
  (lambda ()
    (setenv "TERM" "xterm-256color") ; required for bracketed paste
    (xterm-mouse-mode +1)))

;;;; Basic behavior of Emacs itself
;; (use-package emacs
;;     :custom 
;;     (search-default-mode 'char-fold-to-regexp ;; already done elsewhere
;; ))

;;;; Inhibit superfluous messsages.

(setopt inhibit-startup-echo-area-message "quinn"
        ad-redefinition-action 'accept
        warning-suppress-types '((files))) ;; lexical-binding warnings

(remove-hook 'find-file-hook 'vc-refresh-state) ;; we'll use Magit instead.
(remove-hook 'find-file-hook 'epa-file-find-file-hook)
(remove-hook 'find-file-hook 'url-handlers-set-buffer-mode)

;;;;;; (let ((inhibit-messsage t))

;; Remove chrome (menu bar/tool bar/scroll bar).
;; (Behold the inconsistency of Emacs conventions!)

(setopt inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(use-package scroll-bar :config (scroll-bar-mode -1)
                        :demand t
                        :ensure nil)

;; Don't beep; flash the screen (visual bell) instead.

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg)
                                 (set-face-foreground 'mode-line fg))
                               orig-fg))))
                               
;; Disable bold text (just use regular weight). This applies in both
;; GUI Emacs and TTY Emacs.

(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal))
 (face-list))

;;;; GUI settings (these are no-ops when running in a terminal emulator)

(set-face-font 'default "Menlo-17")
(blink-cursor-mode -1)

;;;; Emacs Lisp native-compilation

;; Compile ahead-of-time, or don't compile at all.
;; This reduces judder from Emacs randmoly reciding to native-compile .elc
;; files during editing.

(setopt native-comp-jit-compilation nil
        load-prefer-newer 't)

;;;; Basic text-editing behavior

;; Turn off right-to-left language support,
;; improving performance slightly.

(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Copypasta settings: overwrite or delete the "region" (selected text)
;; when I type, in keeping with modern UI conventions.

(delete-selection-mode +1)

;; Copypasta settings: before overwriting the syustem clipboard,
;; save its contents in the Emacs kill-ring so I can get it back.

(setq save-interprogram-paste-before-kill t)

;; Display long lines as ......$,
;; rather than displaying the whole text-line on multiple terminal-lines
;; using Emacs ......\ notation

(defalias #'vim-newlines #'toggle-truncate-lines)

(defun start-lines-at-col (one-based-column-number)
  (interactive "n")
  (toggle-truncate-lines +1)
  (set-window-hscroll (selected-window) (- one-based-column-number 1))
  (setq-local auto-hscroll-mode nil))

(defun start-lines-at-1 ()
  (interactive)
  (set-window-hscroll (selected-window) 0)
  (setq-local auto-hscroll-mode 't))

(defun move-line-up-qdw ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down-qdw ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;;;; Color-theme settings

;; ;; Performance tweak: don't color my text as I'm typing; wait for me to stop
;; (setq redisplay-skip-fontification-on-input t)

(defvar self-authored-themes-dir
        ;; (expand-file-name "qdw/qdw-themes" user-emacs-directory))
  (expand-file-name "qdw-themes" user-emacs-directory))

(defvar themes-qdw
        '("amber" "amber-without-bold" "mariana" "tritium"))

(add-to-list 'custom-theme-load-path self-authored-themes-dir)

;; A tweaked version of someone else's Emacs port of Sublime Text mariana

(add-to-list 'custom-theme-load-path "~/src/forks/mariana-theme")

(setopt custom-enabled-themes '())
(defvar my-chosen-theme "tritium")

;; Load my current favorite theme.
;; But keep the cursor bright orange, no matter the theme, for visibility.

(load-theme (intern my-chosen-theme) 't)
(set-cursor-color "DarkOrange1")

;;;; Window manipulation settings.

;; Note that, in Emacs, the term "window" means what is called in modern
;; parlance a pane or viewport. This is because Emacs was written before
;; your primitive human GUIs existed.

(setopt
 window-combination-resize t
 recenter-positions '(top 0.33 bottom)
 display-fill-column-indicator-column 80 ;; noop if fill-column is nil=default
)

;;; Moving the point (cursor) among and within windows

(setopt set-mark-command-repeat-pop t)

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

;; (use-package popper
;;   :bind (("C-c C-q" . popper-toggle)
;;          ("C-c TAB" . popper-cycle)
;;          ("C-c b"   . popper-toggle-type))

;;   :init (setq popper-reference-buffers '("\\*Messages\\*"
;;                                          "Output\\*$"
;;                                          "\\*Async Shell Command\\*"
;;                                          help-mode
;;                                          compilation-mode))
;;         (popper-mode +1)
;;         (popper-echo-mode +1))

;;; Rearranging windows

(defun rotate-windows ()
  "Rotate your windows (from whattheemacsd.com)"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        ('t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i))))))
             
  (other-window -1))

(winner-mode +1)
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; Explicitly load everything at Emacs init time,
;; since I'm running in --fg-daemon mode and I don't
;; want delays later when things lazy-load.
;; 
;; For documentation on use-package, see
;;
;; https://github.com/jwiegley/use-package-examples

(setopt use-package-always-demand t) ;; load at Emacs init time
;;      use-package-always-ensure t) ;; auto-install from repo if not installed
                                     ;; Uncomment this when running Emacs
                                     ;; for the first time on a new machine.

(defun load-qdw-module (el-file-name)
  (load (expand-file-name el-file-name "~/.emacs.d/qdw-modules")))

(defun load-file-if-exists-qdw (file-name)
  "Load FILE-NAME if it exists, and do nothing otherwise."
  (let ((inhibit-message t))
    (condition-case nil
        (load (expand-file-name file-name user-emacs-directory) 't 't)
      (error nil))))

(use-package ansi-color)
(load-qdw-module "c-qdw.el")
(use-package comint)
(use-package compile)
(load-qdw-module "cperl-qdw.el")
(load-qdw-module "org-qdw.el")
(use-package dired)
(use-package dirtrack)
(use-package imenu)
(load-qdw-module "make-mode-qdw.el")
(load-qdw-module "pgsql-hackers-qdw.el") ;; :after (cperl-qdw make-mode-qdw)
(use-package ruby-mode)
(use-package sh-script
    :init (advice-add 'sh-set-shell
                      :around
                      (lambda (orig &rest args)
                        (let ((inhibit-message 't))
                          (apply orig args)))))
(load-qdw-module "sql-qdw.el")
(use-package json) (setq-default json-encoding-default-indentation "    ")
(use-package markdown-mode)
(use-package url-util)

;;;; LSP

;; Increase the size of the buffer that Emacs
;; uses to communicate with the LSP server,
;; speeding up reads.

(setq read-process-output-max (* 4 1024 1024))

;;;; Hack regexes interactively (M-x re-builder)

;; Don't require double backslashes when escaping - only single.
(setq reb-re-syntax 'string)

(use-package whitespace)

;;;; Basic comfort

(put 'upcase-region     'disabled nil)
(put 'downcase-region   'disabled nil)
(put 'narrow-to-region  'disabled nil)

(setq-default
 ;; If I write non-ASCII chars in a buffer, then save the file as Unicode
 prefer-coding-system 'utf-8

 ;; Don't auto-hard-wrap lines
 auto-fill-mode nil

 elisp-indent-level 2)

(setopt
 word-wrap nil
 
 ;; Open symlinks' targets, with the targets' parents directory as buffer cwd
 find-file-visit-truename t
 
 ;; Consider filenames case-sensitive (needed on macOS)
 read-file-name-completion-ignore-case nil 
 
 ;; Warn only when opening really large files
 large-file-warning-threshold 100000000

 ;; Some Unix command-line utilities work properly only when files
 ;; end in a newline, for example, 'wc -l'
 require-final-newline 't

 ;; Single-space: always.
 colon-double-space nil 
 
 ;; Single-space. Always.
 sentence-end-double-space nil

 ;; The first day of the week is Monday.
 calendar-week-start-day 1
 
 ;; If I have a region selected, then make commands like query-replace
 ;; operate on that region only
 transient-mark-mode 't

 ;; Don't leave *~ droppings.
 make-backup-files nil
 
 cursor-type 'box
 column-number-mode nil
 
 ;; 't means always highlight as much distinct syntax as is supported
 global-font-lock-mode 't

 ;; Instead of beeping, flash screen
 visible-bell 't
 )

;;;; Whitespace-handling

;; Indentation: use spaces, not tabs.
(setopt
 indent-tabs-mode nil
 tab-width 4
 ;; Make tabs-as-indentation visible, so I can eliminate them
 ;; (takes effect iff whitespace-mode is true)
 whitespace-style '(face
                    ;; empty
                    indentation
                    ;; trailing
                    ;; lines-tail
                    missing-newline-at-eof)

 whitespace-display-mappings '((tab-mark 9 [187 9] [92 9])))

;; Turn off the display of whitespace for now; I haven't yet found
;; a way of displaying things that I actually like.

(global-whitespace-mode nil)

;;;; Hack: general: automatic indentation

(electric-indent-mode +1)

;;;; Hack: general: tree-sitter
;;
;; nothing here yet

;;;; Hack Emacs Lisp

;; In Emacs Lisp, I often use idiosyncratic "horizontal"
;; coding for readability, and I don't want electric-indent to undo it.

(use-package lisp-mode
  :hook (emacs-lisp-mode . (lambda () (electric-indent-local-mode -1))))

;;;; Miscellaneous helper/convenience functions

(defalias #'linum-mode #'display-line-numbers-mode)

(defun lfi ()
  (interactive)
  (load-file-if-exists-qdw "site.el"))
      
(defmacro setqu (&rest args)
  "Set multiple variables globally, overriding them if they are
already set. This function's syntax is the same as that of setq,
but it calls defvar under the hood."
  (let (forms)
    (while args
      (let ((var (pop args))
            (val (pop args)))
        (push `(defvar ,var ,val) forms)))
    `(progn ,@(nreverse forms))))
    
(defun string-in (str ls)
  "Return \=t if any member of a ls matches string; else return nil"
  (seq-some (lambda (s) (string= str s)) ls))
  
;; Make the contents of *scratch* persist across Emacs sessions

(setq scratch-buffer-file (expand-file-name "~/tmp/scratch.el"))

(defun save-scratch-buffer-to-file ()
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max) scratch-buffer-file)))

(defun load-scratch-buffer-from-file ()
  (if (file-exists-p scratch-buffer-file)
      (with-current-buffer "*scratch*"
        (erase-buffer)
        (insert-file-contents scratch-buffer-file))))

(add-hook 'kill-emacs-hook #'save-scratch-buffer-to-file)

(add-hook 'emacs-startup-hook #'load-scratch-buffer-from-file)

(setopt lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook (lambda ()
                            (eldoc-mode +1)))

;; When I run C-(
;; (which lets me write and eval a quick Lisp expression
;; in the minibuffer),
;; add the opening parenthesis for me.
(add-hook 'eval-expression-minibuffer-setup-hook
          (lambda ()
            (insert "(")))

;;;; Settings for Emacs Lisp I wrote

(add-to-list 'load-path self-authored-themes-dir)

(add-to-list 'load-path (expand-file-name "~/src/forks/mariana-theme"))

(load-file-if-exists-qdw "secrets.el")
(load-file-if-exists-qdw "site.el")

(defvar jira-base-url (getenv "JIRA_BASE_URL"))
(defvar jira-default-project-qdw (getenv "JIRA_DEFAULT_PROJECT"))
(defvar slack-team (getenv "SLACK_TEAM"))

;;;; Javascript settings, TypeScript settings, JSON settings

(let ((desired-indent-level 4))
  (setopt
        typescript-indent-level desired-indent-level)
  (setq js-indent-level desired-indent-level
        js-switch-indent-offset desired-indent-level))

;; eslint (JavaScript/TypeScript linter) settings

(defun seteslintrc ()
  "Returns the location of nearest eslintrc"
  (interactive)
    (setq flycheck-eslintrc
          (locate-dominating-file "~/" ".eslintrc.json")))

(defun configure-typescript ()
  (interactive)
  (tide-setup)
  (setq fill-column 120)
  (company-mode +1)
  (eldoc-mode +1)
  (flycheck-mode +1)
  (column-number-mode +1)
  (seteslintrc)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-auto-indent-flag nil
        company-tooltip-align-annotations t)
  (tide-hl-identifier-mode +1))

(add-hook 'typescript-mode-hook #'configure-typescript)
  
;;;; .ignore settings
;;;; This file is formatted exactly like .gitignore,
;;;; and is honored by fd, rg, and maybe others.
;; (add-to-list 'auto-mode-alist
;;              '("\\.ignore\\'" . gitignore-mode))


;;;; Markdown settings

;; Treat all Markdown as GitHub-Flavored Markdown ("gfm").
;; Its main advantage over the original Markdown standard
;; is that medial underscores no longer toggle italics.

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))

;;;; Buffers; files; saving; killing emacs

(defun buffer-saver-killer-helper (prompt-bool)
  (save-some-buffers prompt-bool)
  (mapc 'kill-buffer (cl-remove-if-not 'buffer-file-name (buffer-list))))
    
(defun kill-all-buffers-qdw ()
  "Kill all file buffers, prompting to save each unsaved buffer"
  (interactive)
  (buffer-saver-killer-helper nil))

(defun kill-all-file-buffers-without-prompting-qdw ()
  "Kill all file buffers, first saving any that need it (without prompting)"
  (interactive)
  (buffer-saver-killer-helper 't))

(defun kill-emacs-qdw ()
  "Save file buffers and kill emacs, all without prompting"
  (interactive)
  (buffer-saver-killer-helper 't)
  (kill-emacs))

(defun start-at-column-qdw (col-num-zero-based)
  (interactive "n") ;; Number read using minibuffer.
  (set-window-hscroll (selected-window) col-num-zero-based))

;;;; Edit this file

(defun ini ()
  (interactive)
  (find-file user-init-file)

  (let ((line 350)
         (column 1))

     (widen)
     (goto-char (point-min))
     (forward-line (- line 1))

     (move-to-column (- column 1))))

;;;; Edit my ~/.bashrc

(defun edit-bashrc ()
  (interactive)
  (find-file (expand-file-name "~/.bashrc")))

;;;; Color-theme-related utility functions

(defun if-file-is-theme-reload-theme ()
  (let ((base-name (file-name-nondirectory (file-truename buffer-file-name))))
    (when (string-match "\\(.*\\)-theme\\.el$" base-name)
      (let ((theme-name (match-string 1 base-name)))
        (when (string-in theme-name themes-qdw)
          (load-theme (intern theme-name) 't 't))))))

(add-hook 'after-save-hook #'if-file-is-theme-reload-theme)

(defun theme-insert-color (color-name)
  (when (boundp (intern color-name))
        (let* ((hex-string (symbol-value (intern color-name))))
          (insert (propertize hex-string
                              'face `(:foreground ,hex-string)))
          (insert (concat "  " color-name "\n")))))

(defun test-pattern ()
  (interactive)
  (let* 
      ((buffer (get-buffer-create "*theme colors*"))
       (basic-colors '("black"
                       "red"
                       "green"
                       "yellow"
                       "blue"
                       "magenta"
                       "cyan"
                       "white"))
       (variants '("ansi-color"
                   "bright"
                   "ansi-color-bright"
                   "extended"
                   "extended-bright")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (col basic-colors)
        (theme-insert-color col)
        (dolist (variant variants)
          (theme-insert-color (concat variant "-" col))))
      (display-buffer buffer))))


(defun theme-generate-tinted-theme-yaml ()
  (interactive)
  (let ((buffer (get-buffer-create "theme.yaml")))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (base16name-themename '(("base00" . "black")
                                        ("base01" . "red")
                                        ("base02" . "green")
                                        ("base03" . "yellow")
                                        ("base04" . "blue")
                                        ("base05" . "magenta")
                                        ("base06" . "cyan")
                                        ("base07" . "white")
                                        ("base08" . "bright-black")
                                        ("base09" . "bright-red")
                                        ("base0A" . "bright-green")
                                        ("base0B" . "bright-yellow")
                                        ("base0C" . "bright-blue")
                                        ("base0D" . "bright-magenta")
                                        ("base0E" . "bright-cyan")
                                        ("base0F" . "bright-white")))
        
        (let* ((tinted-name (car base16name-themename))
                 (theme-name (cdr base16name-themename))
                 (hex (symbol-value (intern theme-name))))
          (insert (concat tinted-name ": \"" hex "\"\n"))))
      (display-buffer buffer))))

(defalias 'colors 'list-colors-display)

(defun amber-reload ()
  (interactive)
  (load-theme 'amber 't 't))

(defun tritium-reload ()
  (interactive)
  (load-theme 'tritium 't 't))

(defun tritium-re-theme ()
  "Load a file that defines the 16 4-bit ANSI colors;
  then load tritium, my custom 16-color theme"
  (interactive)
  (load
   (read-file-name "Color definition file: "
                   (expand-file-name self-authored-themes-dir)))
  (tritium-reload))

;; Interpret any ANSI escape codes in this buffer,
;; coloring the buffer text accordingly.
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; tritium, a versatile, dim dark theme by qdw
;; - Looks good under a variety of indoor lighting conditions
;; - Looks good in the dark
;; - Stays readable if red-shifted Flux.app or Night Shift
;; - Auto-detects how many colors your terminal supports
;;   - And adapts colors accordingly
;;     - Supported color spaces:
;;       - 24-bit true color  (millions of colors)
;;       -  8-bit xterm color (256 colors)
;;       -  4-bit ANSI color   (16 colors)

;;; Third-party color themes settings
;;
;;; Loud themes, for prod boxes
;; T  birds-of-paradise-plus: very loud red theme,
;;    for use on production boxes
;; (load-theme 'birds-of-paradise-plus 't)

(defun switch-theme ()
  (interactive)
  (customize-variable 'custom-enabled-themes))

(defun light ()
  (interactive)
  (setopt custom-enabled-themes (list 'mariana)))

(defun dark ()
  (interactive)
  (setopt custom-enabled-themes (list 'tritium)))

(defun toggle-theme-qdw ()
  "Toggle between my two favorite color themes, if applicable"
  (interactive)
  (let* ((theme (car custom-enabled-themes))
         (other-theme (cond ((eq theme 'tritium) 'mariana)
                            ((eq theme 'mariana) 'tritium)
                            ('t nil))))
    (when other-theme
      (setopt custom-enabled-themes (list other-theme)))))

;;;; Other themes that I like okay:

;;; Light mode:

;; (load-theme 'modus-operandi) ;; max-contrast light theme (good in sunlight)

;;; Dark mode:

;; (load-theme 'modus-vivendi)  ;; max-contrast dark theme

;;;; dired

(defun dired-find-file-qdw ()
  "In Dired, visit the file or directory on the cursor.
If the file is a directory, use `dired-find-alternate-file'
to avoid creating many buffers. If the file is a regular file,
use `dired-find-file' to open it in a new buffer."
  (interactive)
  (let ((is-dir (file-directory-p (dired-get-file-for-visit))))
    (if is-dir
      (dired-find-alternate-file)
      (dired-find-file))))

;;;; Buffer list

(setopt ibuffer-formats
        '((mark modified read-only " "
                (name 40 40 :left :elide) ;; "Name" column width
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

;;;; Alternative editors

;; ;; If a file is open in Emacs and another editor saves the file,
;; ;; update the one in Emacs. Also do this for dired.

;; (setopt auto-revert-verbose nil)
;; (when (not global-auto-revert-mode)
;;       (global-auto-revert-mode 't))

(defun alt-editor (program &rest program-args)
  (interactive)
  (letrec ((progname (file-name-nondirectory program))
           (procname (concat "emacs-" progname)))
    ;; (message (mapconcat 'identity
    (apply 'start-process procname nil program program-args)))

(defun gvim ()
  (interactive)
  (alt-editor
    "/opt/homebrew/opt/macvim/bin/mvim"
    (format "+call cursor(%d, %d)"
            (line-number-at-pos)
            (+ 1 (current-column)))
    (buffer-file-name)))
    
(defalias 'mvim 'gvim)

(defun file-line-column-colon-separated ()
  (interactive)
  (format "%s:%d:%d"
          (buffer-file-name)
          (line-number-at-pos)
          (+ 1 (current-column))))

(defun curse ()
  (interactive)
  (alt-editor
   "/usr/local/bin/cursor"
   "--goto" (file-line-column-colon-separated)))

(defun subl ()
  (interactive)
    (alt-editor
     "subl"
     (file-line-column-colon-separated)))

(defun vscode ()
  (interactive)
  (alt-editor
   "/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code"
   "--goto" (file-line-column-colon-separated)))

;; (defun open-colon-separated (editor-file-name &optional args)
;;   (interactive)
;;   (when buffer-file-name
;;     (let ((command
;;            (cons editor-file-name args)))
;;       (alt-editor command
;;        editor-file-name ))))

;;;; Quality-of-life

(defun goto-column (num)
  "Move to column N (1-based) of the current line"
  (interactive "nGoto column: ")
  (let ((one-based-column-number
         (cond ((not num)   0)
               ((eq num 0)  0)
               ('t          (1- num)))))
    (move-to-column one-based-column-number)))


(defun open-thing-at-point ()
  "Open the file or URL at point, with line and column if applicable."
  (interactive)
  (cond
    ((thing-at-point 'url)
     (call-process "/usr/bin/open" nil 0 nil (thing-at-point 'url 't)))

    ((thing-at-point 'filename)
     (let* ((parts (split-string (thing-at-point 'filename 't) ":"))
            (file-name (or (nth 0 parts) parts))
            (lineno (if (and (nth 1 parts) (string-match-p "^[0-9]+$" (nth 1 parts)))
                        (string-to-number (nth 1 parts))
                        1))
            (col (if (and (nth 2 parts) (string-match-p "^[0-9]+$" (nth 2 parts)))
                     (string-to-number (nth 2 parts))
                     1)))
       (find-file file-name)
       (goto-line lineno)
       (goto-column col)))

    ('t
     (message "Don't know how to handle thing at point"))))


;;;; Settings specific to GUI Emacs
;;;; dired

;; ;; When I delete files in dired, send them to the macOS Trash; don't rm.
;; 
;; (osx-trash-setup)
;; (setopt delete-by-moving-to-trash 't)
;; (defun system-move-file-to-trash (path)
;;   "Moves file at PATH to the macOS Trash
;;   according to `move-file-to-trash' convention.
;; 
;;   Relies on the command-line utility 'trash' to be installed.
;;   Get it from:  <http://hasseg.org/trash/>"
;;   (shell-command (concat "trash -vF \"" path "\""
;;                          "| sed -e 's/^/Trashed: /'")
;;                  nil ;; Name of output buffer
;;                  "*Trash Error Buffer*"))

;; ;; macOS's bundled BSD ls doesn't play well with Emacs,
;; ;; so use Homebrewed GNU ls instead.

;; (dolist (gnu-ls '("/opt/homebrew/bin/gls" "/usr/local/bin/gls"))
;;   (when (file-exists-p gnu-ls)
;;     (setopt insert-directory-program
;;             gnu-ls
            
;;             dired-use-ls-dired
;;             t
            
;;             dired-listing-switches
;;             "-aBhl --group-directories-first")))

;;;; Human languages

;; Adapted from xahlee.org

(defun unfill-paragraph-xah ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region-xah (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r"
  (let ((fill-column 90002000))
    (fill-region start end))))

(setopt
 column-number-mode 't
 custom-safe-themes 't
 eldoc-idle-delay 0
 elpher-default-url-type "gemini"
 isearch-allow-scroll 'unlimited
 search-default-mode 'char-fold-to-regexp
 ls-lisp-use-insert-directory-program nil
 mode-line-position-column-format '(" %C")
 mode-line-position-column-line-format '(" (%l,%C)")
 neo-cwd-line-style 'text
 neo-theme 'classic
 ns-alternate-modifier 'none
 ns-right-alternate-modifier 'meta)

;;;; Reading ePub ebooks

;; (add-to-list
;;  'auto-mode-alist
;;  '("\\.epub\\'" .
;;    (lambda ()
;;      (add-to-list 'load-path (expand-file-name "~/src-3/nov.el") nil)
;;      (load-file (expand-file-name "~/src-3/nov.el/nov.el"))
;;      (nov-mode))))

;;;; elfeed: enjoy podcasts and other RSS and Atom feeds

;; I define individual subscriptions in ./site.el for now
;; (machine-specific; deliberately .gitignore'd).

(use-package elfeed
  :config
  (setopt elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))

  (defun play-podcast-episode-at-point-qdw ()
    (interactive)
    (let* ((episode (if (eq major-mode 'elfeed-show-mode)
                        elfeed-show-entry
                        (elfeed-search-selected :single)))

           (episode-url (elt (elfeed-entry-enclosures episode) 0)))

      (run-applescript-qdw
       "-e" "tell application \"QuickTime Player\""
            "-e" (format "open location \"%s\"" episode-url)
            "-e" "activate"
            "-e" "play front document"
       "-e" "end tell"))))

  ;; ;; elfeed uses the built-in Emacs package shr.el to open links.
  ;; ;; Override it iff in elfeed.
  ;; ;;
  ;; ;; (Hm, or maybe I should just remap RET in elfeed-mode?)
  ;; (advice-add 'shr-browse-url
  ;;             ...)

  ;; :bind
  ;; (("C-c C-c ." play-podcast-episode-at-point-qdw)))


  

;;;; Hack Bash/Bourne shell

;; Automatically make files executable if they start with a shebang

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; (use-package sh-script
;;   :config
;;   (advice-add 'sh-set-shell
;;               :around
;;               (lambda (orig &rest args)
;;                 (let ((inhibit-message 't))
;;                   (apply orig args)))))

;; Don't auto-insert '<< EOF'-style heredocs when I type '<<',
;; since I have my own preferred way of writing heredocs.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-electric-here-document-mode nil)))

;;;; Settings for running shell commands from Emacs

(defun shell-command-qdw (&optional prefix)
  "Prompt for a shell command. If there is a region selected, then insert it as the last argument in the prompt, with the point positioned immediately before that last argument. If PREFIX is non-nil, then insert the command output into in the current buffer at point."
  (interactive "P")
   (let* ((prompt
           "Shell command: ")

          (final-arg
           (if (not (use-region-p))
               ""
               (concat " " (shell-quote-argument
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))))))
         
          (command-string
           (minibuffer-with-setup-hook
               (lambda ()
                 (goto-char (+ 1 (length prompt))))
               (read-shell-command prompt final-arg))))
     
     (shell-command
      (concat "source ~/.bashrc && INSIDE_EMACS=1 " command-string))))

(defun shell-command-to-string-qdw (command)
  (interactive "M")
  (string-trim-right
    (shell-command-to-string
      (concat "source ~/.bashrc && INSIDE_EMACS=1 " command))))

(defun noninteractive-shell-command-qdw1 (command)
  (shell-command
    (concat "source ~/.bashrc && INSIDE_EMACS=1 " command)
    "*qdw-stdout*"
    "*qdw-stderr*"))

;;;; Settings for running interactive shells inside Emacs (via `M-x shell`)

;; Automatically update Emacs's variable that tracks the current working dir,
;; iff the shell tells it to use by emitting OSC 7.
(add-hook 'comint-output-filter-functions #'comint-osc-process-output)
;; (setq-default shell-prompt-regexp "[~][/].*[$][ ]")

(defun shell-dirstack-update (output)
  (when (string-match-p comint-prompt-regexp output)
    (comint-send-string (get-buffer-process (current-buffer)) "dirs\n")))

(add-hook 'comint-output-filter-functions 'shell-dirstack-update nil t)

  
;; Don't prompt me about shell sessions, debug sessions, et cetera
;; when I quit Emacs. Just kill them.

(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer)) nil)))

(defun iterm2-here ()
  (interactive)
  (letrec
      ((procname
        "emacs-invoked-iterm")
        
       (dir
        (shell-quote-argument (file-name-directory buffer-file-name)))
       
       (applescript-args
        `("tell application \"iTerm2\""
          "-e" "create window with default profile"
          "-e" "tell the current session of the current window"
          "-e" ,(format "write text \"cd %s\"" dir)
          "-e" "end tell"
          "-e" "end tell")))

    (apply 'start-process
           procname nil "/usr/bin/osascript" "-e" applescript-args)))

;;;; More macOS-specific stuff

(defun run-applescript-qdw (&rest osascript-args)
  (let ((process-name "emacs-dot-ini-function-run-applescript-qdw")
        (stdouterr-buffer-name "*applescript*")) ;; maybe change to nil later
    (apply #'start-process process-name
                           stdouterr-buffer-name
                           "/usr/bin/osascript"
                           osascript-args)))

(defun open-url-in-quicktime-player-app-qdw (url)
  (interactive "MURL: ")
  (run-applescript-qdw
       "-e" "tell application \"QuickTime Player\""
       "-e"     (format "open location \"%s\"" url)
       "-e"     "activate"
       "-e"     "play front document"
       "-e" "end tell"))

;;;; Time and date

(defun ts()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (shell-command-to-string "date +%Y-%m-%d")))

;;;; Search

(defalias 'pos 'goto-char)

(defun prefix (string)
  (interactive "MPrefix: ")
  (query-replace-regexp "^" string))

;; Write (grep) results; Emacs auto-saves them to the right buffer.
(use-package wgrep ;; built in
    :init (setq-default wgrep-auto-save-buffer t)  
    :config (setq wgrep-auto-save-buffer t))

;; (defun rg-in-current-directory-qdw (regexp)
;;   (interactive "srg ")
;;   (let ((current-dir (if (derived-mode-p 'dired-mode)
;;                          dired-directory
;;                          (file-name-directory (buffer-file-name)))))
;;     (message regexp)
;;     (fd-grep-dired current-dir regexp)))

(defun grep-buffer-qdw (regexp)
  (interactive "sSearch this buffer for regexp: ")
  (occur regexp))
  
(defun grep-all-buffers-qdw (regexp)
  (interactive "sSearch all buffers for regexp: ")
   (multi-occur-in-matching-buffers
     "."
     regexp
     nil))
  
(defun g ()
  (interactive)
  (let ((command-string (read-string "Shell command: " "grep -HnE --color=always"))
        (original-underline-face (face-attribute 'underline :underline)))
    (unwind-protect
      (progn
        ;; Momentarily disable underlining; it looks bad in compilation-mode
        (set-face-attribute 'underline nil :underline t)
        (compile (concat command-string " " buffer-file-name)
                 't)))
    (set-face-attribute 'underline nil :underline original-underline-face)))

;;;; Hack macOS sandbox-exec (.sb) files, which are written in a Lisp dialect

(use-package lisp-mode
  :mode ("\\.sb\\'"))

;;;; Hack Emacs Lisp

;; broken
;; (use-package elisp-mode
;;   :after custom font-lock-mode    
;;   :config
;;   (custom-set-faces `(font-lock-builtin-face
;;                       ((t ((:foreground ,blue)))))))

  
;;;; Hack Python

(use-package python
  :config (setopt python-indent-offset 4)
          (add-hook 'python-mode-hook
                    (lambda ()
                      ;; Auto-indent my own code, but not third-party code.
                      ;; In this case, "my own code" means any file
                      ;; whose path starts with ~/src/1/
                      (if (string-match
                           (concat "^" (file-truename (expand-file-name "~/src/1/")))
                           (file-truename buffer-file-name))
                          (blacken-mode +1)
                        (blacken-mode -1)))))

;;;; tail -f settings

(defalias 'tail 'auto-revert-tail-mode)
  
(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

;;;; various_CaseConvention-settings

(defun kebab-to-camel-case ()
  "Convert the kebab-case word at point to CamelCase."
  (interactive)
  (let ((case-fold-search nil))
    (save-excursion
      (when (re-search-backward "\\b\\(\\w+-\\)+\\w+\\b" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (goto-char start)
          (while (re-search-forward "-\\(\\w\\)" end t)
            (replace-match (upcase (match-string 1)) t t))
          (goto-char start)
          (while (re-search-forward "-" end t)
            (replace-match "" t t)))))))

;;;; Misc. utility functions

(defun convert-to-lisp-case-qdw (str)
    (downcase (replace-regexp-in-string "[[:blank:][:punct:]]+" "-" str)))

(defun canonical-path-qdw (pth)
  (shell-quote-argument
    (expand-file-name
      (url-unhex-string
        pth))))

;;;; Editor MACroS

;; (defun remove-annoying-sh-mode-map-bindings ()
;;   "C-c is supposed to be the prefix for user-defined keybindings, generally speaking, but shell-script-mode sets a bunch of C-c bindings that do dumb stuff like inserting syntactic constructs like case statements, for loops, and so on. I'm fine with typing those out like a normal person, so I don't want these features, and they're clobbering my personal keybindings. So unmap them."
;;   (interactive)
;;   (let ((c-c-bindings
;;          (list "C-c C-d"  ;; sh-cd-here             
;;                "C-c C-f"  ;; sh-for                 
;;                "C-c TAB"  ;; sh-if                  
;;                "C-c C-l"  ;; sh-indexed-loop        
;;                "C-c C-n"  ;; sh-send-line-or-region-
;;                "C-c C-o"  ;; sh-while-getopts       
;;                "C-c C-r"  ;; sh-repeat              
;;                "C-c C-s"  ;; sh-select              
;;                "C-c C-t"  ;; sh-tmp-file            
;;                "C-c C-u"  ;; sh-until               
;;                "C-c C-w"  ;; sh-while               
;;                "C-c C-x"  ;; executable-interpret   
;;                "C-c C-z"  ;; sh-show-shell          
;;                "C-c C-\\" ;; sh-backslash-region    
;;                "C-c ("    ;; sh-function            
;;                "C-c +"    ;; sh-add                 
;;                "C-c :"    ;; sh-set-shell
;;                "C-c <"    ;; }
;;                "C-c ="    ;; }}
;;                "C-c >"    ;; }}} indentation-related functions
;;                "C-c ?"    ;; }}
;;                           ;; }
;;                )))
;;     (dolist (binding c-c-bindings)
;;       (keymap-unset sh-mode-map binding))))
;; (remove-annoying-sh-mode-map-bindings)


;;; Global keybindings that override major and minor modes' bindings.

(defvar override-mode-map-qdw (make-sparse-keymap)
  "Keymap for `qdw-override-mode', used to override other modes' keymaps.")

(define-minor-mode keymap-override-mode-qdw
  "Minor mode that, when on, overrides other modes' keybindings. I do this be3cause some packages fail to respect the convention that `C-c [letter]' bindings should be reserved for the user's personal use"
  :global t
  :keymap override-mode-map-qdw)

(keymap-override-mode-qdw +1)

(mapc
 (lambda (ls) (define-key override-mode-map-qdw
                          (kbd (car ls))
                          (cadr ls)))

 '(
   ;; FIXME: C-c [number] and C-c C-c aren't reserved for the user; some packages use them. Therefore change my number and C-c C-c mappings to one of the following: 1) C-c [letter] (capital letters allowed); 2) C-c [letter-as-prefix] [additional-letter]; or 3) xah-prefix-style bindings.
   ("C-h F"    describe-face)

   ("M-!"  shell-command-qdw)
   ("M-("  eval-expression)
   ("M-5"  query-replace-regexp)
   ("M-w"  unfill-paragraph-xah)
   ("M-o"  other-window-reverse)
   
   ("C-c <down>"   move-line-down-qdw)
   ("C-c <up>"     move-line-up-qdw)
   ("C-x C-t"      move-line-up-qdw) ;; override transpose-lines
   ("C-c C-c /"    grep-all-buffers-qdw)
   ("C-c C-c C-b"  edit-bashrc)
   ("C-c C-c C-g"  rg-in-current-directory-qdw)
   ("C-c C-c SPC"  iterm2-here)
   ("C-c C-c TAB"  ini) ;; a.k.a. C-c C-c C-i
   ("C-c C-c C-o"  (lambda ()
                     (interactive)
                     (find-file (expand-file-name "~/personal.org"))))
   ("C-c C-c C-t"  switch-theme)
   ("C-c o"    open-thing-at-point)
   ("C-c /"    grep-buffer-qdw)
   ("C-c |"    display-fill-column-indicator-mode)
   ("C-c 1"    async-shell-command)
   ("C-c :"    eval-expression)
   ("C-c ;"    comment-dwim)
   ("C-c C-f"  multi-occur-in-matching-buffers)
   ("C-c c"    comment-region)
   ("C-c g"    search-in-buffers)
   ("C-c i"    indent-region)
   ("C-c l"    shell)
   ("C-c n"    display-line-numbers-mode)
   ("C-c O"    rotate-windows)
   ("C-c r"    isearch-backward-regexp)
   ("C-c s"    isearch-forward-regexp)
   ("C-c t"    toggle-theme-qdw)
   ("C-c u"    uncomment-region)
   ("C-c u"    uncomment-region)
   ("C-c v"    vscode)
   ("C-c 6"    split-window-into-thirds))
 )

;;; C keybindings

(keymap-set c-mode-map "RET" 'electric-newline-and-maybe-indent)
(keymap-set c-mode-map "C-c {" 'beginning-of-defun)
(keymap-set c-mode-map "C-c }" 'end-of-defun)

;;; dired-mode keybindings

(keymap-set dired-mode-map "RET" 'dired-find-file-qdw)

;;; help-command keybindings

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;; org-mode keybindings

(keymap-set org-mode-map "C-c L" 'org-link-word-at-point-qdw)
(keymap-set org-mode-map "C-c ;"   'insert-current-song-qdw)
(keymap-set org-mode-map "C-c RET" 'org-open-at-point)
(keymap-set org-mode-map "C-c >"   'org-insert-subheading-respect-content-qdw)
(keymap-set org-mode-map "C-c ["   'org-table-move-column-left)
(keymap-set org-mode-map "C-c ]"   'org-table-move-column-right)
(keymap-set org-mode-map "C-c s"   'org-insert-slack-link)
(keymap-set org-mode-map "C-c j"   'org-insert-jira-link-qdw)
(keymap-set org-mode-map "C-c ("   'org-promote-subtree)
(keymap-set org-mode-map "C-c )"   'org-demote-subtree)
(keymap-set org-mode-map "C-c ["   'org-table-move-column-left)
(keymap-set org-mode-map "C-c ]"   'org-table-move-column-right)
(keymap-set org-mode-map "C-c s"   'org-insert-slack-link)

;;; shell-script-editing-mode keybindings

;; (define-prefix-command 'sh-mode-prefix-map)
;; (define-key sh-mode-map (kbd "C-c C-c") 'sh-mode-prefix-map)
;; (keymap-unset sh-mode-map "C-c C-c i")

;;;;;;; ) ;; end of inhinbit-message lexical binding

;;;; Experimental section

;; Experimenting with paredit

;; (use-package paredit
;;   :hook ((lisp-mode . paredit-mode)
;;          (emacs-lisp-mode . paredit-mode)
;;          (scheme-mode . paredit-mode)))


;; Experimenting with the equivalent of modern UIs' "Open Recent"
(use-package recentf)

;;;; Experimenting with special settings for viewing binary files

;; (defun use-hexl-mode-if-binary ()
;;   "If the current buffer is a binary file, switch to `hexl-mode'."
;;     (when (string-match-p "\0" (buffer-string))
;;         (hexl-mode)))
;;        
;; (add-hook 'find-file-hook #'use-hexl-mode-if-binary)

;; Experiment: broken: go to line in Git(Hub?)
;; (defun gline-qdw ()
;;   (interactive)
;;   (let* ((dir (file-name-directory buffer-file-name))
;;          (git-working-copy-root-dir (shell-command "git rev-parse --show-toplevel")))
;;     (message "dir: %s; gcd: %s" dir git-working-copy-root-dir)
;;     (if (not git-working-copy-root-dir)
;;         (message (format "Not a Git working copy: %s"
;;                          (file-name-directory buffer-file-name)))
;;         (replace-regexp git-working-copy-root-dir "" dir)
;;         (message (format "About to run gline in dir '%s':" git-working-copy-root-dir))
;;         ;; (shell-command-qdw
;;         (message
;;          (format "gline %s %s" buffer-file-name (line-number-at-pos))))))

;; In the default view, show stashes and local branches.
;; Also, move the sections around a bit to suit my preferences.

;; (setq-default magit-section-initial-visibility-alist
;;               '((stashes . show)))
              
;; (setq-default magit-status-sections-hook
;;               '(magit-insert-status-headers
;;                 magit-insert-merge-log
;;                 magit-insert-rebase-sequence
;;                 magit-insert-am-sequence
;;                 magit-insert-sequencer-sequence
;;                 magit-insert-bisect-output
;;                 magit-insert-bisect-rest
;;                 magit-insert-bisect-log
;;                 magit-insert-untracked-files
;;                 magit-insert-stashes
;;                 magit-insert-unstaged-changes
;;                 magit-insert-staged-changes
;;                 magit-insert-local-branches
;;                 magit-insert-unpushed-to-pushremote
;;                 magit-insert-unpushed-to-upstream-or-recent
;;                 magit-insert-unpulled-from-pushremote
;;                 magit-insert-unpulled-from-upstream))
;; 
;; (defun magit-try ()
;;   (interactive)
;;   (magit-stage-modified)
;;   (magit-commit)
;;   (magit-push))

;;; Toggle-able keybindings

(defun unkill ()
  (interactive)
  (keymap-local-set "C-k" 'ignore)
  (keymap-local-set "C-x C-c" 'ignore))


;; Experimenting with ghosttext (emacsclient as editor for web textareas)

;; (use-package ghosttext :ensure t)

;; Experimenting with straight men (package management with version-pinning)

;; (use-package straight :ensure t)

;;;; HTTP client
;; (Use-package verb :ensure t)

;;;; The end

;;;; Customizations

;; The remaining code in this file - various calls to (custom-set-*)
;; - was added by the Emacs built-in Custom system. Don't mess with it.

;;;; ---------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-theme-directory self-authored-themes-dir)
 '(package-selected-packages
   '(applescript-mode blacken company copilot csv-mode dumb-jump elfeed
     elisp-slime-nav ellama elpher evil exec-path-from-shell fd-dired
     fish-mode flymake flymake-hlint fretboard go-mode helpful
     hide-lines imenu-list jenkinsfile-mode json-mode julia-mode
     lua-mode magit markdown-mode moonscript nnreddit nushell-mode
     org-alert org-caldav org-hide-drawers org-super-agenda
     org-transclusion osx-trash paredit plz-see popper popup
     slime-repl-ansi-color smartparens terraform-mode tidal tide
     tree-sitter-langs typescript-mode vimrc-mode wgrep yaml-mode
     yaml-pro yaml-tomato))
 '(safe-local-variable-values '((Lexical-binding . t) (eval keymap-local-unset "C-x k"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sh-font-lock-quoted-subshell ((t (:foreground "#d8a0d0"))))
 '(sh-heredoc ((t (:foreground "#3b8560" :bold nil))))
 '(sh-quoted-exec ((t (:foreground "#d8a0d0")))))
