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

;;;; Inhibit superfluous messsages.

(setopt inhibit-startup-echo-area-message "quinn"
        ad-redefinition-action 'accept
        use-package-always-demand t
        warning-suppress-types '((files))) ;; lexical-binding warnings

(remove-hook 'find-file-hook 'vc-refresh-state) ;; we'll use Magit instead.
(remove-hook 'find-file-hook 'epa-file-find-file-hook)
(remove-hook 'find-file-hook 'url-handlers-set-buffer-mode)

;;;;;; (let ((inhibit-messsage t))

;; Remove chrome (menu bar/tool bar/scroll bar).

;; Behold the inconsistency of Emacs conventions!

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

;;;; GUI settings. If you're running in a terminal emulator, these are no-ops.

(set-face-font 'default "Menlo-17")
(blink-cursor-mode -1)


;;;; Color-theme settings

(defvar themes-qdw '("amber" "amber-without-bold" "mariana" "tritium"))

(mapc
 (lambda (file-name)
         (add-to-list 'custom-theme-load-path (expand-file-name file-name)))
 '("~/.emacs.d/qdw-themes"
   "~/src/forks/mariana-theme"))

(setopt custom-enabled-themes '())
(defvar my-chosen-theme "tritium")

;; Load my current favorite theme. But keep the cursor bright orange for visibility.

(load-theme (intern my-chosen-theme) 't)
(set-cursor-color "DarkOrange1")

;;;; Window manipulation settings.

;; Note that, in Emacs, the term "window" means what is called in modern
;; parlance a pane or viewport. This is because Emacs was written before
;; your primitive human GUIs existed.

(defun split-window-into-thirds ()
  "Split the current frame into three equal-width vertical windows."
  (interactive)
  (delete-other-windows)
  (let* ((total-width (window-total-width))
         (third (/ total-width 3)))
    (split-window-right third)
    (other-window 1)
    (split-window-right third)
    (balance-windows)))

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

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
  
;;;; Basic text-editing behavior
 
;; Text-entry settings: overwrite or delete the "region" (selected text)
;; when I type, in keeping with modern UI conventions.

(delete-selection-mode +1)

;; Display long lines as ......$,
;; rather than displaying the whole text-line on multiple terminal-lines
;; using Emacs ......\ notation

(defalias 'vim-newlines 'toggle-truncate-lines)

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

;;;; Dependency settings

;; Settings for the Emacs package system

(setopt package-archives
        '(("gnu"    . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa"  . "https://melpa.org/packages/")))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Explicitly load everything at Emacs init time,
;; since I'm running in --fg-daemon mode and I don't
;; want delays later when things lazy-load.
;; 
;; For documentation on use-package, see
;;
;; https://github.com/jwiegley/use-package-examples

(setq use-package-always-demand t)  ;; load at Emacs init time
;; use-package-always-ensure t) ;; auto-install from repo if not installed

(use-package ansi-color)
(use-package cc-mode)
(use-package comint)
(use-package compile)
(use-package cperl-mode
  :mode ("\\.pl\\'" . cperl-mode)
  :interpreter ("perl" . cperl-mode)
  :custom (cperl-indent-level 4))
(use-package dired)
(use-package dirtrack)
(use-package ellama)
(use-package hide-lines
  :config (defun org-skel-qdw ()
            (interactive)
            (hide-lines-not-matching "PROJECT\|SUBPROJECT\|NEXT_ACTION")))

(use-package imenu)
(use-package sh-script
    :init (advice-add 'sh-set-shell
                      :around
                      (lambda (orig &rest args)
                        (let ((inhibit-message 't))
                          (apply orig args)))))
(use-package json)
(use-package markdown-mode)

(use-package url-util)

;;;; Hack org-mode documents

(use-package org

  :after url-util
    
  :config

  (setopt org-archive "::"
          org-catch-invisible-edits "show"
          org-cycle-separator-lines 1
          org-return-follows-link 't

          ;; default org formatting; consider changing
          ;; 
          ;; org-emphasis-alist '(("*" bold)
          ;;                      ("/" italic)
          ;;                      ("_" underline)
          ;;                      ("=" org-verbatim verbatim)
          ;;                      ("`" org-code verbatim)
          ;;                      ("~" (:strike-through t)))

          ;; custom value
          ;; 
          ;; org-emphasis-alist '(("*" bold)
          ;;                      ("/" italic)
          ;;                      ("_" underline)
          ;;                      ("=" org-verbatim verbatim)
          ;;                      ("`" org-code verbatim)
          ;;                      ("~" (:strike-through t)))

          
          org-confirm-babel-evaluate nil
          org-babel-load-languages '((emacs-lisp . t)
                                     (python . t)
                                     (shell . t)))

  ;; When visiting links to guitar tabs, open them in Emacs itself.
  (add-to-list 'org-file-apps '("\\.crd\\'" . emacs))

  ;; On macOS, open files of certain types with GUI apps (not Emacs).
  (when (string= system-type "darwin")
      (add-to-list 'org-file-apps
		           (cons "png" "/usr/bin/open %s")
                   (cons "pdf" "/usr/bin/open %s")))

    (defun org-linkify-buffer-qdw ()
    "Make each line in the buffer into an org-mode style link
by enclosing it in [[double square brackets]]. This works
for files of local paths produced by running, e.g., fd,
which is its intended use."
    (interactive)
    (goto-char (point-min))
    (replace-regexp "^" "[[")
    (goto-char (point-min))
    (replace-regexp "$" "]]"))

  (defun org-insert-jira-link-qdw ()
    (interactive)
    (letrec ((pre-filled-text (if (boundp 'jira-default-project-qdw)
                                  (concat jira-default-project-qdw "-")
                                ""))
             (issue-name (read-string
                          "Issue name (e.g. MYPROJ-0000): "
                          pre-filled-text
                          nil
                          nil
                          nil)))
      (message (format "jira-default-project-qdw: %s\npre-filled-text: %s"
		               jira-default-project-qdw
		               pre-filled-text))

      (org-insert-link nil
		               (concat jira-base-url "/browse/" issue-name)
		               issue-name)))

  (defun org-link-word-at-point-qdw ()
    "Wrap word at point in an org link, prompting for URL with org completion."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (start (copy-marker (car bounds)))
           (end (copy-marker (cdr bounds)))
           (offset (- (point) (car bounds))))
      (goto-char end)
      (condition-case nil
          (progn
            (org-insert-link nil nil word)
            (delete-region start end)
            ;; Link is now at start: [[url][word]]
            ;; Jump to description part and apply original offset
            (goto-char start)
            (when (looking-at "\\[\\[[^]]*\\]\\[")
              (goto-char (+ (match-end 0) offset))))
        (quit (goto-char (+ (marker-position start) offset))))))

  (defun org-fold-qdw ()
    "In org-mode, set the VISIBILITY property of the element at point
  to 'folded'."
    (interactive)
    (when (org-at-heading-p)
      (org-set-property "VISIBILITY" "folded")))

  (defun org-insert-file-link-qdw ()
    "Insert a link to a local file"
    (interactive)
    (let* ((filename
            (read-file-name "Link to file: "))

           (link-text
            (read-string "Link text: " (file-name-nondirectory filename))))
      (insert (format "[[file:%s][%s]]" filename link-text))))

  (defun org-insert-slack-link-qdw (channel-name)
    (interactive "MSlack channel (without the '#'): ")
    (org-insert-link
     nil
     (concat "https://" slack-team ".slack.com/channels/#" channel-name)
     (concat "#" channel-name)))
          
  (defun org-just-eval-blocks-qdw ()
    "Automatically evaluate specific org-mode #+CALL: lines on buffer visit."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+CALL:" nil t)
          (org-babel-lob-execute-maybe)))))

  (add-hook 'org-mode-hook #'org-just-eval-blocks-qdw)

  (defun org-insert-subheading-respect-content-qdw (arg)
    (interactive "P")
    (org-insert-todo-subheading-respect-content arg)
    (cond ((org-at-heading-p) (org-do-demote))
          ((org-at-item-p) (org-indent-item))))

  ;;; Music playlists in org-mode
  ;;; (this code depends on Emacs's built-in url-util package
  ;;; being loaded first, hence the :after url-util at the top of
  ;;; my org-mode use-package expression).

   ;; Apple Music

  (org-link-set-parameters
   "play"
   :follow (lambda (song-name _)
             (noninteractive-shell-command-qdw
              (concat "play " (shell-quote-argument song-name)))))
             ;; ^ The 'play' shell function is defined in my ~/.bashrc.

  (org-link-set-parameters
   "play-album"
   :follow (lambda (album-id _)
             (noninteractive-shell-command-qdw
              (concat "play-album-untested "
                      (shell-quote-argument album-id)))))
             ;; ^ The 'play-album' shell function is defined in my ~/.bashrc.

  (defun org-insert-current-song-qdw ()
  "Insert an org-mode link to the currently playing song.
On click or RET, the link will replay the song.
Depends on my Bash function `play', defined in ~/.bashrc."
    (interactive)
    (let* ((song-name (shell-command-to-string-qdw "song-name"))
           (org-code (format "[[play:%s][%s]]" song-name song-name)))
      (insert org-code)))

;; (defun insert-current-album-qdw ()
;;   (interactive)
;;   (let*

  (defun insert-vgm-qdw (path)
    "Insert an org-mode link to a local file containing video game music.
On click or RET, the link will automatically play in a (hard-coded)
player that I have chosen for its filetype. Currently supported
file formats: SID (Commodore 64), NSF (Nintendo Entertainment System)."
    (interactive (list (read-file-name "File: ")))
    (let* ((track-name (file-name-base path))
           (link-type (upcase (file-name-extension path)))
           (urlencoded-path (url-hexify-string path))
           (org-code (format "[[%s:%s][%s]]"
                             link-type
                             urlencoded-path
                             track-name)))
      (insert org-code)))

  ;; C64

  (org-link-set-parameters
   "SID"
   :follow (lambda (path _no-prefix-arg)
             (shell-command-to-string-qdw ;; string  retval is ignored
              (concat
               "open -a Cog.app "
               (canonical-path-qdw path)))))

  ;; NES

  (org-link-set-parameters
   "NSF"
   :follow (lambda (path _no-prefix-arg)
             (shell-command-to-string-qdw ;; string  retval is ignored
              (concat
               "open -a 'Audio Overload.app' "
               (canonical-path-qdw path))))))


(use-package ruby-mode)
(use-package whitespace)

(setopt load-prefer-newer 't)

;;;; Basic comfort

(put 'upcase-region     'disabled nil)
(put 'downcase-region   'disabled nil)
(put 'narrow-to-region  'disabled nil)

(setopt word-wrap nil)

(setq-default
 ;; If I write non-ASCII chars in a buffer, then save the file as Unicode
 prefer-coding-system 'utf-8

 ;; Don't auto-hard-wrap lines
 auto-fill-mode nil

 cperl-indent-level 4
 elisp-indent-level 2
 json-encoding-default-indentation "  "
 org-indent-indentation-per-level 4)

(setopt

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

 ;; Indentation: use spaces, not tabs.
 indent-tabs-mode nil

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

 ;; Make tabs-as-indentation visible, so I can eliminate them
 tab-width 4
 whitespace-style '(face
                    ;; empty
                    indentation
                    ;; trailing
                    ;; lines-tail
                    missing-newline-at-eof)

 whitespace-display-mappings '((tab-mark 9 [187 9] [92 9])))

;; Actually, turn off the foregoing for now; I haven't yet found
;; a way of displaying these things that I actually like.

(global-whitespace-mode -1)

;;;; AI stuff

;; (load-file (expand-file-name "~/src-3/emacs-copilot/copilot.el")) ;; by jart

;;;; Hack: general: automatic indentation

(electric-indent-mode +1)

;;;; Hack: general: tree-sitter
;;;; Hack C

(c-add-style
 "qdw"
 '("stroustrup"
   (c-offsets-alist
    ;; Indent case statements 1 level beyond the enclosing 'switch() {' line
    (case-label . +)
    (statement-case-intro . +)
    (statement-case-open . 0)

    ;; When breaking stuff onto multiple lines, always indent 1 level
    (arglist-cont . +)
    (arglist-cont-nonempty . +)
    (arglist-intro . +)
    
    (statement-cont . +)
    (topmost-intro-cont . +)
    (arglist-cont . +)
    (arglist-cont-nonempty . +)
    (statement-cont . +)
    (topmost-intro-cont . +)
    (brace-list-entry . +)
    (brace-list-intro . +)
    (brace-entry-open . +))

    ;; Keep the opening brace of a function body
    ;; on the same line as the function signature:
    ;; 
    ;; int foo(int arg1) {
    (c-hanging-braces-alist
     (defun-open . (nil . t)))))   ;; no newline before {, newline after

(setopt c-default-style "qdw")

;;;; Hack Emacs Lisp

(setq major-mode-remap-alist '((elisp-mode . elisp-ts-mode)
                               (lisp-data-mode . lisp-data-ts-mode)))

(defun lfi ()
  (interactive)
  (load-file-if-exists-qdw "site.el"))

(defun load-file-if-exists-qdw (file-name)
  "Load FILE-NAME if it exists, and do nothing otherwise."
  (let ((inhibit-message t))
    (condition-case nil
        (load (expand-file-name file-name user-emacs-directory) 't 't)
      (error nil))))
      
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

(add-to-list 'load-path
             (expand-file-name "qdw-themes" user-emacs-directory))

(add-to-list 'load-path
             (expand-file-name "~/src/forks/mariana-theme"))

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
  (find-file user-init-file))
  
;;;; Edit my ~/.bashrc

(defun edit-bashrc ()
  (interactive)
  (find-file (expand-file-name "~/.bashrc")))

;;;; Further theme settings

(defun if-file-is-theme-reload-theme ()
  (let ((base-name (file-name-nondirectory (file-truename buffer-file-name))))
    (when (string-match "\\(.*\\)-theme\\.el$" base-name)
      (let ((theme-name (match-string 1 base-name)))
        (when (string-in theme-name custom-enabled-themes)
          (load-theme (intern theme-name) 't 't))))))

(add-hook 'after-save-hook #'if-file-is-theme-reload-theme)

(setq org-todo-keywords '("DEFER(e)"
                          "TODO(t)"
                          "NEXT_ACTION(n)"
                          "IN_PROGRESS(i)"
                          "GET_HELP(h)"
                          "AWAITING_RESPONSE(a)"          
                          "QUESTION(q)"
                          "YAK_SHAVING(y)"
                          "INFO(f)"
                          "PROJECT(r)"
                          "SUBPROJECT(s)"
                          "POSTPONED(p)"
                          "RECOMMENDATION(c)"
                          
                          "|"

                          "DONE(d)"
                          "WONTFIX(w)"
                          ))

(defun overlay-org-colors-qdw () 
  (interactive)
  (when (or (member 'tritium custom-enabled-themes)
            (member 'mariana custom-enabled-themes))
    (let ((org-keyword-to-color-qdw
          `(("DEFER" . ,bright-cyan)
            ("TODO" . ,bright-magenta)
            ("NEXT_ACTION" . ,bright-red)
            ("IN_PROGRESS" . ,bright-yellow)
            ("GET_HELP" . ,bright-red)
            ("AWAITING_RESPONSE" . ,bright-magenta)
            ("QUESTION" . ,bright-magenta)
            ("YAK_SHAVING" . ,yellow)

            ("DONE" . ,green)
            ("WONTFIX" . ,green)
            ("POSTPONED" . ,yellow)
            ("INFO" . ,yellow)
            ("PROJECT" . ,blue)
            ("SUBPROJECT" . ,bright-cyan)
            )))

      (setopt org-todo-keyword-faces
              (mapc (lambda (pair) (add-to-list 'org-todo-keyword-faces pair))
                    org-keyword-to-color-qdw)))))

(overlay-org-colors-qdw)

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
                   (expand-file-name "~/.emacs.d/qdw-themes/")))
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

(defalias #'dwim #'open-thing-at-point)

;;;; Settings specific to GUI Emacs

  ;; When I delete files in dired, send them to the macOS Trash

  ;; (osx-trash-setup)
  ;; (setopt delete-by-moving-to-trash 't)
  ;; (defun system-move-file-to-trash (path)
  ;;   "Moves file at PATH to the macOS Trash
  ;;   according to `move-file-to-trash' convention.

  ;;   Relies on the command-line utility 'trash' to be installed.
  ;;   Get it from:  <http://hasseg.org/trash/>"
  ;;   (shell-command (concat "trash -vF \"" path "\""
  ;;                          "| sed -e 's/^/Trashed: /'")
  ;;                  nil ;; Name of output buffer
  ;;                  "*Trash Error Buffer*"))

  ;; macOS's bundled BSD ls doesn't play well with Emacs,
;; so use Homebrewed GNU ls instead

;;;; dired
  
;; (dolist (gnu-ls '("/opt/homebrew/bin/gls" "/usr/local/bin/gls"))
;;   (when (file-exists-p gnu-ls)
;;     (setopt insert-directory-program
;;             gnu-ls
            
;;             dired-use-ls-dired
;;             t
            
;;             dired-listing-switches
;;             "-aBhl --group-directories-first")))

;;;; Human-language settings (including English)

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
 ls-lisp-use-insert-directory-program nil
 mode-line-position-column-format '(" %C")
 mode-line-position-column-line-format '(" (%l,%C)")
 neo-cwd-line-style 'text
 neo-theme 'classic
 ns-alternate-modifier 'none
 ns-right-alternate-modifier 'meta
 recenter-positions '(top middle bottom))

;;;; Reading ePub ebooks

;; (add-to-list
;;  'auto-mode-alist
;;  '("\\.epub\\'" .
;;    (lambda ()
;;      (add-to-list 'load-path (expand-file-name "~/src-3/nov.el") nil)
;;      (load-file (expand-file-name "~/src-3/nov.el/nov.el"))
;;      (nov-mode))))

;;;; Reading RSS and Atom feeds

;; (use-package elfeed
;;   :)

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

(defun noninteractive-shell-command-qdw (command)
  (shell-command
    (concat "source ~/.bashrc && INSIDE_EMACS=1 " command)
    "*qdw-stdout*"
    "*qdw-stderr*"))

;;;; Settings for running interactive shells inside Emacs (via `M-x shell`)

(setq-default shell-prompt-regexp "[~][/].*[$][ ]")

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
    ;; :config (setq wgrep-auto-save-buffer t)
    )

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

;;;; Hack Makefiles

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode 't)))

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

;;; Global keybindings that get overridden by major and minor modes' bindings.

(keymap-global-set "C-c C-c C-p"
                   (lambda ()
                     (interactive)
                     (find-file (expand-file-name "~/personal.org"))))

;;; Global keybindings that override major and minor modes' bindings.



(defvar override-mode-map-qdw (make-sparse-keymap)
  "Keymap for `qdw-override-mode', used to override other modes' keymaps.")

(mapc
 (lambda (ls) (define-key override-mode-map-qdw
                          (kbd (car ls))
                          (cadr ls)))

 '(("C-c <up>"     move-line-up-qdw)
   ("C-c <down>" move-line-down-qdw)
   ("M-!"         shell-command-qdw)
   ("M-`"         unfill-paragraph-xah)
   ("C-c u"       uncomment-region)
   ("C-c ."       open-thing-at-point)
   ("C-c /"       grep-buffer-qdw)
   ("C-c C-c ."   grep-all-buffers-qdw)
   ("C-c g"       search-in-buffers)
   ("C-c 1"       async-shell-command)
   ("C-c :"       eval-expression)
   ("C-h F"       describe-face)
   ("C-c C-f"     multi-occur-in-matching-buffers)
   ("C-c c"       comment-region)
   ("C-c C-c C-g" rg-in-current-directory-qdw)
   ("C-c i"       indent-region)
   ("C-c C-c b"   edit-bashrc)
   ("C-c C-c TAB" ini)
   ("C-c SPC"     iterm2-here)
   ("C-c l"       shell)
   ("C-c n"       display-line-numbers-mode)
   ("C-c o"       rotate-windows)
   ("C-c s"       isearch-forward-regexp)
   ("C-c r"       isearch-backward-regexp)
   ("C-c C-c C-t" switch-theme)
   ("C-c t"       toggle-theme-qdw)
   ("C-c u"       uncomment-region)
   ("C-c v"       vscode)
   ("C-x 6"       split-window-into-thirds)
   ("M-("         eval-expression)
   ("M-5"         query-replace-regexp)
   ("M-o"         other-window-reverse)))

(define-minor-mode keymap-override-mode-qdw
  "Minor mode to override major and minor mode keybindings."
  :global t
  :keymap override-mode-map-qdw)

(keymap-override-mode-qdw +1) ; enable the override mode

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
;;          (emacs-lisp-mode . paredit-mode)))


;; Experimenting with org-superagenda

;; (use-package org-superagenda)

;; Experimenting with the equovalent of modern UIs' "Open Recent"

;; (use-package recentf)

;;;; Experimenting with special settings for viewing binary files

;; (defun use-hexl-mode-if-binary ()
;;   "If the current buffer is a binary file, switch to `hexl-mode'."
;;     (when (string-match-p "\0" (buffer-string))
;;         (hexl-mode)))
;;        
;; (add-hook 'find-file-hook #'use-hexl-mode-if-binary)

;;;; Experimenting with Git/Magit settings (note to self: re-enable now that that bug is fixed!)

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

;; Experimenting with poppers

(use-package popper
  :ensure t ; or :straight t

  :bind (("C-c C-q" . popper-toggle)
         ("C-c TAB" . popper-cycle)
         ("C-c b"   . popper-toggle-type))

  :init (setq popper-reference-buffers '("\\*Messages\\*"
                                         "Output\\*$"
                                         "\\*Async Shell Command\\*"
                                         help-mode
                                         compilation-mode))
        (popper-mode +1)
        (popper-echo-mode +1))

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
 '(custom-theme-directory "~/.emacs.d/qdw-themes")
 '(package-selected-packages
   '(applescript-mode blacken company copilot csv-mode dumb-jump elfeed
     elisp-slime-nav ellama elpher evil exec-path-from-shell fd-dired
     fish-mode flymake flymake-hlint fretboard go-mode helpful
     hide-lines imenu-list jenkinsfile-mode json-mode julia-mode
     lua-mode magit markdown-mode moonscript nnreddit nushell-mode
     org-alert org-caldav org-super-agenda org-transclusion osx-trash
     paredit plz-see popper popup slime-repl-ansi-color smartparens
     terraform-mode tidal tide tree-sitter-langs typescript-mode
     vimrc-mode wgrep yaml-mode yaml-pro yaml-tomato))
 '(safe-local-variable-values '((eval keymap-local-unset "C-x k"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sh-font-lock-quoted-subshell ((t (:foreground "#d8a0d0"))))
 '(sh-heredoc ((t (:foreground "#3b8560" :bold nil))))
 '(sh-quoted-exec ((t (:foreground "#d8a0d0")))))
