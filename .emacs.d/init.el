;;;; Window-chrome settings

(setq inhibit-startup-screen 't)
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;; Settings for GUI Emacs versus terminal Emacs

(defun initialize-ui ()
  (interactive)
  (cond

    ((display-graphic-p)
     (set-face-font 'default "Menlo-17")
     (blink-cursor-mode -1))
    
    ('t
     (xterm-mouse-mode 1)
     (global-set-key [mouse-4]
                     (lambda () (interactive) (scroll-down 1)))
     (global-set-key [mouse-5]
                     (lambda () (interactive) (scroll-up 1)))
     (defun track-mouse (e))
     (setq mouse-sel-mode 't))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (initialize-ui)))

(initialize-ui)

;;;; Dependency/library settings

;; Unpacked third-party libraries that I've downloaded

(add-to-list 'load-path (expand-file-name "~/.site.lisp.d"))

(setq-default load-prefer-newer 't)

;; I'm running emacs as a daemon,
;; so just require all these modules at once;
;; don't get clever with lazy loading.
(require 'ansi-color)
(require 'blacken)
(require 'comint)
(require 'compile)
(require 'devil)
(require 'dirtrack)
(require 'dockerfile-mode)
(require 'fd-dired)
(require 'flycheck)
(require 'json)
;; (require 'lyrics-fetcher)
(require 'magit)
;; (require 'magithub)
(require 'menu-bar)
(require 'mouse)
(require 'origami)
(require 'org)
(require 'osx-trash)
(require 'package)
(require 'paredit)
(require 'recentf)
;; (require 'restclient)
(require 'ruby-mode)
(require 'scroll-bar)
(require 'sh-script)
(require 'tide)
(require 'tool-bar)
(require 'typescript-mode)
(require 'whitespace)
(require 'yaml-mode)

;;;; Edit this file

(defun ini ()
  (interactive)
  (find-file user-init-file))

;;;; Color-theme settings (plus site-specific settings)

(defun load-if-present-qdw (file-name)
  "Load FILE-NAME if it exists, and do nothing otherwise."
  (condition-case nil
      (load-file (expand-file-name file-name user-emacs-directory))
    (error nil)))

(load-if-present-qdw "site.el")
(when (not (boundp 'my-chosen-theme))
      (setq-default my-chosen-theme "tritium"))
(load-file (expand-file-name "my-themes.el" user-emacs-directory))

(defun switch-theme ()
  (interactive)
  (customize-variable 'custom-enabled-themes))

;; Disable bold type passim.

(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal))
 (face-list))
 
;; Always use the same cursor color, no matter what theme I use.

(set-cursor-color "DarkOrange1")

;;;; Machine-specific settings

(load-if-present-qdw "site.el")

(load-if-present-qdw "secrets.el")

;;;; Utility functions used in this file

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
  
;;;; .ignore settings
;;;; This file is formatted exactly like .gitignore,
;;;; and is honored by fd, rg, and maybe others?

(add-to-list 'auto-mode-alist
             '("\\.ignore\\'" . gitignore-mode))
   

;;;; dired settings

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

;;;; Alternative-editor settings

;; If a file is open in Emacs and another editor saves the file,
;; update the one in Emacs. Also do this for dired.
(setq-default auto-revert-verbose nil)
(when (not global-auto-revert-mode)
      (global-auto-revert-mode 't))

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

(defun subl ()
  (interactive)
    (alt-editor
     "/opt/homebrew/bin/subl"
     (file-line-column-colon-separated)))

(defun vscode ()
  (interactive)
  (alt-editor
   "/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code"
   "--goto" (file-line-column-colon-separated)))

(defalias 'code 'vscode)

;; (defun open-colon-separated (editor-file-name &optional args)
;;   (interactive)
;;   (when buffer-file-name
;;     (let ((command
;;            (cons editor-file-name args)))
;;       (alt-editor command
;;        editor-file-name ))))

;;;; QoL settings

(defun goto-column (num)
  "Move to column N (1-based) of the current line"
  (interactive "nGoto column: ")
  (let ((one-based-column-number
         (cond ((not num)   0)
               ((eq num 0)  0)
               ('t          (1- num)))))
    (move-to-column one-based-column-number)))
    

(defun find-file-at-point-with-line-and-column-if-present ()
  "Open the file at point and navigate to the specified line and column, if provided."
  (interactive)
  (letrec ((thing (string-trim (thing-at-point 'line 't)))
           (parts (split-string thing ":"))
           (file-name (or (nth 0 parts)
                          parts))                          
           (lin (if (and (nth 1 parts)
                         (string-match-p "^[0-9]+$" (nth 1 parts)))
                    (string-to-number (nth 1 parts))
                  1))
           (col (if (and (nth 2 parts)
                         (string-match-p "^[0-9]+$" (nth 2 parts)))
                    (string-to-number (nth 2 parts))
                  1)))
    (find-file file-name)
    (message (format "goto line %s" lin))
    (goto-line lin)
    (message (format "goto column %s" col))
    (goto-column col)))

(defalias 'dwim 'find-file-at-point-with-line-and-column-if-present)

;;;; GUI Emacs settings

;;;; macOS settings

(when (string= system-type "darwin")
  ;; Open PNGs with the system PNG viewer, not in Emacs
  ;; (org-mode only).
  (add-to-list 'org-file-apps
               (cons "png" "open %s"))


  ;; When I delete files in dired, send them to the macOS Trash

  (osx-trash-setup)
  (setq-default delete-by-moving-to-trash 't)
  (defun system-move-file-to-trash (path)
    "Moves file at PATH to the macOS Trash
    according to `move-file-to-trash' convention.

    Relies on the command-line utility 'trash' to be installed.
    Get it from:  <http://hasseg.org/trash/>"
    (shell-command (concat "trash -vF \"" path "\""
                           "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*"))

  ;; macOS's bundled BSD ls doesn't play well with Emacs,
  ;; so use Homebrewed GNU ls instead
  
  (dolist (gnu-ls '("/opt/homebrew/bin/gls" "/usr/local/bin/gls"))
    (when (file-exists-p gnu-ls)
      (setq insert-directory-program
            gnu-ls
            
            dired-use-ls-dired
            t
            
            dired-listing-switches
            "-aBhl --group-directories-first"))))

;;;; Human-language settings (including English)

;; From xahlee.org
(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))
 
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;;; eBook/e-reader settings

(add-to-list
 'auto-mode-alist
 '("\\.epub\\'" .
   (lambda ()
     (add-to-list 'load-path (expand-file-name "~/src-3/nov.el") nil)
     (load-file (expand-file-name "~/src-3/nov.el/nov.el"))
     (nov-mode))))

;;;; Shell settings

;; When running shell commands, source my ~/.bashrc first,
;; so that my many custom shell functions are available,
;; as they would be in a terminal emulator session.

(defun login-shell-command (command-string)
  (interactive "MShell command: ")
  (shell-command (concat "source ~/.bashrc && " command-string)))
  
(defun shell-in-dir ()
  "Open an existing or new shell, cd'ing to the current dir if appropriate"
  (interactive)
  (letrec ((dir (if buffer-file-name
                    (file-name-directory buffer-file-name)
                    nil)))
    (shell)
    (when dir

      ;; (goto-char (point-max))
      ;; (comint-simple-send "cd " + dir))))
      
      (shell-cd dir))))

;; Depends on my ~/.bashrc, which sets HOMEBREW_BIN

(when
  (getenv "HOMEBREW_BIN")
  (setq explicit-shell-file-name
        (concat (getenv "HOMEBREW_BIN") "/bash")))

;; Don't display the STDOUT or STDERR of quick shell commands
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-no-window)
               (allow-no-window . t)))

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

;; Before every command, run `dirs`. This should prevent shell-mode
;; from frequently getting confused about what dir is the current
;; working directory, as it often does.

(defun dirs-qdw (ignored-string)
  (when (derived-mode-p 'shell-mode)
    (shell-resync-dirs)))

;; (add-hook 'comint-input-filter-functions 'dirs-qdw)
;; (add-hook 'comint-output-filter-functions 'dirs-qdw)

;;;; Settings for the Emacs package system
  
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)
;; (package-install-selected-packages)

;;;; Basic comfort

(setq-default
 ;;; Settings for use with macOS

 ;; Consider filenames case-sensitive
 read-file-name-completion-ignore-case nil 

 ;; If I write non-ASCII chars in a buffer, save the file as Unicode
 prefer-coding-system 'utf-8
 
 ;; Warn when opening large files
 large-file-warning-threshold 100000000

 ;; Some Unix command-line utilities require a final newline
 ;; (e.g. 'wc -l')
 require-final-newline 't

 ;; Natural-language settings

 ; Don't auto-hard-wrap lines!
 auto-fill-mode nil
 word-wrap nil
 
 ; Single-space: always
 colon-double-space nil 
 
 ; Single-space. Always
 sentence-end-double-space nil

 ; If I have a region selected, and I do some typing,
 ; overwrite region when I type. This is how modern GUIs work.
 delete-selection-mode +1

 ;; Auto-indentation settings

 ; Use spaces, not tabs.
 indent-tabs-mode nil
 cperl-indent-level 4
 elisp-indent-level 2
 json-encoding-default-indentation "  "
 org-indent-indentation-per-level 4
 
 ; The first day of the week is Monday.
 calendar-week-start-day 1
 
 ; If I have a region selected, then make command like query-replace
 ; operate on that region only
 transient-mark-mode 't

 make-backup-files nil
 
 ; Automatically follow symlinks to git-controlled files; don't ask
 vc-follow-symlinks 't
 
 cursor-type 'box
 column-number-mode nil
 
 ; 't means always highlight as much distinct syntax as is supported
 global-font-lock-mode 't

 ; Instead of beeping, flash screen
 visible-bell 't
 
 )

;; Customized visual bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg)
                                 (set-face-foreground 'mode-line fg))
                               orig-fg))))
(tool-bar-mode -1)   ;; Turn off toolbar.
(menu-bar-mode -1)   ;; Turn off menubar.
(scroll-bar-mode -1) ;; Turn off scrollbar.

;;;; Time-related settings

(defun ts()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


;;;; Search/compile/shell-command output buffer settings

(defun disable-underline-qdw (orig-fun &rest args)
  "Advice around `compile` to temporarily disable underlining (it looks bad)."
  (let ((original-underline-face (face-attribute 'underline :underline)))
    (unwind-protect
      (progn
          ;; Call the original compile function
          (apply orig-fun args)
          ;; Change the 'underline' face in the compilation buffer
          (with-current-buffer "*compilation*"
            (set-face-attribute 'underline nil :underline nil)))
      ;; Restore the original 'underline' face
      (set-face-attribute 'underline nil :underline original-underline-face))))

(advice-add 'compile :around #'disable-underline-qdw)

;;;; Search settings

(setq-default
 ;; Make isearch case-sensitive by default.
 case-fold-search 't

 isearch-allow-scroll 'unlimited)
 
(defun search-in-buffers (regexp)
  (interactive "sSearch for regexp: ")
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

;;;; org-mode settings

(setq-default
  org-agenda-restore-windows-after-quit 't
  org-startup-folded 't
  org-cycle-separator-lines -2
  org-return-follows-link 't 
  org-agenda-files (list (expand-file-name "~/main.org")
                         (expand-file-name "~/personal.org")
                         ))

(defun org-insert-jira-link-qdw ()
  (interactive)
  (letrec ((pre-filled-text (if (boundp 'jira-default-project-qdw)
                                (concat jira-default-project-qdw "-")
                                ""))
           (ticket-name (read-string
                         "Ticket name (e.g. MYPROJ-000): "
                         pre-filled-text
                         nil
                         nil
                         nil)))
    (message (format "jira-default-project-qdw: %s\npre-filled-text: %s"
                     jira-default-project-qdw
                     pre-filled-text))

    (org-insert-link nil
                     (concat jiralib-url "/browse/" ticket-name)
                     ticket-name)))
                     
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

;; This regexp is correct...
(setq jira-ticket-regexp-qdw
      "\\b[[:upper:]]\\{2,10\\}-[[:digit:]]-\\{2,\\9}")
;; ... but this function is broken                              
;; (defun qdw-isearch-forward-jira-ticket
;;   (interactive)
;;   (isearch-resume jira-ticket-regexp-qdw 't nil 't nil 't))
    
(defun org-insert-pr-link-qdw (repo pr-number)
  "Insert a link to a GitHub pull request.
  REPO is the name of the repository,
  and PR-NUMBER is, well, what it sounds like."
  (interactive "MRepo: \nnPR number: ")
  (org-insert-link nil
                   (format "https://github.com/%s/%s/pull/%s"
                           getenv("GITHUB_ORG")
                           repo
                            pr-number)))

(setq-default
  org-catch-invisible-edits "show"
  org-todo-keywords '((sequence
                       "TODO(t)"
                       "QUESTION(q)"
                       "NEXT_ACTION(n)"
                       "AWAITING_RESPONSE(a)"
                       "IN_PROGRESS(i)"
                       "GET_HELP(h)"
                       "INFO(f)"
                       "PROJECT(r)"
                       "SUBPROJECT(s)"
                       
                       "|"
                       
                       "DONE(d)"
                       "POSTPONED(p)"
                       "WONTFIX(w)"
                       )))

;; Hide the PROPERTIES section if folded;
;; pasted from https://stackoverflow.com/a/17492723
(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                    (point-min)
                    (point)))
             (end (if globalp
                    (point-max)
                    (if (eq state 'children)
                      (save-excursion
                        (outline-next-heading)
                        (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                       (save-excursion
                         (outline-next-heading)
                           (point)))
                     (msg (format
                            (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                            (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                  (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))


(defun org-fold-qdw ()
  "In org-mode, set the VISIBILITY property of the element at point
  to 'folded'."
  (interactive)
  (when (org-at-heading-p)
    (org-set-property "VISIBILITY" "folded")))

(defun org-insert-file-link-qdw ()
  "Insert a link to a local file."
  (interactive)
  (let*
    ((filename
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

(defun org-insert-pr-link-qdw (repo pr-number)
  (interactive "MRepo: \nnPR number: ")
  (org-insert-link nil
                   (format "https://github.com/%s/%s/pull/%s"
                           getenv("GITHUB_ORG")
                           repo
                           pr-number)))

(defun org-insert-subheading-respect-content-qdw (arg)
  (interactive "P")
  (org-insert-todo-subheading-respect-content arg)
    (cond
      ((org-at-heading-p) (org-do-demote))
      ((org-at-item-p) (org-indent-item))))


;;;; Viewport management

;; Cycle through viewports.
(defun rotate-windows () ;; from whattheemacsd.com
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
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
             (setq i (1+ i)))))))
             
(defun other-window-reverse ()
  (interactive)
  (other-window -1))
  
;;;; Scrolling settings

(setq-default
 ;; Auto Scroll: keep me from craning my neck
 ;; by automatically scrolling when the cursor gets
 ;; a few lines from the top or bottom of a buffer
 ;; scroll-margin 13
 
 ;; When I first press Ctrl-L while on a given line,
 ;; scroll so that the line appears at the top,
 ;; not the middle, of the screen.
 ;;
 ;; This behavior is affected by the setting of
 ;; scroll-margin: the top of the screen is
 ;; defined as line 1 plus scroll-margin
 ;; (same for the bottom).
 recenter-positions '(top middle bottom))

;;;; tail settings

(defun tail ()
  (interactive)
  (auto-revert-tail-mode))
  
;;;; diff, merge, and git settings

(defun open-current-line-in-github-web-ui-qdw ()
  (interactive)
  (let*
    ((filename
      (file-name-nondirectory (buffer-file-name)))
     (line
      (number-to-string (line-number-at-pos))))
    ;; (src-dir-regexp
    ;; (concat home "/src.*/")
    ;; 
    ;; (truncated-filename
    ;; (replace-regexp-in-string src-dir-regexp "" filename 't 't))
    ;; 
    ;; (print (concat filename ":" line))))
    (async-shell-command (concat "gh browse " filename ":" line)
                         "emacs-gh-stdout"
                         "emacs-gh-stderr")))


;; If I have a Homebrewed git installed, use that
;; instead of /usr/bin/git
(dolist (git-executable
          '("/opt/homebrew/bin/git" "/usr/local/bin/git"))
  (when (file-exists-p git-executable)
    (setq-default magit-git-executable git-executable)))

;;; diff and merge settings

;; When invoking ediff commands, use a new pane
;; to the right of the current pane
;; (don't create a new frame).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; File-handling settings 

;; Automatically make file executable if it starts with a #!
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Globally enable dumb-jump, an it-just-works way to jump
;; from functions' invocations to their definitions of those
;; functions, which supports many languages.
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defun my-enter ()
  "Inserts a newline character then indents the new line just
   like the previous line"
  (interactive)
  (newline)
  (unless (derived-mode-p 'mhtml-mode)
          (indent-relative-maybe)))

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

;; Code-folding

(global-origami-mode)

;;;; Interactive-shell settings

(setq-default shell-prompt-regexp "[~][/].*[$][ ]")

(defun shell-dirstack-update (output)
  (when (string-match-p comint-prompt-regexp output)
    (comint-send-string (get-buffer-process (current-buffer)) "dirs\n")))

(add-hook 'comint-output-filter-functions 'shell-dirstack-update nil t)

;;;; Settings for writing shell scripts

;; Don't auto-insert '<< EOF'-style heredocs when I type '<<',
;; since I have my own preferred way of writing heredocs.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-electric-here-document-mode nil)))

;;;; Lisp settings

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
(setq lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook (lambda ()
                            (paredit-mode +1)
                            (eldoc-mode +1)))

;; When I run C-(
;; (which lets me write and eval a quick Lisp expression
;; in the minibuffer),
;; add the opening parenthesis for me.
(add-hook 'eval-expression-minibuffer-setup-hook
	  (lambda ()
	    (insert "(")))

;;;; JSON settings

(add-hook 'json-mode-hook
          (lambda ()
            (origami-mode +1)))

;;;; Magit settings

;; In the default view, show stashes and local branches.
;; Also, move the sections around a bit to suit my preferences.

(setq-default magit-section-initial-visibility-alist
              '((stashes . show)))
              
(setq-default magit-status-sections-hook
              '(magit-insert-status-headers
                magit-insert-merge-log
                magit-insert-rebase-sequence
                magit-insert-am-sequence
                magit-insert-sequencer-sequence
                magit-insert-bisect-output
                magit-insert-bisect-rest
                magit-insert-bisect-log
                magit-insert-untracked-files
                magit-insert-stashes
                magit-insert-unstaged-changes
                magit-insert-staged-changes
                magit-insert-local-branches
                magit-insert-unpushed-to-pushremote
                magit-insert-unpushed-to-upstream-or-recent
                magit-insert-unpulled-from-pushremote
                magit-insert-unpulled-from-upstream))
                
(defun magit-try ()
  (interactive)
  (magit-stage-modified)
  (magit-commit)
  (magit-push))
  
;;;; JavaScript, TypeScript, and JSON settings

(let ((desired-indent-level 2))
  (setq-default
   typescript-indent-level desired-indent-level
   js-indent-level desired-indent-level))

(setq-default
 tide-node-executable
 (expand-file-name "~/.asdf/installs/nodejs/16.20.0/bin/node"))

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

;;;; Lyrics-downloading settings

(defun munge-song-or-band-name (string)
  "Prepare a STRING to be saved as a part of a filename."
  (replace-regexp-in-string
   (rx (or "<" ">" ":" "\"" "/" "\\" "|" "?" "*" " "))
   "-"
   string))

(setq lyrics-fetcher-format-file-name-method
      'munge-song-or-band-name)

(defun prefix (string)
  (interactive "MPrefix: ")
  (query-replace-regexp "^" string))

(defalias 'pos 'goto-char)

(defun rg-in-current-directory-qdw (regexp)
  (interactive "srg ")
  (let ((current-dir (if (derived-mode-p 'dired-mode)
                         dired-directory
                         (file-name-directory (buffer-file-name)))))
    (message regexp)
    (fd-grep-dired current-dir regexp)))
    


;;;; Global Editor MACroS settings

(keymap-global-set "RET" 'my-enter)
(keymap-set c-mode-map "RET" 'electric-newline-and-maybe-indent)
(keymap-set dired-mode-map "RET" 'dired-find-file-qdw)
(keymap-set org-mode-map "C-c RET" 'org-insert-subheading)

(keymap-global-set "C-c /"   'search-in-buffers)
(keymap-global-set "C-c \\"   'helm-swoop-back-to-last-point)
(keymap-global-unset "C-c .")
(keymap-global-set "C-c ."   'find-file-at-point-with-line-and-column-if-present)
(keymap-global-set "C-c !"   'async-shell-command)
(keymap-global-set "C-c %"   'query-replace-regexp)
(keymap-global-set "C-c :"   'eval-expression)
(keymap-global-set "C-h 1"   'describe-face)

(keymap-global-set "C-c C-f" 'multi-occur-in-matching-buffers)
(keymap-global-set "C-c C-s" 'isearch-forward-regexp)
(keymap-global-set "C-c c"   'comment-region)
(keymap-global-set "C-c f"   'origami-toggle-node)
(keymap-global-set "C-c g"   'rg-in-current-directory)
(keymap-global-set "C-c i"   'indent-region)
(keymap-global-set "C-c SPC" 'iterm2-here)
(keymap-global-set "C-c k"   'shell)
(keymap-global-set "C-c l"   'shell)
(keymap-global-set "C-c n"   'display-line-numbers-mode)
(keymap-global-set "C-c o"   'rotate-windows)
(keymap-global-set "C-c t"   'switch-theme) 
(keymap-global-set "C-c u"   'uncomment-region)
(keymap-global-set "C-c v"   'vscode)
(keymap-global-set "M-!" 'login-shell-command)
(keymap-global-set "M-(" 'eval-expression)
(keymap-global-set "M-5"     'query-replace-regexp)
(keymap-global-set "M-o"     'other-window-reverse)

;;;; c-mode Editor MACroS settings

(keymap-set c-mode-map "C-c {" 'beginning-of-defun)
(keymap-set c-mode-map "C-c }" 'end-of-defun)

;;;; help-command Editor MACroS settings

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;;; org-mode Editor MACroS settings

(keymap-set org-mode-map "C-c RET" 'org-open-at-point)
(keymap-set org-mode-map "C-c >"
                          'org-insert-subheading-respect-content-qdw)
(keymap-set org-mode-map "C-c [" 'org-table-move-column-left)
(keymap-set org-mode-map "C-c ]" 'org-table-move-column-right)
(keymap-set org-mode-map "C-c s" 'org-insert-slack-link)
(keymap-set org-mode-map "C-c j" 'org-insert-jira-link-qdw)
(keymap-set org-mode-map "C-c (" 'org-promote-subtree)
(keymap-set org-mode-map "C-c )" 'org-demote-subtree)
(keymap-set org-mode-map "C-c [" 'org-table-move-column-left)
(keymap-set org-mode-map "C-c ]" 'org-table-move-column-right)
(keymap-set org-mode-map "C-c s" 'org-insert-slack-link)

;;;; shell-mode Editor MACroS settings
(keymap-unset shell-mode-map "C-c .")
(keymap-set shell-mode-map "C-c ." 'find-file-at-point-with-line-and-column-if-present)
(keymap-global-set "C-c s" 'shell-in-dir)

;;;; Binary-file-viewing settings

(defun use-hexl-mode-if-binary-p ()
  "If the current buffer is a binary file, switch to `hexl-mode'."
    (when (string-match-p "\0" (buffer-string))
        (hexl-mode)))
       
(add-hook 'find-file-hook #'use-hexl-mode-if-binary-p)

;;;; Usenet settings

(setq gnus-select-method '(nntp "news.eternal-september.org")
      gnus-secondary-select-methods '((nntp "news.sonic.net"))
      gnus-newsgroup-maximum-articles nil
      gnus-thread-hide-subtree 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                             ;;;;
;;;; Experimental area                                           ;;;;
;;;;                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; Totally broken
;; 
;; python-mode doesn't yet use the new *-mode-map pattern, so...
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "RET") 'python-auto-indent-qdw)
;;   ;; Run the Blacken real-time Python code auto-formatter,
;;   ;; except on third-party code.
;;   (unless (string-match
;;            (expand-file-name "~/src-3")
;;            buffer-file-name)
;;     (blacken-mode)))

;; ;; sketch
;; (defun run-applescript (lines)
;;   (async-shell-command
;;     (format "osascript -e '%s'"
;;             (shell-quote-argument string))))

;;;; Settings for displaying the list of currently open buffers; see https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html

;; (defun display-buffer-in-new-window-qdw (buffer)
;;   "Display BUFFER in a new window."
;;   (let ((window (split-window-below)))
;;     (set-window-buffer window buffer)
;;     window))
;;
;; (defun list-buffers-qdw (&optional arg)
;;   "Display a list of names of existing buffers.
;;    The list is displayed in a buffer named `*Buffer List*'.
;;    Note that buffers with names starting with spaces are omitted.
;;    Non-null optional arg means show buffer `*Buffer List*',
;;    even if it is empty. See also `buffer-menu'."
;;   (interactive "P")
;;   (let ((
;;          display-buffer-overriding-action
;;          ('display-buffer . display-buffer-in-new-window-qdw)
;;        ))
;;     (list-buffers arg)))
;;

;; Prominently highlight any tabs used for indentation in source code,
;; (but not in the PostgreSQL source code or in Go, because
;; tabs are the convention there).
;;
;; (set-face-attribute 'whitespace-tab nil
;;                     :background red
;;                     :foreground black)                    
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                              ;;;;
;;;; Don't change stuff below this comment; it's programmatically ;;;;
;;;; maintained by the Customize system.                          ;;;;
;;;;                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; customize faces

(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-odd-diff-B ((t (:extend t :background "#0D2A34" :distant-foreground "#869496"))))
 '(flycheck-error ((t (:weight normal))))
 '(italic ((t (:underline t :slant normal :weight normal))))
 '(org-date ((t (:foreground "#6495ed" :weight normal))))
 '(sh-quoted-exec ((t (:foreground "#f5b5ff" :weight normal)))))

;;;; customize other settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'new-buffer)
 '(custom-enabled-themes '(mariana))
 '(custom-safe-themes
   '("1c97a02acc274c4fcd95508fabaf7c0c63ad67e3e6ef1842ba694c740965e854" "e47df11e6d51b79e2e36823fd188e5b485930077a52df3285392cdaa4020f0c9" "f63aa09117f4df8855bb7bf38a49fa11cf0936f2fcfb64f923cfd828e114ab08" "85dc1406bb51ef440b56cde64d8eb33d47d6302cb339b2cdab1f997e3123b992" "0bdcd500d4360751824f33090f8cffd5828d38a6abc01bf4e42a7006cb6beb5c" "b9efba60244721cc1f2ad7c576f09136d2016cd0436a32e7a581331644e29fce" "8f281cfe198d9b9f954885793bd76960b920d4d6e3018fbb6e373938024cd34f" "5cdfad2a7cedfdc084b2b544bb0447bcfc4cd1bfe8f0c0735674c8dbad0e717c" "6c1ad5cc5017676e109163a955bebdbf3cc201858eb153fd56aeba33ec52b68e" "5ffe5be170a405b04e403d157630a6c9d292b5d58a1e9d7366c4c4118df423aa" "71296b6a2d13efb96e131c1643d84eca23075098d843a2ae51e3fa7f899091a5" "9e72d49cdd4e9534aa2411ee90b17def8e3a600c023691bb2fd4caa9b60e0e59" "a90c4c4188050ad506e82dcb6c7db564f7b4e386dbdae86d8f4508b530193a0c" "a8faa272156975eb3fa10425688739df436694603d028ec174aff859ec24cba3" "3af3dd88dea749cf2a0e131184ed75db59f66b904e85c525895bbed025330421" "dc941924c343007e6f3fa3eb716ae9b3ed76c6bdd9e6cf0614aee4a03a4adc32" "e7487e37246f7b70ae36b59023e7e6460d2b5df4f13184de8d1dee2b9d3b0ef4" "53be810e291161e33ace3015935f560ebf4a6d0737eb2d6af1a7ca796ce05e74" "1bc996dc2dc10c530bcbfa1bc6af987d68acd4a2f69f1e041444e61a42920a13" "7e9d636453ff18f1032d6e926c2777b684360fc6fd896a276402afefa403876a" "0d26cb1d6d92f5f1c9c2d2167915b8f5a21b244334a6d7ed0e785742b8349ac8" "3ff156dcf0c581089a0de0c22c6d30f07f104008abe569e12a1f97d51a38d5ef" "c5890693563e2a0be4bf6e2f3045a9b7bd8c057bfc8d9a8a9f7118e5e87634e3" "d3b10ba17cb48c09cd15058192a9a62012b4ca255ad04f15465ebc6851dbc457" "7cc12de7fca1f48ce8f3d85f71fc71adb3b8f7ef248d0cdba44b94e067d93cf8" "56df7d8343ba86c443f18ec0192a495753de3afea4622a6ac920f4b7e31256e9" "6dd8d36d24895e21527fda4f951054450e21e2d7367ea1f12c523decf600c66d" "9e8d46e15c6c3deb4fcb17bc47e300c0899913289ae770e50eb32aa4aa65336a" "82caf1187ff6e99aa59a94e8e449a6fc7a396714b768436444cb7809040a1efe" "21334be100120f874d9a1105d3c492da0bc2002aa6ea8f2bdd70a476b1a73ea1" "004d4c8d94fb22fe26ee805e7666e86dea6ac772ed83cb63108327918f4f1d05" "3e4f3a3f4dc285e9499d92d96170703b4cc7b1e7241f3dde9d37b1d6e7acf175" "4f2c625c7cbd7c2716d5718a82afb7a1767521eba8f71ed436e16523638f5c9c" "65facdd8128d5e4da12424b373b8ad10403c64316f07c97fd3ba27f1620250c2" "dd421f76ead86e704fce0d9e6586b1815263f521f1b409d4add7533a8eb16a3e" "e1f0530e6d48b92de6bfb6690212b616da78975e0c22f490bf799e4b9cec1837" "febc930c89f9b6f5f2e206e8d4ae7dbd85f3c0f5ff86ff55f743515d9d062c4f" "beda5f79b80bbd70ef600e852a33c4eb50d897b22d51bdecf04b83c33928c77c" "6b292056832c15d659dd715361e4bfa3e6d624f43cc2106af3555a523776c8d9" "97b889a8d50c8df11cb0343332dbfc99c18286f40651e0c3e4e69d79dad67807" "f5a7e07642decb17b03483af7c44e93353d2b128de403bf301651954c628c0ab" "f7f63c4f14c5eb79b6c26c308e6ad625ff91c649f39e57f198f1fb13ba2eb9a2" "f4833b553e92b4668afac5f351e68e14a0f90678bacfaf21bd4a19e1e4adc665" "39bcbc8fd33b1932683c3b05aab9ac9c59beec7f7fc211f6db775e9cad1702da" "291708e76bbdc185db5d3f154cb58a547ffdb2ab88b9ca68c8b67fd328ffaf60" "8e059e42d34e37de1c9d4134b89d4a41d11bf44ca88b70a738a8b2fa1d030900" "1843cbc8dd7232022cfcc54218864563000192e06188196c654443832c328047" "79a750a591511be62a78fcb2c3de2f422da7c4cd35e61571bc7cd8548e302a28" "5ccf92da2a1298b7c6fd8aac0e8641b8a0acd079d48df9718009feda3b0cdee8" "258f232c779de08cc48f0504805ba7f6f73bf4e380cd8c06541fab25c2686f89" "d0f9d3798ca38427957e3366ad49545c6f2748f0c838e615e3f24fa69e28b05e" "c1e8a0552f2019fd3e94dc38f50a2b89bed4cde151b5e3a944741003c76628e4" "51db76da37f3d91d9e910bde0fef9068d1b41a9adc1962c348b26eabc3cfe972" "0e21b836943cd76aaac05a5a18ffbae5aee1db741e56b8cc52d05ab88157f930" "085fa2189b97d6ce38d33b5a4854d0beaedc777834c48bb75e597151b7345ad2" "2cb34817f716ebaf37ab3f13e2e878177d6bb7182984d4126b356453f5d2e00c" "f34e811a660f850e88b399d072b106d44c15bfe0480045770b11fa7cc48d45d0" "a7a3dbf87e468bc7043d5d3b3b53a970ab3f98782aeafed6ad41b9cffd762543" "0338ac530ffe4fa47cb9d56548419bdfd2ec8c075dcf8d641425c2d847bf9ed6" "ec1992a1a643d37c880a7492922de4093cf9a5ebfe83dd4b68f9cc2e38497875" "fd0840d2cb2adf126ecd57d36cb7238900606099c9307537be0de9e6bac24e84" "fe5091a6867f070c7e9d70aaa30d05c105f3812ab451ff8d9d1197c44e7bd28d" "a67b6cb65db241e033b6aed5eeaf0805a1b62e598cedc605c71d003a1d5c00c6" "6ec75322239539171cefbce7e47c30627e6ba5e92d6dadfae119df94175e4293" "9ac8bcd9d4d22bbd087bbf9fb2db592a6f622a0774959e62a7fabb9b732ae347" "7d02d3cd33991c55773076e9f51caeb54efa1ca94bc140f431d446d175a0850b" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "e989d10979a9bd3230077945a8169cf6988cbc7ca7ca95fc7dfe373d573eea85" "b099685963c251fb3ed5dd60237bda3390a11372221dfc9d168aa54e7b1c252e" "80d5a22931c15756b00fb258b80c93b8bc5096bb698dadfb6155ef3550e1c8fb" default))
 '(eldoc-idle-delay 0.15)
 '(elpher-default-url-type "gemini")
 '(explicit-bash-args '("--noediting" "-i" "-l"))
 '(markdown-italic-underscore t)
 '(mode-line-position-column-format '(" %C"))
 '(mode-line-position-column-line-format '(" (%l,%C)"))
 '(neo-cwd-line-style 'text)
 '(neo-theme 'classic)
 '(ns-alternate-modifier 'none)
 '(ns-right-alternate-modifier 'meta)
 '(org-agenda-files '("/Users/quinn.weaver/main.org"))
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t)))
 '(org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("`" org-code verbatim)
     ("~"
      (:strike-through t))))
 '(osx-trash-command '("trash" "-F"))
 '(package-selected-packages
   '(wgrep macrostep-geiser geiser-chibi ac-geiser ac-slime geiser geiser-chez glue slime slime-repl-ansi-color helm helm-swoop elisp-slime-nav tt-mode elfeed elquery elpy elpygen ack devil yaml-mode symbol-overlay multiple-cursors paredit-everywhere lyrics-fetcher tron-legacy-theme jiralib2 osx-plist magithub fd-dired counsel-fd osx-trash org logview outshine paredit butler git-modes lsp-origami origami origami-predef xterm-color flymake flymake-yamllint yaml-tomato yaml-pro gptai ivy fish-mode blacken titlecase company-lua flymake-lua lua-mode evil go-mode jenkinsfile-mode applescript-mode w3m ## vimrc-mode org-bullets magit-delta dired-sidebar magit dockerfile-mode dumb-jump terraform-mode company tide elpher json-mode markdown-mode typescript-mode ox-jira))
 '(safe-local-variable-values '((eval display-ansi-colors)))
 '(sh-indent-statement-after-and nil)
 '(shell-prompt-pattern "\"[[~].*$ "))
 
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
