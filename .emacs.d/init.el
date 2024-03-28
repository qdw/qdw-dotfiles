(load (expand-file-name "~/.emacs.d/secrets.el") 't)

(require 'ansi-color)
(require 'blacken)
(require 'dirtrack)
(require 'dockerfile-mode)
(require 'json)
(require 'magit)
(require 'menu-bar)   
(require 'org)
(require 'osx-trash)
(require 'package)
(require 'ruby-mode)
(require 'scroll-bar)
(require 'sh-script)
(require 'tool-bar)

;;;; Color settings

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))
(load-theme 'qdw-subatomic256-fork 't)
(defalias 'colors 'list-colors-display)

;;;; dired settings

(defun system-move-file-to-trash (path)
  "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.

Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
  (shell-command (concat "trash -vF \"" path "\""
                         "| sed -e 's/^/Trashed: /'")
                 nil ;; Name of output buffer
                 "*Trash Error Buffer*"))
                 
;; (defun trash (path)
;;   (interactive "f")
;;   (system-move-file-to-trash path))

;; Settings (incl. dired settings) to open files in external apps

(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if @fname is given, open that.
URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;; Xah Lee rides to the rescue again, this time with a hex -> dec RGB converter
(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2 )
    (save-excursion
      (search-backward-regexp "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (search-forward-regexp "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq inputStr (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tempStr (replace-regexp-in-string "^0x" "" inputStr )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "^#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "^#" "" tempStr ))  ; CSS …
      )

    (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
    ))


;;;; Alternative-editor settings
;;;; FIXME: abstract this repetitive code

;; FIXME: abstract this repetitive code
(defun sublime ()
  (interactive)
  (save-window-excursion
    (async-shell-command
     (format
      "/opt/homebrew/bin/subl --background %S:%d"
      (shell-quote-argument buffer-file-name)
      (+ (if (bolp) 1 0) (count-lines 1 (point)))))))

(defun gvim ()
  (interactive)
  (save-window-excursion
    (async-shell-command
     (format
      ;; "gvim %S +%d"
      "open -g -a MacVim.app '%S +1'"
      (shell-quote-argument buffer-file-name)
      (+ (if (bolp) 1 0) (count-lines 1 (point)))))))

(defun vscode ()
  (interactive)
  (save-window-excursion
    (async-shell-command
     (format "%S %S %d"
             (expand-file-name "~/.emacs.d/open-file-in-macos-vscode.scpt")
             (shell-quote-argument buffer-file-name)
             (+ (if (bolp) 1 0) (count-lines 1 (point)))))))


;; (if (getenv "TMUX")
    

;;;; Alternative color theme settings
        
;;; Dark themes

;;T  Selenized: medium-contrast white-on-green 256-color w/ bright font-lock
;; (oad-theme 'solarized-selenized-dark 't)

;; Less-favored themes

;;T  Niflheim: high-contrast white-on-brown 256-color theme w/ muted font-lock
;; (oad-theme 'niflheim 't)

;;T  birds-of-paradise-plus: very loud red theme for use on production boxes
;; (oad-theme 'birds-of-paradise-plus 't) ;; RED, loud, for prod box

;;T  base16-greenscreen: green-phosphor nostalgia theme
;; (oad-theme 'base16-greenscreen 't)

;;T  Quinnokai: medium-contrast, dark >256-color theme. No good for terminals.
;;   This is is my very slightly tweaked version of the third-party theme
;; (oad-theme 'quinnokai 't)

;;; Light themes

;; (add-to-list 'load-path
;;              (expand-file-name "~/.emacs.d/themes"))
;; (oad-theme 'almost-mono-cream 't)

;; No matter what theme I chose above,
;; make the cursor orange.
(set-cursor-color "DarkOrange1")
;; FIXME: do it this way instead? "You can also set the text cursor
;; color, for all frames, by customizing the ‘cursor’ face."

;;;; Other color settings

;; Interpret ANSI escape codes in this buffer,
;; coloring the buffer text accordingly.
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
        
;;;; GUI Emacs settings

(defvar current-favorite-font "SF Mono-18")
;; (defvar current-favorite-font "Hack-14-Regular")
;; (defvar current-favorite-font "JetBrains Mono-14")
;; (defvar current-favorite-font "Fira Code-16")

(add-hook 'after-make-frame-functions

          ;; FIXME: figure out how to make this function run for the very first frame created when I launch Emacs
          (lambda (frame)
            (if (display-graphic-p)              
                
                ;; If running in a GUI frame, set the font explicitly
                ;; (unnecessary when running in a terminal)
                (set-face-font 'default current-favorite-font frame)
                
                ;; If running in a terminal-based frame,
                ;; enable terminal mouse integration.
                (xterm-mouse-mode 1)))
	  99)
;;       ;; ;; I often mistake Emacs windows for iTerm2 windows,
;;       ;; ;; press Command-N, and then get annoyed when I get another Emacs window.
;;       ;; ;; So just remap Command-N to open an iTerm2 window! FIXME: broken
;;       ;; (global-set-key (kbd "s-n")
;;       ;;   (lamdba () nil)))
  
;;;; macOS settings

(when (string= system-type "darwin")

  ;; In macOS, the bundled BSD ls doesn't play well with Emacs,
  ;; so use Homebrewed GNU ls instead
  (dolist (gnu-ls '("/opt/homebrew/bin/gls" "/usr/local/bin/gls"))
    (when (file-exists-p gnu-ls)
      (setq insert-directory-program gnu-ls
            dired-use-ls-dired t
            dired-listing-switches "-aBhl --group-directories-first")))
            
  ;; When I delete files in dired, send them to the macOS Trash
  (osx-trash-setup)
  (setq-default delete-by-moving-to-trash 't))

;;;; Human-language settings (including English)

;; From xahlee.org
(defun unfill-paragraph ()
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

;;;; eBook settings

(add-to-list
 'auto-mode-alist
 '("\\.epub\\'" . (lambda ()
                    (add-to-list 'load-path (expand-file-name "~/src-3/nov.el") nil)
                    (load-file (expand-file-name "~/src-3/nov.el/nov.el"))
                    (nov-mode))))

  

;;;; Shell settings

(when
  (getenv "HOMEBREW_BIN")
  (setq explicit-shell-file-name (concat (getenv "HOMEBREW_BIN") "/bash")))    

;; When running a quick shell command via 'M-!', make it source my
;; ~/.bashrc
;; (setq shell-command-switch "-ic")

;; Don't display the STDOUT or STDERR of quick shell commands
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Don't prompt me about shell sessions, debug sessions, et cetera
;; when I quit emacs. Just kill them.
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer)) nil)))
  
(defun ts()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;;;; Settings for the Emacs package system
  
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)
;; (package-install-selected-packages)


;;;; Basic comfort

(setq-default
 ;;;; Settings for use with macOS
 read-file-name-completion-ignore-case nil ;; Consider filenames case-sensitive

 prefer-coding-system 'utf-8         ;; Iff file contains non-ASCII chars
 large-file-warning-threshold 100000000 ;; Warn when opening large files
 require-final-newline 't            ;; Make, e.g., 'wc -l' work right

 ;; Natural-language settings
 auto-fill-mode nil                  ;; Don't automatically hard-break lines
 word-wrap nil
 colon-double-space nil              ;; Single-space: always
 sentence-end-double-space nil       ;; Single-space. Always
 delete-selection-mode +1            ;; Overwrite selection when I type

 ;;;; Auto-indentation settings
 indent-tabs-mode nil                ;; Indent using spaces, not tabs
 cperl-indent-level 4
 elisp-indent-level 2
 org-indent-indentation-per-level 4
 typescript-indent-level 2

 calendar-week-start-day 1           ;; First day of the week = Monday
 isearch-allow-scroll 'unlimited
 transient-mark-mode 't              ;; Operate on highlighted region
 make-backup-files nil               ;; Don't leave myfilename~ droppings
 vc-follow-symlinks 't               ;; Follow links to git files w/o asking
 cursor-type 'box                    ;; Box cursor (line is too hard to see)
 column-number-mode nil         ;; Turn off col num display (distracting)
 global-font-lock-mode 't            ;; Always do maximal syntax highlighting

 inhibit-startup-screen 't           ;; No GNU spam.
 visible-bell 't                     ;; Instead of beeping, flash screen
 display-fill-column-indicator-column 80
 
 ;; Something is setting isearch to be case-sensitive. Undo that.
 case-fold-search 't
 )
 
;; Exception to the usual rule of always using spaces for indentation:
;; .gitconfig files require tabs and break if you spaces.
;;
;; FIXME: doesn't work, for some reason.
;; (add-hook 'conf-mode-hook
;;           (lambda ()
;;             (when (eq (buffer-name) ".gitconfig")
;;               (setq indent-tabs-mode 't
;;            99))))

;; Customized visual bell
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
(tool-bar-mode -1)   ;; Turn off toolbar.
(menu-bar-mode -1)   ;; Turn off menubar.
(scroll-bar-mode -1) ;; Turn off scrollbar.
(blink-cursor-mode 0) ;; Use a solid cursor (blinking is too distracting).

;;;; Settings for ecb, an emacs sidebar showing directory trees, compilation,
;;;; lists of functions defined in the current file, and more

;; (require 'ecb)
;; (global-unset-key (kbd "M-e"))
;; (global-set-key (kbd "M-e") 'ecb-show-layout-help)

;;;; Other editors


;; (add-hook 'find-file-hook
;;           (open-in-sublime-text)
;;           (open-in-gvim)
;;           (open-in-vscode))
                                
;;   (add-hook 'find-file-hook
;;             (lambda ()
;;               (letrec dirname (file-name-directory (buffer-file-name current-buffer))
;;                       basename (file-name-nondirectory (buffer-file-name current-buffer))
;;                       path (concat dirname basename))
;;               (when (file-exists-p path)
;;                 (open-in-sublime-text)
;;                 (open-in-gvim)
;;                 (open-in-vscode))))

;;;; org-mode settings

;; Hide the PROPERTIES section if folded;
;; pasted from https://stackoverflow.com/a/17492723
;; Open *.org_archive files in read-only mode;
;; the record of completed items should be immutable.
(defun qdw-org-archive-read-only-hook ()
  (when (string-match "[.]org_archive" buffer-file-name)
        (read-only-mode)))
(add-hook 'find-file-hook 'qdw-org-archive-read-only-hook)

(setq-default ;; for yet more org-mode settings, see ~/.emacs.d/site-lisp.el
  org-startup-folded 't
  org-cycle-separator-lines -2
  org-return-follows-link 't 
  org-agenda-files (list (expand-file-name "~/main.org"))
  org-todo-keywords
  '((sequence
     "TODO(t)"
     "QUESTION(q)"
     "NEXT_ACTION(n)"
     "AWAITING_RESPONSE(a)"
     "IN_PROGRESS(i)"
     "GET_HELP(h)"
     "INFO(f)"
     "PROJECT(r)"
     
     "|"
  
     "DONE(d)"
     "POSTPONED(p)"
     "WONTFIX(w)"
     "JIRAFIED"
     ))

  ;; Always use colors compatible with TERM=xterm-256colors,
  ;; and always specify them as hex.
  ;; 
  ;; You can browse colors and copy their hex values here:
  ;; https://www.ditig.com/256-colors-cheat-sheet

  ;; FIXME: assign hex codes to color names,
  ;; either here using (let ...
  ;; or in the subatomic-256.el theme file.
  org-todo-keyword-faces
  '(("GET_HELP" . "#ff00af")
    ("NEXT_ACTION" . "#ff00af")
    ("AWAITING_RESPONSE" . "#ff00af")
    ("QUESTION" . "#ffff00")
    ("POSTPONED" . "#ffff00")
    ("INFO" . "#ffff00")
    ("PROJECT" . "#00afff")
    ("IN_PROGRESS" . "color-46")))

(add-to-list 'org-file-apps
             (cons "png" "open %s"))

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

;; Damn, GPT is good at writing code to English-language spec
(defun org-fold ()
  "Set the VISIBILITY property of the element at point to 'folded' in org-mode."
  (interactive)
  (when (org-at-heading-p)
    (org-set-property "VISIBILITY" "folded")))

;; Another gem from org-mode. Prompt:
;; Please write me an interactive Emacs function that starts a
;; minibuffer dialog like the "Find file: " dialog, but with the label
;; "Link to file." The function should handle tab completion the same
;; way that find-file does, including tilde expansion. After you
;; choose a filename and hit RET, the function should prompt you for
;; the visible link text, supplying the basename of the file as a
;; default option if you just hit RET without specifying a label.
(defun org-insert-file-link ()
  "Insert a link to a local file."
  (interactive)
  (let* ((filename (read-file-name "Link to file: "))
         (link-text (read-string "Link text: " (file-name-nondirectory filename))))
    (insert (format "[[file:%s][%s]]" filename link-text))))

(defun org-insert-slack-link (channel-name)
  (interactive "MSlack channel (without the '#'): ")
  (org-insert-link nil
                   (concat "https://" slack-team ".slack.com/channels/#" channel-name)
                   (concat "#" channel-name)))            

(defun org-insert-jira-link (ticket-name)
  (interactive "MTicket name (e.g. DATAPLAT-000): ")
  (org-insert-link nil
                   (concat jiralib-url "/browse/" ticket-name)
                   ticket-name))

(defun org-insert-pr-link (repo pr-number)
  (interactive "MRepo: \nnPR number: ")
  (org-insert-link nil
                   (format "https://github.com/%s/%s/pull/%s"
                           getenv("GITHUB_ORG")
                           repo
                           pr-number)))

(defun org-insert-subheading-respect-content (arg)
  (interactive "P")
  (org-insert-todo-subheading-respect-content arg)
    (cond
      ((org-at-heading-p) (org-do-demote))
      ((org-at-item-p) (org-indent-item))))

(add-hook 'org-mode-hook
          (lambda ()
            (keymap-set org-mode-map "C-c s" 'org-insert-slack-link)
            (keymap-set org-mode-map "C-c j" 'org-insert-jira-link)
            (keymap-set org-mode-map "C-c (" 'org-promote-subtree)
            (keymap-set org-mode-map "C-c )" 'org-demote-subtree)
            (keymap-set org-mode-map "C-c [" 'org-table-move-column-left)
            (keymap-set org-mode-map "C-c ]" 'org-table-move-column-right)
            
            ))

;;;; Help

(define-key 'help-command (kbd "C-l") 'find-library)  ;; Look up Emacs Lisp lib.
(define-key 'help-command (kbd "C-f") 'find-function) ;; Look up Emacs func src.
(define-key 'help-command (kbd "C-k") 'find-function-on-key) ;; " " macro src.
(define-key 'help-command (kbd "C-v") 'find-variable) ;; " " variable definition

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
             
(global-set-key (kbd "C-c C-o") 'rotate-windows)

(defun other-window-reverse ()
  (interactive)
  (other-window -1))
  

;; Editor macros

(global-set-key (kbd "C-(") 'eval-expression)
(global-set-key (kbd "C-c !") 'async-shell-command)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "M-5")  'query-replace-regexp)
(global-set-key (kbd "C-c C-l") 'display-line-numbers-mode)
(global-set-key (kbd "C-h 1") 'describe-face)
(global-set-key (kbd "C-c C-f") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "M-o") 'other-window)

;;;; TRAMP: edit remote files as if they were local.

;; TRAMP into Docker containers
;; (code from https://www.emacswiki.org/emacs/TrampAndDocker):
;; (defadvice tramp-completion-handle-file-name-all-completions
;;   (around dotemacs-completion-docker activate)
;;   "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
;;     a list of active Docker container names, followed by colons."
;;   (if (equal (ad-get-arg 1) "/docker:")
;;       (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
;;              (dockernames (cl-remove-if-not
;;                            #'(lambda (dockerline) (string-match ":$" dockerline))
;;                            (split-string dockernames-raw "\n"))))
;;         (setq ad-return-value dockernames))
;;     ad-do-it))

;; (push
;;  (cons
;;   "docker"
;;   '((tramp-login-program "docker")
;;     (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
;;     (tramp-remote-shell "/bin/sh")
;;     (tramp-remote-shell-args ("-i") ("-c"))))
;; tramp-methods)

;;;; Directories and directory trees

;; dired is emacs's built-in mode for viewing and changing
;; the contents of a directory, including subdirs.
;; 
;; By default, when you press return while the point is on a subdir,
;; it will open that subdir in a separate dired buffer.
;; I don't want that, since it leads to a lot of unneeded buffers
;; when I'm just navigating around.
;;
;; So I'm changing RET to open the subdir in the *current* buffer
;; instead:
;; (add-to-hook 'dired-mode-hook
;;              (lambda ()
;;                (if 
;;                (set-key (kbd RET) 
;;                         'dired-find-alternate-file

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

(defun qdw-open-current-line-in-github-web-ui ()
  (interactive)
  (letrec ((filename (file-name-nondirectory (buffer-file-name)))
           (line (number-to-string (line-number-at-pos))))
           ;; (src-dir-regexp (concat home "/src.*/")
           ;; (truncated-filename (replace-regexp-in-string src-dir-regexp "" filename 't 't))
    ;; (print (concat filename ":" line))))
    (async-shell-command (concat "gh browse " filename ":" line)
                         "emacs-gh-stdout"
                         "emacs-gh-stderr")))


;; If I have a Homebrewed git installed, use that instead of /usr/bin/git
(dolist (git-executable '("/opt/homebrew/bin/git" "/usr/local/bin/git"))
  (when (file-exists-p git-executable)
    (setq-default magit-git-executable git-executable)))

;;; diff and merge settings

;; When invoking commands, use a new pane to the right of the current pane
;; (don't create a new frame).
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; File-handling settings 

;; Automatically make file executable if it starts with a shebang (#!).
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Globally enable dumb-jump, an it-just-works way to jump from function invocations
;; to the actual definitions of those functions, supporting many languages.
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(defun my-enter ()
  "Inserts a newline character then indents the new line just
   like the previous line"
  (interactive)
  (newline)
  (unless (string-equal major-mode "mhtml-mode") (indent-relative-maybe)))

(global-set-key "\r" 'my-enter)

;; (setq-default blacken-skip-string-normalization 't)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\r" 'electric-newline-and-maybe-indent)
            ;; Run the Blacken real-time Python code auto-formatter,
            ;; except on third-party code.
            (unless (string-match (expand-file-name "~/src-3") buffer-file-name)
                    (blacken-mode))))

;;;; Generic coding settings (not language-specific)

;;; Code-folding
(require 'origami)
(add-hook 'origami-mode-hook
          (lambda ()
            (define-key origami-mode-map
                        (kbd "C-c C-f")
                        'origami-toggle-node)))
(global-origami-mode)

;;;; Bash-(and-other)-shell-script-editing settings

;; Don't auto-insert '<< EOF'-style heredocs when I type '<<',
;; since I have my own preferred way of writing heredocs.
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-electric-here-document-mode nil)))

;;;; C settings
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key
             "\r"
             'electric-newline-and-maybe-indent)))

;;;; Lisp settings FIXME: use slime.el instead
(setq lisp-indent-function 'common-lisp-indent-function)
(add-hook 'lisp-mode-hook (lambda ()
                            (eldoc-mode +1)))

;; When I run C-(
;; (which lets me write and eval a quick Lisp expression
;; in the minibuffer),
;; add the opening parenthesis for me.
(add-hook 'eval-expression-minibuffer-setup-hook
	  (lambda ()
	    (insert "(")))
            
(global-set-key (kbd "C-c :") 'eval-expression)

;;;; JSON settings

(setq-default json-encoding-default-indentation "    ")

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

;;;; TypeScript settings

(setq-default tide-node-executable (expand-file-name "~/.asdf/shims/node"))

;; eslint (JavaScript/TypeScript linter) settings
(defun seteslintrc ()
  "Returns the location of nearest eslintrc"
  (interactive)
    (setq flycheck-eslintrc (locate-dominating-file "~/" ".eslintrc.json")))

(defun configure-typescript ()
  (interactive)
  (company-mode +1)
  (eldoc-mode +1)
  (flycheck-mode +1)
  (column-number-mode +1)

  (seteslintrc)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        typescript-auto-indent-flag nil
        company-tooltip-align-annotations t)
  
  ;;; Seems like tide works for node only, not for deno :(
  (tide-setup)
  (tide-hl-identifier-mode +1))

(add-hook 'typescript-mode-hook 'configure-typescript)

;;;; customize'd settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'new-buffer)
 '(custom-safe-themes
   '("6ec75322239539171cefbce7e47c30627e6ba5e92d6dadfae119df94175e4293" "9ac8bcd9d4d22bbd087bbf9fb2db592a6f622a0774959e62a7fabb9b732ae347" "7d02d3cd33991c55773076e9f51caeb54efa1ca94bc140f431d446d175a0850b" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "e989d10979a9bd3230077945a8169cf6988cbc7ca7ca95fc7dfe373d573eea85" "b099685963c251fb3ed5dd60237bda3390a11372221dfc9d168aa54e7b1c252e" "80d5a22931c15756b00fb258b80c93b8bc5096bb698dadfb6155ef3550e1c8fb" default))
 '(ecb-layout-name "left13")
 '(ecb-options-version "2.50")
 '(ecb-tip-of-the-day nil)
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
   '(osx-trash org logview outshine paredit butler git-modes lsp-origami origami origami-predef xterm-color flymake flymake-yamllint yaml-tomato yaml-pro gptai ivy fish-mode blacken titlecase company-lua flymake-lua lua-mode evil esxml go-mode slack jenkinsfile-mode applescript-mode color-theme-sanityinc-solarized solarized-theme w3m ## vimrc-mode org-bullets magit-delta dired-sidebar magit dockerfile-mode dumb-jump yaml-mode terraform-mode company tide elpher json-mode markdown-mode typescript-mode ox-jira))
 '(sh-indent-statement-after-and nil))



;; FIXME: move these to my theme?
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-odd-diff-B ((t (:extend t :background "black" :distant-foreground "color-247"))))
 '(sh-quoted-exec ((t (:foreground "#f5b5ff")))))

 
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;; YAML settings

;; By default, yaml-mode doesn't color keys differently from scalar values;
;; they're both just variable-name-face (plain white text on black background,
;; in my theme).
;;
;; But I like keys to be highlighted for easy reading.
;;
;; Implement this by setting variable-name-face to use a special color,
;; but only when I'm in yaml-mode.
  
(add-hook 'yaml-mode-hook
          (lambda ()
            (face-spec-set 'font-lock-variable-name-face '((t (:foreground "#af5f00"))))))
            

;; Prevent all uses of bold typefaces
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal))
 (face-list))

;;;;;;;; Experimental area {{

;; (defun my-after-push-hook ()
;;   (message "Pushed!"))

(advice-add 'magit-push-current-to-upstream :after #'my-after-push-hook)

;; ;; Broken; no-op
;; (defun org-recursively-mark-as-done ()
;;   (interactive)
;;   (org-map-entries
;;     (let ((state (nth 2 (org-heading-components))))
;;       (when (cl-position state org-todo-keywords)
;;         (org-todo "DONE")))))


;; N.G.
;;
;; (defun my-cd (dir)
;;   "tell a running shell-mode session to cd to `dir'"
;;   (if (not dir)
;;       (error "No dir argument specified")
;;       (let ((shell-buffer (get-buffer "*shell*"))
;;             (shell-process (get-process "shell")))
;;         (if (and shell-buffer shell-process)
;;             (with-current-buffer shell-buffer
;;               (comint-send-input (concat "cd " dir)))
;;               (shell-process-cd dir)
;;             (error "my-cd failed somehow")))))
;; (my-cd "/Applications")


(defun prefix (string)
  (interactive "MPrefix: ")
  (query-replace-regexp "^" string))

;; Needs work

;; org-mode: let me specify colors for text;
;; these colors show up both when editing
;; and in the export.
;; (add-to-list 'load-path
;;              (expand-file-name "~/.emacs.d/3"))
;; (load "org-colored-text" nil 't)

;; Needs work

;; (defun gpt (prompt)
;;   "Send a prompt to OpenAI *using my shell script*, and print its response in a separate buffer"
;;   (interactive "MGPT prompt: ")

;;   (let ((output-buffer "*GPT output*"))
;;     (split-window-below)
;;     (other-window 1)
;;     (switch-to-buffer output-buffer 't)
;;     (other-window -1)
  
;;     (start-process "GPT"
;;                    output-buffer
;;                    "/Users/quinn.weaver/bin/ai"
;;                    prompt)))

;; Needs work

;; (defun bing (prompt)
;;   "Send a prompt to Bing Chat and return its response"
;;   (interactive "MBing prompt: ")
;;   (let ((output-buffer (get-buffer-create "*bing*"))
;;         (orig-point (point)))
;;     ;; (print (concat "bing" "'" (shell-quote-argument prompt) "'"))))
    
;;     ;; (set-process-sentinel
;;      (start-process "async-shell"
;;                     output-buffer
;;                     shell-file-name
;;                     shell-command-switch
;;                     "bing" "'" (shell-quote-argument prompt) "'")))
;;      ;; `(lambda (process signal)
;;      ;;    (when (= 0 (process-exit-status process))
;;      ;;      (with-current-buffer ,output-buffer
;;      ;;        (let ((output-text (buffer-string)))
;;      ;;          (with-current-buffer ,(current-buffer)
;;      ;;            (save-excursion
;;      ;;              (goto-char ,orig-point)
;;      ;;              (insert output-text))))))))))


;;;; Needs work
;;;;
;;;;
;; (add-hook
;;  'shell-mode-hook
;;  (lambda ()
;;    (shell-dirtrack-mode -1)
;;    (letrec
;;        (( sgs "\\(?:" )  ;; regexp code to start a "shy" (non-capturing) group
;;         ( cgs "\\("   )  ;; regexp code to start a capturing group
;;         ( ge "\\)"    )  ;; regexp code to end any group (shy or capturing)
        
;;         ( path-start-regexp
;;           (concat
;;            sgs "[^ ]*[ ]" ge "?"  ;; Optional: a single string terminated by space
;;            sgs ansi-color-control-seq-regexp ge ;; A single ANSI SGR color code for the path
;;            cgs ".*" ge "[$]" ;; The path itself, terminated by a literal dollar sign
;;            ;; ^ (use cgs to assign to regexp group 1)
;;            ) ))
   
;;      ;; Begin diagnostics
;;      (dirtrack-debug-mode +1)
;;      (setq-default dirtrack-debug-mode 1)

;;      (add-hook 'comint-preoutput-filter-functions
;;                (lambda (input)
;;                  (message (concat "input:  " input))
;;                  input)
;;                nil
;;                't)
;;      (add-hook 'comint-preoutput-filter-functions
;;                (lambda (input)
;;                  (let
;;                      ((output (dirtrack input))
;;                       (strmout (string-match path-start-regexp input))
;;                       (message (concat "dirtrack output: " output))
;;                       (message (concat "(string-match " path-start-regexp " " input ") returned "strmout))
;;                    output)))
;;                nil
;;                't)
;;      ;; End diagnostics   

;;      (setq dirtrack-list (list path-start-regexp 1))))
;;  nil
;;  't)

;; Jenkins API integration - disabled until I can figure out what auth method to use
;; For documentation on butler.el, see https://github.com/AshtonKem/Butler
;;
;; (add-to-list 'butler-server-list
;;              '(jenkins (getenv "JENKINS_HOSTNAME")
;;                        (getenv "JENKINS_ROOT_URL")
;;                       
;;                        FIXME: since my organization uses Okta with 2FA, I probably need to use a diffferent authentication mechanism - maybe I can make a bearer token?
;;                        (auth-file . "~/.jenkins.authinfo.gpg")))

;; Load Emacs settings specific to this organization
;; (may override any of the settings above)
(load (expand-file-name "~/.emacs.d/site-lisp.el") 't)
