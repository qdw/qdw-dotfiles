;;;; Suppress noisy warnings about packages that use deprecated idioms.
;;;; FIXME: somehow none of this works.

;; (defun ignore-all-warnings (type message &rest args)
;;   "Ignore all warnings."
;;     (ignore))
;; (advice-add 'display-warning :override #'ignore-all-warnings)
;; (setq warning-minimum-level :emergency)
;; (setq byte-compile-warnings nil)
;; (setq warning-minimum-level :error)

;;;; Hide window chrome

(setq inhibit-startup-screen 't)
(require 'tool-bar)
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

;;;; Typesettings: disable bold type

(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal))
 (face-list))

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
 
 ;;;; Emacs Lisp helper settings

(defun load-file-if-exists-qdw (file-name)
  "Load FILE-NAME if it exists, and do nothing otherwise."
  (condition-case
      nil (load-file (expand-file-name file-name user-emacs-directory))
      (error (message (concat "Error loading " file-name)))))
      
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

;;;; Some Lisp I wrote, or adapted, or adopted

(add-to-list 'load-path
             (expand-file-name "qdw-themes" user-emacs-directory))

(add-to-list 'load-path
             (expand-file-name "~/src/forks/mariana-theme"))

(load-file-if-exists-qdw "secrets.el")
(load-file-if-exists-qdw "site.el")

(set 'jiralib-url (getenv "JIRA_BASE_URL"))
(set 'jira-default-project-qdw (getenv "JIRA_DEFAULT_PROJECT"))
(defvar slack-team (getenv "SLACK_TEAM"))

;;;; Dependency settings

;; If I hack .el files but forget to recompile them to .elc,
;; then notice the newer .el file and read that instead.
(setq-default load-prefer-newer 't)

;; Don't show me warnings about deprecated Emacs Lisp idioms
;; (or about anything else).
(setq warning-minimum-level :error)

;; Also don't show me the not-a-warning about the deprecated
;; Common Lisp compat package, cl.el # FIXME: doesn't work
(advice-add
  'message
  :around
  (lambda (orig &rest args)
    (unless (and args
                 (stringp (car args))
                 (string-match-p "^Package cl is deprecated" (car args)))
      (apply orig args))))

;;;; Settings for the Emacs package system
  
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)
(package-install-selected-packages)

;; I'm running emacs as a daemon,
;; so just require all these modules at once;
;; don't get clever with lazy-loading.

(require 'ansi-color)
(require 'blacken)
(require 'comint)
(require 'compile)
(require 'dirtrack)
(require 'dockerfile-mode)
(require 'evil)
(require 'fd-dired)
(require 'flycheck)
(require 'json)
(require 'magit)
(require 'menu-bar)
(require 'mouse)
(require 'origami)
(require 'org)
(require 'osx-trash)
(require 'package)
(require 'paredit)
(require 'pandoc-mode)
;; (require 'per-buffer-theme)
(require 'recentf)
(require 'ruby-mode)
(require 'scroll-bar)
(require 'sh-script)
(require 'tide)
(require 'tool-bar)
(require 'typescript-mode)
(require 'whitespace)
(require 'yaml-mode)

;;;; Settings for the Emacs package system
  
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)
;; (package-install-selected-packages)

;; I'm running emacs as a daemon,
;; so just require all these modules at once;
;; don't get clever with lazy-loading.

;;;; Buffer-management functions

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

;;;; Edit this file

(defun ini ()
  (interactive)
  (find-file user-init-file))
  
;;;; Edit my ~/.bashrc

(defun edit-bashrc ()
  (interactive)
  (find-file (expand-file-name "~/.bashrc")))

;;;; Color-theme settings (plus site-specific settings)

;; Always use the same cursor color, no matter what theme I use.

(set-cursor-color "DarkOrange1")

;;;; Themes that I created or forked

(defvar qdw-themes '("amber" "amber-without-bold" "mariana" "tritium"))

(mapc
 (lambda (file-name)
         (add-to-list 'custom-theme-load-path (expand-file-name file-name)))
 '("~/.emacs.d/qdw-themes"
   "~/src/forks/mariana-theme"))

;;;; Load my current favorite theme

(setq-default custom-enabled-themes '())
(setq-default my-chosen-theme "tritium")
(load-theme (intern my-chosen-theme) 't)

;;;; Special colors for org-mode, overriding whatever theme I'm using

(defun overlay-org-colors-qdw () 
  (interactive)
  (setq-default
   org-catch-invisible-edits "show"
   org-todo-keywords '((sequence
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
                       
                       "|"

                       "DONE(d)"
                       "WONTFIX(w)"
                       )))

  (when (or (member 'tritium custom-enabled-themes)
            (member 'mariana custom-enabled-themes))
    (setq org-keyword-to-color-qdw
          `(("TODO" . ,bright-magenta)
            ("NEXT_ACTION" . ,bright-red)
            ("IN_PROGRESS" . ,bright-yellow)
            ("GET_HELP" . ,bright-red)
            ("AWAITING_RESPONSE" . ,yellow)
            ("QUESTION" . ,magenta)
            ("YAK_SHAVING" . ,yellow)

            ("DONE" . ,green)
            ("WONTFIX" . ,green)
            ("POSTPONED" . ,yellow)
            ("INFO" . ,yellow)
            ("PROJECT" . ,blue)
            ("SUBPROJECT" . ,bright-cyan)
            ))

    (setq org-todo-keyword-faces
          (mapc (lambda (pair) (add-to-list 'org-todo-keyword-faces pair))
                org-keyword-to-color-qdw))))

(overlay-org-colors-qdw)

;;;; Theme-related utility functions

(defun if-file-is-theme-reload-theme ()
  (let ((base-name (file-name-nondirectory (file-truename buffer-file-name))))
    (when (string-match "\\(.*\\)-theme\\.el$" base-name)
      (let ((theme-name (match-string 1 base-name)))
        (message "theme-name: %s; qdw-themes: %s" theme-name qdw-themes)
        (when (string-in theme-name qdw-themes)
          (message "theme name is in qdw-themes")
          (load-theme (intern theme-name) 't 't))))))

(add-hook 'after-save-hook #'if-file-is-theme-reload-theme)

(defun theme-insert-color (color-name)
  (when (boundp (intern color-name))
        (let* ((hex-string (symbol-value (intern color-name))))
          (insert (propertize hex-string
                              'face `(:foreground ,hex-string)))
          (insert (concat "  " color-name "\n")))))


(defun theme-test-pattern (&rest additional-colors)
  (interactive)
  (let*
      ((buffer
        (get-buffer-create "*theme colors*"))

       (basic-colors '("black"
                       "red"
                       "green"
                       "yellow"
                       "blue"
                       "magenta"
                       "cyan"
                       "white"))
                     
       (bright-colors
        (mapcar (lambda (c) (concat "bright-" c)) basic-colors))
      
       (all-colors
        (append basic-colors bright-colors additional-colors)))
                       
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (co all-colors) (theme-insert-color co))
      (display-buffer buffer))))

      
(defun idiosyncratic-test-pattern-qdw ()
  (interactive)
  (theme-test-pattern "extended-bright-white"
                        "extended-bright-black"
                        "extended-black"
                        "extended-bright-red"))


(defun theme-test-pattern-interleaved ()
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
       (variants '("extended"
                   "bright"
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
  (load-file
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
;;
;;; Light themes, for use outdoors
;; T  almost-mono: light theme, okay for using outdoors, I guess?
;; (load-theme 'almost-mono-cream 't)
;;
;;; Dark themes
;; T  base16-greenscreen: green-phosphor nostalgia theme
;; (load-theme 'base16-greenscreen 't)
;; T  tron-legacy: shades of blue and green text on a black background
;; (load-theme 'tron-legacy)

(defun switch-theme ()
  (interactive)
  (customize-variable 'custom-enabled-themes))


(defun toggle-theme ()
  "Toggle between my two favorite color themes, if applicable"
  (interactive)
  (letrec ((theme (car custom-enabled-themes))
           (other-theme (cond ((> (length custom-enabled-themes) 1)
                               nil)
        
                              ((eq theme 'tritium)
                               'mariana)
           
                              ((eq theme 'mariana)
                               'tritium)
           
                              ('t
                                nil))))
    (when (and (boundp 'other-theme) other-theme)
          (custom-set-variables '(custom-enabled-themes
                                  (list other-theme)))
          (overlay-org-colors-qdw))))
          

(defun load-theme-wrapper-qdw (orig-fun theme &optional no-confirm no-enable)
  ;; Disable all currently enabled themes
  (mapc 'disable-theme custom-enabled-themes)
  
  (prog1
    ;; Call the original load-theme
    (funcall orig-fun theme no-confirm no-enable)
    
    ;; Always use my custom org colors, regardless of theme
    (overlay-org-colors-qdw)))
                      
(advice-add 'load-theme :around #'load-theme-wrapper-qdw)


;; (defun theme-load-qdw (theme-name)
;;   "Load one of my themes, then overlay my org-mode colors"
;;   (custom-set-variable custom-enabled-themes '())
;;   (load-theme theme-name)
;;   (overlay-org-colors-qdw))

;;;; Third-party themes that I like okay:
;;
;;; Light mode:
;; Bundled with Emacs:
;; (load-theme 'modus-operandi) ;; max-contrast light theme
;; (load-theme 'modus-operandi-tinted)
;; (load-theme 'modus-vivendi)  ;; max-contrast dark theme (good in sunlight)

;; (load-theme 'hydandata)
;; (load-theme 'solo-jazz)
;; (load-theme 'infodoc)
;; (load-theme 'organic-green)

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
      
;;;; Buffer list settings

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) ;; "Name" column width
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))


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
          
(defun curse ()
  (interactive)
  (alt-editor
   "/usr/local/bin/cursor"
   "--goto" (file-line-column-colon-separated)))

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
(defun unfill-paragraph-xah ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region-xah (start end)
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
     
;;;; Shell-script-editing settings

(defun remove-annoying-sh-mode-map-bindings ()
  "C-c is supposed to be the prefix for user-defined keybindings, generally speaking, but shell-script-mode sets a bunch of C-c bindings that do dumb stuff like inserting syntactic constructs like case statements, for loops, and so on. I'm fine with typing those out like a normal person, so I don't want these features, and they're clobbering my personal keybindings. So unmap them."
  (interactive)
  (let ((c-c-bindings
         (list "C-c C-d"  ;; sh-cd-here             
               "C-c C-f"  ;; sh-for                 
               "C-c TAB"  ;; sh-if                  
               "C-c C-l"  ;; sh-indexed-loop        
               "C-c C-n"  ;; sh-send-line-or-region-
               "C-c C-o"  ;; sh-while-getopts       
               "C-c C-r"  ;; sh-repeat              
               "C-c C-s"  ;; sh-select              
               "C-c C-t"  ;; sh-tmp-file            
               "C-c C-u"  ;; sh-until               
               "C-c C-w"  ;; sh-while               
               "C-c C-x"  ;; executable-interpret   
               "C-c C-z"  ;; sh-show-shell          
               "C-c C-\\" ;; sh-backslash-region    
               "C-c ("    ;; sh-function            
               "C-c +"    ;; sh-add                 
               "C-c :"    ;; sh-set-shell
               "C-c <"    ;; }
               "C-c ="    ;; }}
               "C-c >"    ;; }}} indentation-related functions
               "C-c ?"    ;; }}
                          ;; }
               )))
    (dolist (binding c-c-bindings)
      (keymap-unset sh-mode-map binding))))
      
(remove-annoying-sh-mode-map-bindings)

;;;; Shell settings

(defun login-shell-command-qdw (command-string)
  "Run a shell command, sourcing my ~/.bashrc first, so that my many custom shell functions are available, as they would be in a terminal emulator session. This doesn't shell-quote your arguments, so if you need to do that, do that"
  (interactive "MShell command: ")
  (shell-command (concat "source ~/.bashrc && " command-string)))

(defun login-shell-command-and-show-output-qdw (command-string)
  "Like login-shell-command, but show standard output and standard error (each in its own new buffer)"
  (interactive "MShell command: ")
  (shell-command (concat "source ~/.bashrc && INSIDE_EMACS=true " command-string) "stdout" "stderr"))
  
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
    
;;;; Makefile settings
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode 't)))

;;;; org-mode settings

(setq-default
  org-agenda-restore-windows-after-quit 't
  org-agenda-files (list (expand-file-name "~/main.org")
                         (expand-file-name "~/personal.org"))
  org-cycle-separator-lines 1
  org-return-follows-link 't 
  org-startup-folded 't
  org-insert-heading '((heading . auto) (plain-list-item . auto)))

(defun org-insert-jira-link-qdw ()
  (interactive)
  (letrec (
           ;; (pre-filled-text (if (boundp 'jira-default-project-qdw)
           ;;                      (concat jira-default-project-qdw "-")
           ;;                      ""))
           (pre-filled-text "")                                
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
                     (concat jiralib-url "/browse/" issue-name)
                     issue-name)))
                     
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

;; Hide the PROPERTIES section if folded;
;; pasted from https://stackoverflow.com/a/17492723
;; FIXME: broken in Emacs 31
;; (defun org-cycle-hide-drawers (state)
;;   "Re-hide all drawers after a visibility state change."
;;   (when (and (derived-mode-p 'org-mode)
;;              (not (memq state '(overview folded contents))))
;;     (save-excursion
;;       (let* ((globalp (memq state '(contents all)))
;;              (beg (if globalp
;;                     (point-min)
;;                     (point)))
;;              (end (if globalp
;;                     (point-max)
;;                     (if (eq state 'children)
;;                       (save-excursion
;;                         (outline-next-heading)
;;                         (point))
;;                       (org-end-of-subtree t)))))
;;         (goto-char beg)
;;         (while (re-search-forward org-drawer-regexp end t)
;;           (save-excursion
;;             (beginning-of-line 1)
;;             (when (looking-at org-drawer-regexp)
;;               (let* ((start (1- (match-beginning 0)))
;;                      (limit
;;                        (save-excursion
;;                          (outline-next-heading)
;;                            (point)))
;;                      (msg (msg) (format
;;                             (concat
;;                               "org-cycle-hide-drawers:  "
;;                               "`:END:`"
;;                               " line missing at position %s")
;;                             (1+ start))))
;;                 (if (re-search-forward "^[ \t]*:END:" limit t)
;;                   (outline-flag-region start (point-at-eol) t)
;;                   (user-error msg))))))))))


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

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

;; Cycle through viewports.
(defun rotate-windows () ;; from whattheemacsd.com
  "Rotate your windows"
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

;;; Smart setting of executable bits for...

;; ... files that start with a #! (shebang) line
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ... files written in bash, python, AppleScript, &c.
(setq executable-binary-suffixes '(".pl"
                                   ".py"
                                   ".scpt"
                                   ".sh"))

;; Globally enable dumb-jump, an it-just-works way to jump
;; from functions' invocations to their definitions of those
;; functions, which supports many languages.
(add-hook 'xref-backend-functions 'dumb-jump-xref-activate)

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

(defalias 'index 'elt) ;; as in Chez Scheme

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

;;;; Git/Magit settings

(defun gline-qdw ()
  (interactive)
  (let* ((dir (file-name-directory buffer-file-name))
         (git-working-copy-root-dir (shell-command "git rev-parse --show-toplevel")))
    (message "dir: %s; gcd: %s" dir git-working-copy-root-dir)))
    (if (not git-working-copy-root-dir)
        (message (format "Not a Git working copy: %s"
                         (file-name-directory buffer-file-name)))
        (replace-regexp git-working-copy-root-dir "" dir)
        (message git-working-copy-root-dir))))
      ;; (login-shell-command-and-show-output-qdw
      ;; (message (format "gline %s %s" buffer-file-name (line-number-at-pos)))
      ;; )
       )))

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
  
;;;; Settings > Hacking > General

;; Make tabs-as-indentation visible, so I can eliminate them

(setq-default
 whitespace-style
 '(face
   ;; empty
   indentation
   ;; trailing
   ;; lines-tail
   missing-newline-at-eof
   ))

(setq whitespace-display-mappings '((tab-mark 9 [187 9] [92 9]))) ;; Optional: Customize tab display
(global-whitespace-mode -1)
(setq-default tab-width 4)

;;;; Settings > Hacking > JavaScript/TypeScript/JSON

(let ((desired-indent-level 2))
  (setq-default
   typescript-indent-level desired-indent-level
   js-indent-level desired-indent-level
   js-switch-indent-offset desired-indent-level)

   tide-node-executable (expand-file-name
                         "~/.asdf/installs/nodejs/16.20.0/bin/node"))

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

;;;; Settings > Hacking > YAML
;; yaml-font-lock-keywords -> yaml-hash-key-re   =   (1 font-lock-keyword-face)

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

;;;; Editor MACroS settings

;;; Global macros/keybindings
(keymap-global-set "M-`" 'unfill-paragraph-xah)
(keymap-global-unset "C-c .")
(keymap-global-set   "C-c ."     'find-file-at-point-with-line-and-column-if-present)
(keymap-global-set   "C-c /"     'grep-buffer-qdw)
(keymap-global-set   "C-c C-c /" 'grep-all-buffers-qdw)
(keymap-global-set   "RET"       'my-enter)
(keymap-global-set   "C-c g"     'search-in-buffers)
(keymap-global-set   "C-c 1"     'async-shell-command)
(keymap-global-set   "C-c :"     'eval-expression)
(keymap-global-set   "C-h F"     'describe-face)
(keymap-global-set   "C-c C-f"   'multi-occur-in-matching-buffers)
(keymap-global-set   "C-c C-b"   'ibuffer)
(keymap-global-set   "C-c c"     'comment-region)
(keymap-global-set   "C-c f"     'origami-toggle-node)
(keymap-global-set   "C-c C-c g"     'rg-in-current-directory-qdw)
(keymap-global-set   "C-c i"     'indent-region)
(keymap-global-set   "C-c C-c b" 'edit-bashrc)
(keymap-global-set   "C-c C-c i" 'ini)
(keymap-global-set   "C-c SPC"   'iterm2-here)
(keymap-global-set   "C-c l"     'shell)
(keymap-global-set   "C-c n"     'display-line-numbers-mode)
(keymap-global-set   "C-c o"     'rotate-windows)
(keymap-global-set   "C-c s"     'isearch-forward-regexp)
(keymap-global-set   "C-c r"     'isearch-backward-regexp)
(keymap-global-set   "C-c C-q t" 'switch-theme)
(keymap-global-set   "C-c t"     'toggle-theme)
(keymap-global-set   "C-c u"     'uncomment-region)
(keymap-global-set   "C-c v"     'vscode)
(keymap-global-set   "M-1"       'login-shell-command-qdw)
(keymap-global-set   "M-!"       'login-shell-command-and-show-output-qdw)
(keymap-global-set   "M-("       'eval-expression)
(keymap-global-set   "M-5"       'query-replace-regexp)
(keymap-global-set   "M-o"       'other-window-reverse)

;;;; Experimental settings to make Emacs's navigation commands work like vim's
;; (defun evilmacs-forward-word ()
;;   "Move point forward using Evil word motion."
;;   (interactive)
;;   (goto-char (evil-forward-word-begin 1)))

;; (keymap-global-set "M-f" 'evilmacs-forward-word)
      
;; (defun evilmacs-backward-word ()
;;   "Move point backward using Evil word motion."
;;   (interactive)
;;   (goto-char (evil-backward-word-begin 1)))


;;;; c-mode macros/keybindings

(keymap-set c-mode-map "RET" 'electric-newline-and-maybe-indent)
(keymap-set c-mode-map "C-c {" 'beginning-of-defun)
(keymap-set c-mode-map "C-c }" 'end-of-defun)

;;;; dired-mode macros/keybindings

(keymap-set dired-mode-map "RET" 'dired-find-file-qdw)

;;;; help-command macros/keybindings

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;;; org-mode macros/keybindings

(keymap-unset org-mode-map "C-c RET")
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

;;;; shell-mode macros/keybindings

(keymap-unset shell-mode-map "C-c .")
(keymap-set shell-mode-map "C-c ." 'find-file-at-point-with-line-and-column-if-present)
;; FIXME (keymap-global-set "C-c s" 'shell-in-dir)

;;;; shell-script-editing-mode macros/keybindings

(define-prefix-command 'sh-mode-prefix-map)
(define-key sh-mode-map (kbd "C-c C-c") 'sh-mode-prefix-map)
(keymap-unset sh-mode-map "C-c C-c i")

;;;; Binary-file-viewing settings

(defun use-hexl-mode-if-binary ()
  "If the current buffer is a binary file, switch to `hexl-mode'."
    (when (string-match-p "\0" (buffer-string))
        (hexl-mode)))
       
(add-hook 'find-file-hook #'use-hexl-mode-if-binary)

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

;; Generated by bing; read, debug, and test
;; 
;; (defun my/org-insert-subheading-with-todo ()
;;   "Insert a subheading with the same TODO state as the current heading."
;;   (interactive)
;;   (let ((current-todo (org-get-todo-state)))
;;     (org-insert-subheading nil)
;;     (when current-todo
;;       (org-todo current-todo))))



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
 '(ansi-color-black ((t (:foreground "black") t)))
 '(ansi-color-blue ((t (:foreground "blue") t)))
 '(ansi-color-bright-black ((t (:foreground "black") t)))
 '(ansi-color-bright-blue ((t (:foreground "blue") t)))
 '(ansi-color-bright-cyan ((t (:foreground "cyan") t)))
 '(ansi-color-bright-green ((t (:foreground "green") t)))
 '(ansi-color-bright-magenta ((t (:foreground "magenta") t)))
 '(ansi-color-bright-red ((t (:foreground "red") t)))
 '(ansi-color-bright-white ((t (:foreground "white") t)))
 '(ansi-color-bright-yellow ((t (:foreground "yellow") t)))
 '(ansi-color-cyan ((t (:foreground "cyan") t)))
 '(ansi-color-green ((t (:foreground "green") t)))
 '(ansi-color-magenta ((t (:foreground "magenta") t)))
 '(ansi-color-name (('t (:foreground ansi-color-symbol) 't)))
 '(ansi-color-red ((t (:foreground "red") t)))
 '(ansi-color-white ((t (:foreground "white") t)))
 '(ansi-color-yellow ((t (:foreground "yellow") t)))
 '(italic ((t (:underline t :slant normal :weight normal)))))

;;;; customize other settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'new-buffer)
 '(column-number-mode t)
 '(custom-enabled-themes (list (intern theme-name)))
 '(custom-safe-themes
   '("d3879290d7be1497b7e9b9347cdc7c97fde8b828a971f11a9065587c46512ba1"
     "2f0c5eee7bc5b05b3590cf8355e625289281c8cfcc4c2a45c0518e60c3e64f41"
     "3c67fe7b8429d6f7674a915c0552f753b7b47756f54eadc29d15ce6e088230ed"
     "159ae4abf0ad2c59a2510e5b96db6455dd5a6009334e2d1fb55d4f581e575978"
     "8833611549f9601a1ce916f81a7fe1991b9725cefb603a7bf7c27e15ba9da13c"
     "513e86c779765823244631afd7512f8781de9992d1a04e20ffc07536b5abf5c1"
     "dcde5ee234edfae4c33cf8a115af01c1ac16ecd8c998685d25da02a7d1fd58b7"
     "9e4c1fe99c24137cc7d3c972e63e7c294336a0e08a111d220bffe6ae6aa25d5f"
     "6f6f83b1f1913e59ea0a7426a817bd174b77ba23fad8a2b477a8d685532b4b5b"
     "bb0baaa8c02224f89cf0c9a97e7584c54bc78db194526c8df96bdc24e39c0e33"
     "b208fb51ab6725140f7454dcef5816064cfd71c0c347866e27b3e30bacaa5424"
     "5a897c2c1919bc2e13321534a250d4fafbf55fa82496cdf47080dc69a23d1468"
     "b70a39d0a8ef98f231a22d39d9dced347275598ec202c8c138972ca40cd29fc0"
     "d09acc0564824346c42919c5cc79edc0990e6f73c615a9c9ec28f4796864791f"
     "81abc2c491be64d999588f05bc16672b771d8965099e16d0ceefc8d2fb6c682a"
     "f027ed38df2792060c1558aabef13bd85cba7735da74d6720bea8e7b8ecaeeeb"
     "52fddd1e2f5a6aecfd4ef03ead7bca5791fbbc6027da48a29b8f54b2f1eeea61"
     "bec1808d1b07e9c43e9208f7bd72f9a84b1b1212fe1458fb5e87b822a78ac0b7"
     "e72ff600fc010ca9abc1db4b7fc9facada4cbb1dbeef1d5a96c54d65b402aa14"
     "afffef1edcc6fbeff42664696b5c4525de7662ad954754e7374f208df39b57e8"
     "5950f03a3ef75043032c938bc1bee8094cb3a14d14b355a395321474727a9cf9"
     "c812704cb47660e990264ca40b5a109f8fee716a76485642004e012ea4d4f999"
     "1f4a44bf30dd3f8c368c18a4414bcb9d69a54842b4170ab693beb5ddf026df49"
     default))
 '(eldoc-idle-delay 0.15)
 '(elpher-default-url-type "gemini")
 '(exec-suffixes nil)
 '(explicit-bash-args '("--noediting" "-i" "-l"))
 '(ls-lisp-use-insert-directory-program nil)
 '(mode-line-position-column-format '(" %C"))
 '(mode-line-position-column-line-format '(" (%l,%C)"))
 '(neo-cwd-line-style 'text)
 '(neo-theme 'classic)
 '(ns-alternate-modifier 'none)
 '(ns-right-alternate-modifier 'meta)
 '(org-agenda-files '("/Users/quinn.weaver/main.org"))
 '(org-babel-load-languages '((shell . t) (emacs-lisp . t)))
 '(org-emphasis-alist
   '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim)
     ("`" org-code verbatim) ("~" (:strike-through t))))
 '(osx-trash-command '("trash" "-F"))
 '(package-selected-packages
   '(## ac-geiser ac-slime ack applescript-mode blacken company
     company-lua counsel-fd devil dired-sidebar dockerfile-mode
     dumb-jump elfeed elisp-slime-nav elpher elpy elpygen elquery evil
     fd-dired fish-mode flymake flymake-lua flymake-yamllint geiser
     geiser-chez geiser-chibi git-modes glue go-mode gptai ivy
     jenkinsfile-mode jiralib2 json-mode logview lsp-origami lua-mode
     lyrics-fetcher macrostep-geiser magit magit-delta magithub
     markdown-mode multiple-cursors org org-bullets origami
     origami-predef osx-plist osx-trash outshine ox-jira pandoc-mode
     paredit paredit-everywhere per-buffer-theme polymode slime
     slime-repl-ansi-color symbol-overlay terraform-mode tide
     titlecase tron-legacy-theme tt-mode typescript-mode vimrc-mode
     w3m wgrep xterm-color yaml-mode yaml-pro yaml-tomato))
 '(safe-local-variable-values '((eval display-ansi-colors)))
 '(sh-indent-statement-after-and nil)
 '(shell-command-dont-erase-buffer nil)
 '(shell-prompt-pattern "\"[[~].*$ "))
 
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
