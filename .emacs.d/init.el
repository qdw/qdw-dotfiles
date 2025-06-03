(setopt vc-follow-symlinks 't)
(setopt load-prefer-newer 't)

;; Settings for the Emacs package system

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Suppress noisy warnings about packages that use deprecated idioms.

(setopt warning-minimum-level :error)
(setopt warning-suppress-types '((comp) (obsolete)))
(setopt warning-suppress-log-types '((comp) (obsolete)))
(setopt byte-compile-warnings nil)

;; Hide window chrome

(setopt inhibit-startup-screen 't)
(require 'tool-bar)
(if (boundp 'menu-bar-mode) (menu-bar-mode -1))
(if (boundp 'tool-bar-mode) (tool-bar-mode -1))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Settings for GUI Emacs versus terminal Emacs

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
      (setopt mouse-sel-mode 't))))

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

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Overwrite the region (selection) when I type. This is how modern GUIs work.
(delete-selection-mode 1) 

(setq-default
 ;; If I write non-ASCII chars in a buffer, then save the file as Unicode
 prefer-coding-system 'utf-8

 ;; Don't auto-hard-wrap lines
 auto-fill-mode nil
 word-wrap nil

 cperl-indent-level 4
 elisp-indent-level 2
 json-encoding-default-indentation "  "
 org-indent-indentation-per-level 4)

(setopt
 ;; Consider filenames case-sensitive (needed on macOS)
 read-file-name-completion-ignore-case nil 
 
 ;; Warn when opening large files
 large-file-warning-threshold 100000000

 ;; Some Unix command-line utilities require a final newline
 ;; (e.g. 'wc -l')
 require-final-newline 't

 ;; Single-space: always.
 colon-double-space nil 
 
 ;; Single-space. Always.
 sentence-end-double-space nil

 ;; Auto-indentation settings

 ;; Use spaces, not tabs.
 indent-tabs-mode nil
 
 ;; The first day of the week is Monday.
 calendar-week-start-day 1
 
 ;; If I have a region selected, then make commands like query-replace
 ;; operate on that region only
 transient-mark-mode 't

 make-backup-files nil
 
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
      (error nil)))
      
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
;;;; and is honored by fd, rg, and maybe others.

(add-to-list 'auto-mode-alist
             '("\\.ignore\\'" . gitignore-mode))

;;;; Some Lisp I wrote, or adapted, or adopted

(add-to-list 'load-path
             (expand-file-name "qdw-themes" user-emacs-directory))

(add-to-list 'load-path
             (expand-file-name "~/src/forks/mariana-theme"))

;; (load-file-if-exists-qdw "secrets.el")
;; (load-file-if-exists-qdw "site.el")

(defvar jira-base-url (getenv "JIRA_BASE_URL"))
(defvar jira-default-project-qdw (getenv "JIRA_DEFAULT_PROJECT"))
(defvar slack-team (getenv "SLACK_TEAM"))

;;;; Dependency settings

;; I'm running emacs as a daemon,
;; so just require all these modules at once;
;; don't get clever with lazy-loading.

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'dirtrack)
(require 'dockerfile-mode)
(require 'evil)
(require 'fd-dired)
(require 'flycheck)
(require 'json)
(require 'markdown-mode)
(require 'menu-bar)
(require 'mouse)
(require 'org)
(require 'osx-trash)
(require 'paredit)
(require 'pandoc-mode)
(require 'recentf)
(require 'ruby-mode)
(require 'scroll-bar)
(require 'sh-script)
(require 'tide)
(require 'tool-bar)
(require 'typescript-mode)
(require 'whitespace)
(require 'yaml-mode)

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

;;; Themes that I created or forked

(defvar qdw-themes '("amber" "amber-without-bold" "mariana" "tritium"))

(mapc
 (lambda (file-name)
         (add-to-list 'custom-theme-load-path (expand-file-name file-name)))
 '("~/.emacs.d/qdw-themes"
   "~/src/forks/mariana-theme"))

;;; Load my current favorite theme

(setopt custom-enabled-themes '())
(defvar my-chosen-theme "tritium")
(load-theme (intern my-chosen-theme) 't)

(defun overlay-org-colors-qdw () 
  (interactive)
  
  (setopt org-catch-invisible-edits "show"
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
    (let ((org-keyword-to-color-qdw
          `(("TODO" . ,bright-magenta)
            ("NEXT_ACTION" . ,bright-red)
            ("IN_PROGRESS" . ,bright-yellow)
            ("GET_HELP" . ,bright-red)
            ("AWAITING_RESPONSE" . ,yellow)
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
          (setopt custom-enabled-themes (list other-theme)
                  per-buffer-theme-default-theme theme)
          (overlay-org-colors-qdw))))


(defun load-theme-wrapper-qdw (orig-fun theme &optional no-confirm no-enable)
  ;; Disable all currently enabled themes
  (mapc 'disable-theme custom-enabled-themes)
  
  (prog1
    ;; Call the original load-theme
    (funcall orig-fun theme no-confirm no-enable)
    
    ;; Always use my custom org colors, regardless of theme
    (overlay-org-colors-qdw))
    
   (setopt per-buffer-theme-default-theme other-theme))
                      
(advice-add 'load-theme :around 'load-theme-wrapper-qdw)

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

(setopt ibuffer-formats
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

(setopt auto-revert-verbose nil)
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

;;;; Quality-of-life settings

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
  (setopt delete-by-moving-to-trash 't)
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
      (setopt insert-directory-program
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
  (interactive "r"
  (let ((fill-column 90002000))
    (fill-region start end))))

(setopt
 async-shell-command-buffer 'new-buffer
 column-number-mode 't
 custom-safe-themes 't
 eldoc-idle-delay 0
 elpher-default-url-type "gemini"
 exec-suffixes nil
 explicit-bash-args '("--noediting" "-i" "-l")
 ls-lisp-use-insert-directory-program nil
 mode-line-position-column-format '(" %C")
 mode-line-position-column-line-format '(" (%l,%C)")
 neo-cwd-line-style 'text
 neo-theme 'classic
 ns-alternate-modifier 'none
 ns-right-alternate-modifier 'meta
 org-agenda-files '("/Users/quinn.weaver/main.org")
 org-babel-load-languages '((shell . t) (emacs-lisp . t))
 org-emphasis-alist '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim
                                                               ("`" org-code verbatim) ("~" (:strike-through t))))
 osx-trash-command '("trash" "-F")
 sh-indent-statement-after-and nil
 shell-command-dont-erase-buffer nil
 shell-prompt-pattern "\"[[~].*$ "
 ;; woman-path
 ;;   '("/Applications/Wireshark.app/Contents/Resources/share/man"
 ;;    "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man"
 ;;    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/share/man"
 ;;    "/Applications/Xcode.app/Contents/Developer/usr/share/man"
 ;;    "/Library/Apple/usr/share/man"
 ;;    "/System/Cryptexes/App/usr/share/man"
 ;;    "/Users/quinn.weaver/.local/share/man"
 ;;    "/Users/quinn.weaver/share/man" "/opt/X11/share/man"
 ;;    "/opt/homebrew/manpages/" "/opt/homebrew/opt/diffutils/share/man"
 ;;    "/opt/homebrew/share/man/" "/opt/node/lib/node_modules/npm/man/"
 ;;    "/opt/node/share/man/" "/usr/X11/man" "/usr/local/share/man"
 ;;    "/usr/share/man")
 )

;;;; eBook/e-reader settings

(add-to-list
 'auto-mode-alist
 '("\\.epub\\'" .
   (lambda ()
     (add-to-list 'load-path (expand-file-name "~/src-3/nov.el") nil)
     (load-file (expand-file-name "~/src-3/nov.el/nov.el"))
     (nov-mode))))
     
;;;; Generative AI/LLM/genai settings

(defun get-openai-key ()
  (getenv "OPENAI_API_KEY"))

(defun get-curl-path ()
  "/opt/homebrew/bin/curl")

(setopt
 gptel-api-key 'get-openai-key
 gptel-use-curl 't
 gptel--curl-path 'get-curl-path)

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

(defun shell-command-qdw (&optional prefix)
  "Prompt for a shell command If there is a region selected, then insert it as the last argument in the prompt, with the point positioned immediately before that last argument. If PREFIX is non-nil, then insert the command output into in the current buffer at point."
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
      (concat
       "source ~/.bashrc && INSIDE_EMACS=1 "
       command-string))))
  
;; ;; Don't prompt me about shell sessions, debug sessions, et cetera
;; ;; when I quit Emacs. Just kill them.

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

Before every command, run `dirs`. This should prevent shell-mode
from frequently getting confused about what dir is the current
working directory, as it often does.

(defun dirs-qdw (ignored-string)
  (when (derived-mode-p 'shell-mode)
    (shell-resync-dirs)))

(add-hook 'comint-input-filter-functions 'dirs-qdw)
(add-hook 'comint-output-filter-functions 'dirs-qdw)

;;;; Visual bell settings

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg)
                                 (set-face-foreground 'mode-line fg))
                               orig-fg))))
(tool-bar-mode -1)   Turn off toolbar.
(menu-bar-mode -1)   Turn off menubar.
(scroll-bar-mode -1) Turn off scrollbar.

;;;; Time-related settings

(defun ts()
  "Insert today's date in YYYY-MM-DD format."
  (interactive)
  (insert (shell-command-to-string "date +%Y-%m-%d")))

;;;; Search/compile/shell-command output buffer settings

(defun disable-underline-qdw (orig-fun &rest args)
  "Advice around `compile` to temporarily disable underlining (it looks bad)."
  (let ((original-underline-face (face-attribute 'underline :underline)))
    (unwind-protect
      (progn
          Call the original compile function
          (apply orig-fun args)
          Change the 'underline' face in the compilation buffer
          (with-current-buffer "*compilation*"
            (set-face-attribute 'underline nil :underline nil)))
      Restore the original 'underline' face
      (set-face-attribute 'underline nil :underline original-underline-face))))

(advice-add 'compile :around #'disable-underline-qdw)

;;;; Search settings

(setq-default
 Make isearch case-sensitive by default.
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
        Momentarily disable underlining; it looks bad in compilation-mode
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

This regexp is correct...
(setq jira-ticket-regexp-qdw
      "\\b[[:upper:]]\\{2,10\\}-[[:digit:]]-\\{2,\\9}")
... but this function is broken                              
(defun qdw-isearch-forward-jira-ticket
  (interactive)
  (isearch-resume jira-ticket-regexp-qdw 't nil 't nil 't))
    
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

;;;; Experimental org-mode settings

;; ;; Generated by bing; read, debug, and test
;; 
;; (defun org-insert-subheading-with-todo-qdw ()
;;   "Insert a subheading with the same TODO state as the current heading."
;;   (interactive)
;;   (let ((current-todo (org-get-todo-state)))
;;     (org-insert-subheading nil)
;;     (when current-todo
;;       (org-todo current-todo))))

;;;; org-mode: Jira-related settings

(defun org-insert-jira-link-qdw ()
  (interactive)
  (letrec (
           (pre-filled-text (if (boundp 'jira-default-project-qdw)
                                (concat jira-default-project-qdw "-")
                                ""))
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
                     (concat jira-base-url "/browse/" issue-name)
                     issue-name)))
                     

;; ;; FIXME: doesn't work
;; (defun jira-issue-create-from-region-qdw ()
;;   (interactive)
;;   (if (use-region-p)
;;       (let ((summary (shell-quote-argument (buffer-substring-no-properties
;;                                              (region-beginning)
;;                                              (region-end)))))
;;         (shell-command (concat
;;           "source ~/.bashrc && INSIDE_EMACS=1 jic "
;;           summary)))))

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


Viewport management

(defun other-window-reverse ()
  (interactive)
  (other-window -1))

Cycle through viewports.
(defun rotate-windows () from whattheemacsd.com
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
 Auto Scroll: keep me from craning my neck
 by automatically scrolling when the cursor gets
 a few lines from the top or bottom of a buffer
 scroll-margin 13
 
 When I first press Ctrl-L while on a given line,
 scroll so that the line appears at the top,
 not the middle, of the screen.
 
 This behavior is affected by the setting of
 scroll-margin: the top of the screen is
 defined as line 1 plus scroll-margin
 (same for the bottom).
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

;;;; diff and merge settings

;; When invoking ediff commands, use a new pane
;; to the right of the current pane
;; (don't create a new frame).

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Executable-script settings

;; ... files that start with a shebang line

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

(setopt lisp-indent-function #'common-lisp-indent-function)
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

;;;; Global keybindings

(keymap-global-set "M-`" 'unfill-paragraph-xah)
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
(keymap-global-set   "C-c C-c C-t" 'switch-theme)
(keymap-global-set   "C-c t"     'toggle-theme)
(keymap-global-set   "C-c u"     'uncomment-region)
(keymap-global-set   "C-c v"     'vscode)
(keymap-global-set   "M-!"       'shell-command-qdw)
(keymap-global-set   "M-("       'eval-expression)
(keymap-global-set   "M-5"       'query-replace-regexp)
(keymap-global-set   "M-o"       'other-window-reverse)

;;;; Experimental lyrics-downloading settings

;; (defun munge-song-or-band-name (string)
;;   "Prepare a STRING to be saved as a part of a filename."
;;   (replace-regexp-in-string
;;    (rx (or "<" ">" ":" "\"" "/" "\\" "|" "?" "*" " "))
;;    "-"
;;    string))
;; (setq lyrics-fetcher-format-file-name-method
;;       'munge-song-or-band-name)

;;;; Experimental settings to make Emacs's navigation commands work like vim's
;;;; (cf. evil-mode)

;; (defun evilmacs-forward-word ()
;;   "Move point forward using Evil word motion."
;;   (interactive)
;;   (goto-char (evil-forward-word-begin 1)))

;; (keymap-global-set "M-f" 'evilmacs-forward-word)
      
;; (defun evilmacs-backward-word ()
;;   "Move point backward using Evil word motion."
;;   (interactive)
;;   (goto-char (evil-backward-word-begin 1)))

;;;; C settings

(keymap-set c-mode-map "RET" 'electric-newline-and-maybe-indent)
(keymap-set c-mode-map "C-c {" 'beginning-of-defun)
(keymap-set c-mode-map "C-c }" 'end-of-defun)

;;;; dired-mode keybindings

(keymap-set dired-mode-map "RET" 'dired-find-file-qdw)

;;;; help-command keybindings

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-v") 'find-variable)

;;;; org-mode keybindings

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

;;;; shell-mode keybindings

(keymap-set shell-mode-map "C-c ." 'find-file-at-point-with-line-and-column-if-present)

;;;; shell-script-editing-mode keybindings

;; (define-prefix-command 'sh-mode-prefix-map)
;; (define-key sh-mode-map (kbd "C-c C-c") 'sh-mode-prefix-map)
;; (keymap-unset sh-mode-map "C-c C-c i")

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

;; Prominently highlight any tabs used for indentation in source code,
;; (but not in the PostgreSQL source code or in Go, because
;; tabs are the convention there).

(set-face-attribute 'whitespace-tab nil
                    :background red
                    :foreground black)

;;;; Git/Magit settings

(require 'magit)

(defun gline-qdw ()
  (interactive)
  (let* ((dir (file-name-directory buffer-file-name))
         (git-working-copy-root-dir (shell-command "git rev-parse --show-toplevel")))
    (message "dir: %s; gcd: %s" dir git-working-copy-root-dir)
    (if (not git-working-copy-root-dir)
        (message (format "Not a Git working copy: %s"
                         (file-name-directory buffer-file-name)))
        (replace-regexp git-working-copy-root-dir "" dir)
        (message (format "About to run gline in dir '%s':" git-working-copy-root-dir))
        ;; (shell-command-qdw
        (message
         (format "gline %s %s" buffer-file-name (line-number-at-pos))))))

In the default view, show stashes and local branches.
Also, move the sections around a bit to suit my preferences.

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

(custom-set-variables
 '(package-selected-packages
   '(exec-path-from-shell ## ac-geiser ac-slime ack applescript-mode blacken company company-lua counsel-fd devil dired-sidebar dockerfile-mode dumb-jump elfeed elisp-slime-nav elpher elpy elpygen elquery evil fd-dired fish-mode flymake flymake-lua flymake-yamllint geiser geiser-chez geiser-chibi git-modes glue go-mode ivy jenkinsfile-mode json-mode logview lsp-origami lua-mode macrostep-geiser markdown-mode multiple-cursors org org-bullets origami-predef osx-plist osx-trash outshine ox-jira pandoc-mode paredit paredit-everywhere per-buffer-theme slime slime-repl-ansi-color symbol-overlay terraform-mode tide titlecase tron-legacy-theme tt-mode typescript-mode vimrc-mode w3m wgrep xterm-color yaml-mode yaml-pro yaml-tomato)))
