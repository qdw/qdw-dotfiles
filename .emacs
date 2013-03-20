;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My stupid module system, which I should reimplement in terms of desire.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun use-module (module-name)
  (load-file (concat (expand-file-name "~/.emacs.d/elisp/00-dot-emacs-modules/")
                     module-name
                     ".el")))

;;;;;;;;;;;;;;;
;; Basic sanity
;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)  ;; Tabs considered harmful.
(prefer-coding-system 'utf-8)        ;; Use UTF-8 where possible.
(setq transient-mark-mode 't)        ;; Operate on highlighted region.
(auto-compression-mode 1)            ;; Auto-uncompress compressed files.
(setq-default vc-follow-symlinks 't) ;; Follow links to git files w/o nagging!
(blink-cursor-mode 0)                ;; Solid cursor (blinking is distracting).
(setq-default cursor-type 'box)      ;; Box cursor (line is too hard to see).
(setq column-number-mode nil)        ;; Turn off col num display (distracting).
(setq fill-column 79)                ;; Wrap at 79 columns (per PEP 8).
(setq global-font-lock-mode nil)     ;; Stop junking up my display with colors.
(setq font-lock-maximum-decoration '((t nil))) ;; Only minimal syntax coloring.
(setq inhibit-startup-screen 't)     ;; No GNU spam.

;; On quit, just kill any running shell processes w/o nagging.
(add-hook 'shell-mode-hook
  (lambda ()
    (process-kill-without-query (get-process "shell"))))

;;;;;;;;;;;;;;;;;;
;; Basic UI config
;;;;;;;;;;;;;;;;;;

(setq-default visible-bell 't)             ;; Turn off beep; use visible bell.
(require 'tool-bar)   (tool-bar-mode -1)   ;; Turn off toolbar.
(require 'menu-bar)   (menu-bar-mode -1)   ;; Turn off menubar.
(require 'scroll-bar) (scroll-bar-mode -1) ;; Turn off scrollbar.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG: when opening a GPG-encrypted file, decrypt it automatically
;; (prompting me for the passphrase if necessary)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; desktop.el: restore state (files, positions in files) from my last session.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load "desktop")
(setq-default desktop-load-locked-desktop 't)
(desktop-read "~/.emacs.d/desktop")
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsclient: accept files sent to us via emacsclient.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Run the server.
(server-start 't)

;; Automatically save files from emacsclient when I close them.
(set 'server-temp-file-regexp ".") ; i.e. match all files.

;;;;;;;;;;;;;;;;;;;;;;
;; Cutting and pasting
;;;;;;;;;;;;;;;;;;;;;;

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
(setq x-select-enable-clipboard 't)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing: ancient Greek
;;;;;;;;;;;;;;;;;;;;;;;;;

(set-input-method 'greek-babel)
(inactivate-input-method) ; C-\ will activate it.

;;;;;;;;;;;;;;;;;;
;; Coding: general
;;;;;;;;;;;;;;;;;;
(use-module "coding-general")

;; Comment out current line - http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
(defun line-comment()
  "Comments out current line."
  (interactive)
  (comment-region (line-beginning-position) (+ 1 (line-end-position))))

;; Uncomment current line   - http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
(defun line-uncomment()
  "Uncomments current line."
  (interactive)
  (uncomment-region (line-beginning-position) (+ 1 (line-end-position))))

;; Comment out a paragraph and duplicate it.
;;                          - http://www.emacswiki.org/cgi-bin/wiki/extraedit.el
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
  "Forcibly indent the whole buffer."
  (indent-region (point-min) (point-max) 't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding:  Apache config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; (add-hook 'after-save-hook 'restart-catalyst-server-if-controller)

;;;;;;;;;;;;;;;;;
;; Coding:  XHTML
;;;;;;;;;;;;;;;;;

;; Use nxml-mode for all XHTML files and templates.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/nxml-mode"))
(require 'nxml-mode)
(setq nxml-child-indent 4)
(defalias 'html-mode 'nxml-mode)
(defalias 'html-helper-mode 'nxml-mode)

;; Also use nxml-mode for Template Toolkit templates.
(add-to-list 'auto-mode-alist
             '("\\.tt2$" . nxml-mode))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/ecmascript-mode"))
(require 'ecmascript-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding:  Asterisk config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asterisk-config-file-p ()
  (string-match "/extensions.conf$" buffer-file-name))

(defun hup-asterisk-if-asterisk-config-file ()
  "If this file is an Asterisk config file, then tell Asterisk to re-read
its config files (e.g. extensions.conf).

Add this function to `after-save-hook' to make it happen automatically
when you save such files."
  (if (asterisk-config-file-p)
      ;; FIXME:  Report success/failure to a buffer?
      (call-process "/bin/sh" nil nil nil
                   "-c" "/usr/bin/sudo kill -HUP `cat /var/run/asterisk.pid`")))

(add-hook 'after-save-hook 'hup-asterisk-if-asterisk-config-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding: C-like languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "ellemtel")

            ;; Get rid of the behavior of c-electric-colon,
            ;; which left-justifies the line whenever you type a colon :/
            (global-set-key ":" 'self-insert-command)))

;;;;;;;;;;;;
;; Coding: C
;;;;;;;;;;;;

(setq c-indent-level 4)

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

;;;;;;;;;;;;;;;;;;;;;
;; Coding: Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;
;; Coding:  Perl
;;;;;;;;;;;;;;;;

(use-module "perl")

;; Recognize .t files as Perl code; open them in perl-mode
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode)
                            auto-mode-alist))

;; perlcritic
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/perlcritic"))
(setq perlcritic-severity 1)
(autoload 'perlcritic          "perlcritic" "" t)
(autoload 'perlcritic-region   "perlcritic" "" t)
(autoload 'perlcritic-mode     "perlcritic" "" t)

;; cperl-mode
(defalias 'perl-mode 'cperl-mode) ;; Use cperl-mode as the default perl mode.
(add-hook 'cperl-mode-hook
  (lambda ()
    (local-set-key "\C-q" 'perlcritic)
    (local-set-key "\M-q"
      (lambda ()
        (interactive)
        (query-replace "\"" "'")))))
;; Indentation
(setq cperl-close-paren-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-indent-level 4)
(setq cperl-indent-parens-as-block t)
(setq cperl-tab-always-indent t)
(global-set-key "\r" 'newline-and-indent)

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

;;;;;;;;;;;;;;;;;
;; Coding: Python
;;;;;;;;;;;;;;;;;

(use-module "python")

;;;;;;;;;;;;;;
;; Coding: SQL
;;;;;;;;;;;;;;

;; The variables sql-user and sql-password do not play properly with
;; PostgreSQL.  Instead, you have to specify them in sql-postgres-options (you
;; may use ~/.pgpass for the password, as I have elected to do).

(setq sql-user "build")
(setq sql-database "")
(setq sql-host "")
(setq sql-postgres-options '("-U" "build" "pager=off"))

;;;;;;;;;;;;;;;;
;; Coding:  YAML
;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/yaml-mode/trunk"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(setq yaml-indent-offset 4)

;; When I hit enter, auto-indent the next line.
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;

;; Use C-o to switch windows; the default C-x o is too cumbersome,
;; and the default binding for C-o just inserts a newline,
;; which you can do with Enter.
(global-set-key "\C-o" 'other-window)
(add-hook 'dired-mode-hook  (lambda () (local-set-key "\C-o" 'other-window)))

;; Use C-; to switch buffers; the default C-x b is too cumbersome for this
;; frequently used command.
;;
;; We have to use a vector to represent this key sequence,
;; because ; in a string is interpreted as a LISP comment.
;; See http://www.gnu.org/software/emacs/elisp-manual/html_node/Strings-of-Events.html
(global-set-key [67108923] 'switch-to-buffer)

;; Make C-u delete the whole current line (regardless of the initial
;; position of the point), replacing the line with "~/".
;;
;; I use this to clear the minibuffer when I want to find a file in
;; my home directory, not in the current working directory.
;; Keyboard shortcuts
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\M-5"  'query-replace-regexp)
(global-set-key "\C-u" (lambda ()
                         (interactive)
                         (if (string-match "^23" emacs-version)
                             (move-beginning-of-line nil) ; same as C-a
                             (beginning-of-line nil))     ; same as C-a
                         (kill-line)                      ; same as C-k
                         (insert "~/")))

;; Unfortunately, buffer-local bindings override global ones,
;; and minor-mode bindings shadow buffer-local ones.
;; So I have to modify some minor modes to make these keys work everywhere.
;; See M-s below for an example...
(global-set-key "\M-s"  'shell)
(add-hook 'text-mode-hook (lambda () (local-set-key "\M-s" 'shell)))
(global-set-key "\M-c"  'compile)
(global-set-key "\C-f"  'goto-line)
(global-set-key "\M-m"  'man)
(global-unset-key "\M-t") ;; Prevent accidental damage.
