;; (find-file "~/d/projects/config-tasks.rst")
;; (find-file "~/d/projects/tasks.rst")
;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________

;; Don't prompt me about shell sessions, debug sessions, et cetera
;; when I quit emacs. Just kill them.
(add-hook 'comint-exec-hook 
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;;; Basic sanity


(setq require-final-newline 't)      ;; Always put a newline at end of file.
(delete-selection-mode +1)           ;; Overwrite selection when I type.
(setq-default indent-tabs-mode nil)  ;; Tabs considered harmful.
(setq-default sentence-end-double-space nil) ;; Single-space. Always.
(setq-default colon-double-space nil);; Single-space: always.
(setq-default auto-fill-mode nil)    ;; Don't wrap lines.
(setq-default word-wrap nil)         ;; Really don't wrap lines!
(prefer-coding-system 'utf-8)        ;; Use UTF-8 where possible.
(setq transient-mark-mode 't)        ;; Operate on highlighted region.
(auto-compression-mode 1)            ;; Auto-uncompress compressed files.
(setq-default vc-follow-symlinks 't) ;; Follow links to git files w/o nagging!
(blink-cursor-mode 0)                ;; Solid cursor (blinking is distracting).
(setq-default cursor-type 'box)      ;; Box cursor (line is too hard to see).
(setq column-number-mode nil)        ;; Turn off col num display (distracting).
(global-font-lock-mode 't)
;; (setq font-lock-maximum-decoration '((t nil))) ;; Only minimal syntax coloring.
(setq inhibit-startup-screen 't)     ;; No GNU spam.
(setq-default visible-bell 't)             ;; Turn off beep; use visible bell.
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
(require 'tool-bar)   (tool-bar-mode -1)   ;; Turn off toolbar.
(require 'menu-bar)   (menu-bar-mode -1)   ;; Turn off menubar.
(require 'scroll-bar) (scroll-bar-mode -1) ;; Turn off scrollbar.
(setq-default make-backup-files nil) ;; Don't leave my_filename~ droppings.
(setq large-file-warning-threshold 100000000) ;; Bug me only when file >= 100MB

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

;; Keyboard shortcuts
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\M-5"  'query-replace-regexp)
(global-unset-key "\C-x\C-o") (global-set-key   "\C-x\C-o" 'rotate-windows)
(global-unset-key "\C-c\C-f") (global-set-key "\C-c\C-f" 'multi-occur-in-matching-buffers)

;; Coding: Lisp
(setq lisp-indent-function 'common-lisp-indent-function)

(load "~/.emacs.d/site-lisp.el" 't)
