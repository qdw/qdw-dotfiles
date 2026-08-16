;;;; AI settings == LLM settings == genai settings




;;; emacs-copilot by jart: code completion using an open-source, local LLM

;; From https://github.com/jart/emacs-copilot/blob/main/README.md
 
;; (defun copilot-complete ()
;;   (interactive)
;;   (let* ((spot (point))
;;          (inhibit-quit 't)
;;          (current-file (buffer-file-name))
;;          (cash (concat current-file ".cache"))
;;          (hist (concat current-file ".prompt"))
;;          (lang (file-name-extension current-file))

;;          ;; extract current line, to left of the caret
;;          ;; plus the previous line. We will pass
;;          ;; these to the LLM.
;;          (code (save-excursion
;;                  (dotimes (i 2)
;;                    (when (> (line-number-at-pos) 1)
;;                      (previous-line)))
;;                  (beginning-of-line)
;;                  (buffer-substring-no-properties (point) spot)))

;;          ;; create an English-langiage prompt for this interaction
;;          (system "\
;; You are an Emacs code generator. \
;; Writing comments is forbidden. \
;; Writing test code is forbidden. \
;; Writing English explanations is forbidden. ")
;;          (prompt (format
;;                   "[INST]%sGenerate %s code to complete:[/INST]\n```%s\n%s"
;;                   (if (file-exists-p cash) "" system) lang lang code)))

;;     ;; iterate text deleted within editor then purge it from prompt
;;     (when kill-ring
;;       (save-current-buffer
;;         (find-file hist)
;;         (dotimes (i 10)
;;           (let ((substring (current-kill i t)))
;;             (when (and substring (string-match-p "\n.*\n" substring))
;;               (goto-char (point-min))
;;               (while (search-forward substring nil t)
;;                 (delete-region (- (point) (length substring)) (point))))))
;;         (save-buffer 0)
;;         (kill-buffer (current-buffer))))

;;     ;; append prompt for current interaction to the big old prompt
;;     (write-region prompt nil hist 'append 'silent)

;;     ;; run llamafile streaming stdout into buffer catching ctrl-g
;;     (with-local-quit
;;       (call-process "wizardcoder-python-34b-v1.0.Q5_K_M.llamafile"
;;                     nil (list (current-buffer) nil) t
;;                     "--prompt-cache" cash
;;                     "--prompt-cache-all"
;;                     "--silent-prompt"
;;                     "--temp" "0"
;;                     "-c" "1024"
;;                     "-ngl" "35"
;;                     "-r" "```"
;;                     "-r" "\n}"
;;                     "-f" hist))

;;     ;; get rid of most markdown syntax
;;     (let ((end (point)))
;;       (save-excursion
;;         (goto-char spot)
;;         (while (search-forward "\\_" end t)
;;           (backward-char)
;;           (delete-backward-char 1 nil)
;;           (setq end (- end 1)))
;;         (goto-char spot)
;;         (while (search-forward "```" end t)
;;           (delete-backward-char 3 nil)
;;           (setq end (- end 3))))

;;       ;; append generated code to prompt
;;       (write-region spot end hist 'append 'silent))))

;;;; ^ End of jart's emacs-copilot settings

;; ;;;; GitHub Copilot settings (slower than emacs-copilot; not yet working)

;; (defun copilot-disable-p-qdw ()
;;   "When copilot should not automatically show completions."
;;   (not (derived-mode-p 'typescript-mode)))
  
;; (add-to-list 'copilot-disable-predicates 'copilot-disable-p-qdw)

;; ;; Adapted from https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

;; (defun rk/copilot-change-activation ()
;;   "Switch between three activation modes:
;;   - automatic: copilot will automatically overlay completions
;;   - manual: you need to press a key (M-C-<return>) to trigger completions
;;   - off: copilot is completely disabled."
;;   (interactive)
;;   (if (and copilot-mode rk/copilot-manual-mode)
;;       (progn
;;         (message "deactivating copilot")
;;         (global-copilot-mode -1)
;;         (setq rk/copilot-manual-mode nil))
;;     (if copilot-mode
;;         (progn
;;           (message "activating copilot manual mode")
;;           (setq rk/copilot-manual-mode t))
;;       (message "activating copilot mode")
;;       (global-copilot-mode))))

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

;;;; Experimental org-mode settings

;; This regexp is correct...
;; (setq jira-ticket-regexp-qdw
;;       "\\b[[:upper:]]\\{2,10\\}-[[:digit:]]-\\{2,\\9}")

;; ... but this function is broken       
;; (defun qdw-isearch-forward-jira-ticket
;;   (interactive)
;;   (isearch-resume jira-ticket-regexp-qdw 't nil 't nil 't))
    
;; (defun org-insert-pr-link-qdw (repo pr-number)
;;   "Insert a link to a GitHub pull request.
;;   REPO is the name of the repository,
;;   and PR-NUMBER is, well, what it sounds like."
;;   (interactive "MRepo: \nnPR number: ")
;;   (org-insert-link nil
;;                    (format "https://github.com/%s/%s/pull/%s"
;;                            getenv("GITHUB_ORG")
;;                            repo
;;                            pr-number)))

;; ;; Generated by bing; read, debug, and test
;; 
;; (defun org-insert-subheading-with-todo-qdw ()
;;   "Insert a subheading with the same TODO state as the current heading."
;;   (interactive)
;;   (let ((current-todo (org-get-todo-state)))
;;     (org-insert-subheading nil)
;;     (when current-todo
;;       (org-todo current-todo))))
;; 
;; FIXME: doesn't work
;; (defun jira-issue-create-from-region-qdw ()
;;   (interactive)
;;   (if (use-region-p)
;;       (let ((summary (shell-quote-argument (buffer-substring-no-properties
;;                                              (region-beginning)
;;                                              (region-end)))))
;;         (shell-command (concat
;;           "source ~/.bashrc && INSIDE_EMACS=1 jic "
;;           summary)))
;;       (error "No selection")))
;; 
;; (defun org-insert-pr-link-qdw (repo pr-number)
;;   (interactive "MRepo: \nnPR number: ")
;;   (org-insert-link nil
;;                    (format "https://github.com/%s/%s/pull/%s"
;;                            getenv("GITHUB_ORG")
;;                            repo
;;                            pr-number)))

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

;;;; Experimental Usenet settings

;; (setopt gnus-select-method '(nntp "news.eternal-september.org")
;;         gnus-secondary-select-methods '((nntp "news.sonic.net"))
;;         gnus-newsgroup-maximum-articles nil
;;         gnus-thread-hide-subtree 't)

;; Prominently highlight any tabs used for indentation in source code,
;; (but not in the PostgreSQL source code or in Go, because
;; tabs are the convention there).

;; (set-face-attribute 'whitespace-tab nil
;;                     :background red
;;                     :foreground black)
;;;; Experimental diff, merge, and git settings

;; (defun open-current-line-in-github-web-ui-qdw ()
;;   (interactive)
;;   (let*
;;     ((filename
;;       (file-name-nondirectory (buffer-file-name)))
;;      (line
;;       (number-to-string (line-number-at-pos))))
;;     ;; (src-dir-regexp
;;     ;; (concat home "/src.*/")
;;     ;; 
;;     ;; (truncated-filename
;;     ;; (replace-regexp-in-string src-dir-regexp "" filename 't 't))
;;     ;; 
;;     ;; (print (concat filename ":" line))))
;;     (async-shell-command (concat "gh browse " filename ":" line)
;;                          "emacs-gh-stdout"
;;                           "emacs-gh-stderr")))

;; Globally enable dumb-jump, an it-just-works way to jump
;; from functions' invocations to their definitions of those
;; functions, which supports many languages.

