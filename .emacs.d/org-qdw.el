;; -*- lexical-binding: t; -*-

;;;; Experimental: use a newer org-mode than the bundled one (via straight.el)

(use-package org
  :after url-util
  :straight (:host github :repo "emacs-straight/org-mode" :local-repo "org")

  :config

  ;; (setopt org-hide-drawers ('top-level-property-drawer ""))
  ;; (org-hide-drawers-mode +1)

  (setopt org-archive "::"
          org-cycle-separator-lines 1
          
          org-cycle-hide-drawer-startup t
          org-cycle-hide-drawers 'all
          
          org-return-follows-link t

          ;; org-hide-drawer-startup t ;; old org versions

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
               (canonical-path-qdw path)))))

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
               ("RECOMMENDATION" . ,yellow)
               ("SUBPROJECT" . ,bright-cyan)
            )))

        (setopt org-todo-keyword-faces
                (mapc
                 (lambda (pair) (add-to-list 'org-todo-keyword-faces pair))
                 org-keyword-to-color-qdw)))))


  (overlay-org-colors-qdw))


;; (use-package 'org-ql :straight (:host :repo :local-repo)
;;   ...)
