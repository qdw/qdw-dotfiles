;;; itermesque-theme.el --- Fork of subatomic-theme for terminals that support only 256 colors (for example, macOS Terminal.app).

;; Copyright 2012 John Olsson

;; Author: John Olsson &lt;john@cryon.se&gt;

;; Modified by: Chris Corbyn &lt;chris@w3style.co.uk&gt;
;; https://github.com/cryon/subatomic256
;; Version: 1.3

;; Futher modified by Quinn Weaver &lt;quinn.weaver@gmail.com&gt;
;; https://github.com/qdw/itermesque-theme

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;; 

(deftheme itermesque
  "itermesque emacs theme")

;; (defun 0x (color-name)
;;   (letrec ((rgb (color-name-to-rgb color-name))
;; 	   (hex (color-rgb-to-hex rgb)))
;;     (message "equivalent exchange: %s" color-name)))
					   

  ;;; For terminals that support full 24-bit color
(setq

 background "#002b36"

 black "#373737"
 bright-black "#706f6e"

 ;; red "#ad5f01" ;; actually orange
 red "#a04a20" ;; actually orange
 bright-red red
 
 ;; green "#007f06"
 green "#2b7f46"
 bright-green "7efe08"
 
 yellow "#7b5dee"  ;; actually a blueish purple
 ;; yellow "#7d4cb7" ;; actually a blueish purple
 bright-yellow "#af5faf" ;; actually a reddish purple
 
 blue "#6495ed"
 bright-blue blue

 ;; magenta "#af42c2"
 magenta "#afafaf"
 bright-magenta "#f66ce2"
 
 cyan "#00808b"
 bright-cyan "#008080"
 
 white "#839496"
 bright-white white
 )

(custom-theme-set-faces
   'itermesque

   ;;;; default stuff

   `(default
      ((t (:background ,background :foreground ,white))))

   `(fringe
     ((t (:background ,black))))

   `(vertical-border
     ((t (:background ,black))))

   `(region
     ((t (:background ,cyan :foreground ,black))))

   `(show-paren-match-face
     ((t (:background ,bright-magenta :foreground ,black :bold nil))))

   `(show-paren-mismatch-face
     ((t (:foreground ,red :bold nil))))

   `(isearch
     ((t (:background ,bright-magenta :foreground ,black :bold nil))))

   `(lazy-highlight
     ((t (:background ,black :foreground ,red :bold nil))))

   `(query-replace
     ((t (:inherit lazy-highlight))))

   `(trailing-whitespace
     ((t (:inherit show-paren-mismatch-face :underline t))))
   
   `(mode-line
     ((t (:foreground ,white :background ,bright-black ))))
   
   `(minibuffer-prompt
     ;; ((t (:foreground ,bright-magenta))))
     ((t (:foreground ,yellow))))

     
   `(header-line
     ((t (:background ,black :foreground ,white :foreground ,white))))

   `(hl-line
     ((t (:background ,black))))

   `(highlight-current-line-face
     ((t (:inherit hl-line))))

   `(powerline-active1
     ((t (:background ,bright-black))))

   `(powerline-active2
     ((t (:background ,bright-black :foreground ,white))))

   `(powerline-inactive1
     ((t (:background ,black :foreground ,white :foreground ,white))))

   `(powerline-inactive2
     ((t (:background ,black :foreground ,white))))

   `(escape-glyph
     ((t (:foreground ,bright-magenta))))

   `(link
     ((t (:foreground ,yellow :underline t))))
     
   ;;;; ANSI colors (used in shell-mode)
   
   `(ansi-color-black ((t (:background: ,black :foreground ,black :underline nil))))
   `(ansi-color-bright-black ((t (:background ,bright-black :foreground ,bright-black :underline nil))))
   
   `(ansi-color-red ((t (:background ,red :foreground ,red :underline nil))))
   `(ansi-color-bright-red ((t (:background ,bright-red :foreground ,bright-red :underline nil))))
   
   `(ansi-color-green ((t (:background ,green :foreground ,green :underline nil))))
   `(ansi-color-bright-green ((t (:background ,bright-green :foreground ,bright-green :underline nil))))
   
   `(ansi-color-yellow ((t (:background ,yellow :foreground ,yellow :underline nil))))
   `(ansi-color-bright-yellow ((t (:background ,bright-yellow :foreground ,bright-yellow :underline nil))))
   
   `(ansi-color-blue ((t (:background ,blue :foreground ,blue :underline nil))))
   `(ansi-color-bright-blue ((t (:background ,bright-blue :foreground ,bright-blue :underline nil))))

   `(ansi-color-magenta ((t (:background ,magenta :foreground ,magenta :underline nil))))
   `(ansi-color-bright-magenta ((t (:background ,bright-magenta :foreground ,bright-magenta :underline nil))))

   `(ansi-color-cyan ((t (:background ,cyan :foreground ,cyan :underline nil))))
   `(ansi-color-bright-cyan ((t (:background ,bright-cyan :foreground ,bright-cyan :underline nil))))
      
   `(ansi-color-white ((t (:background ,white :foreground ,white :underline nil))))
   `(ansi-color-bright-white ((t (:background ,bright-white :foreground ,bright-white :underline nil))))
   
   ;;;; font lock

   `(font-lock-keyword-face
     ((t (:foreground ,yellow))))

   `(font-lock-function-name-face
     ((t (:foreground ,blue))))

   `(font-lock-warning-face
     ((t ((:foreground ,bright-green)))))

   `(font-lock-builtin-face
     ((t ((:foreground ,bright-yellow)))))

   `(font-lock-variable-name-face
     ((t (:foreground ,bright-blue))))

   `(font-lock-constant-face
     ;; ((t (:foreground ,white, :italic t))))
     ((t (:foreground ,bright-blue :italic t))))

   `(font-lock-type-face
     ((t (:foreground ,bright-yellow))))

   `(font-lock-negation-char-face
     ((t (:foreground ,bright-magenta))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,bright-magenta))))

   `(font-lock-comment-face
     ((t (:foreground , red))))

   `(font-lock-string-face
     ((t (:foreground ,green))))

   `(font-lock-comment-delimiter-face
     ((t (:foreground ,red))))

   `(font-lock-doc-face
     ((t (:foreground ,red))))
     
   ;;;; flycheck/flymake
   
   `(flycheck-error
     ((t (:underline ,red))))

   `(flymake-errline
     ((t (:underline ,red))))

   `(flymake-warnline
     ((t (:underline ,bright-magenta))))

   ;;;; eshell

   `(eshell-ls-clutter
     ((t (:inherit font-lock-comment-face))))

   `(eshell-ls-executable
     ((t (:foreground ,bright-magenta))))

   `(eshell-ls-directory
     ((t (:foreground ,blue :bold nil))))

   `(eshell-ls-archive
     ((t (:foreground ,red))))

   `(eshell-ls-backup
     ((t (:inherit font-lock-comment-face))))

   `(eshell-ls-missing
     ((t (:inherit font-lock-warning-face))))

   `(eshell-ls-unreadable
     ((t (:inherit font-lock-warning-face))))

   `(eshell-ls-symlink
     ((t (:inherit font-lock-builtin-face))))

   `(eshell-prompt
     ((t (:inherit minibuffer-prompt))))

   `(eshell-ls-backup
     ((t (:foreground ,red))))

   `(eshell-ls-product
     ((t (:inherit default))))

   `(eshell-ls-readonly
     ((t (:inherit font-lock-comment))))

   `(eshell-ls-special
     ((t (:foreground ,bright-magenta))))

   ;;;; calendar

   `(calendar-today-face
     ((t (:foreground ,bright-magenta :bold nil))))

   `(holiday-face
     ((t (:foreground ,bright-green))))

   `(diary-face
     ((t (:foreground ,red))))

   ;;;; erc

   `(erc-default-face
     ((t (:inherit default))))

   `(erc-current-nick-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-action-face
     ((t (:foreground ,bright-magenta))))

   `(erc-dangerous-host-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-highlight-face
     ((t (:weight normal))))

   `(erc-direct-msg-face
     ((t (:foreground ,bright-magenta))))

   `(erc-nick-msg-face
     ((t (:foreground ,blue))))

   `(erc-fool-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-input-face
     ((t (:inherit default))))

   `(erc-error-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-keyword-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-nick-default-face
     ((t (:inherit default))))

   `(erc-prompt-face
     ((t (:inherit eshell-prompt))))

   `(erc-notice-face
     ((t (:foreground ,bright-magenta))))

   `(erc-timestamp-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-pal-face
     ((t (:foreground ,bright-magenta))))

   ;;;; highlight-symbol

   `(highlight-symbol-face
     ((t (:background ,black))))

   ;;;; diff

   `(diff-file-header
     ((t (:background ,bright-magenta :foreground ,black))))

   `(diff-header
     ((t (:inherit default :foreground ,blue))))

   `(diff-indicator-changed
     ((t (:foreground ,bright-magenta))))

   `(diff-changed
     ((t (:foreground ,red))))

   `(diff-indicator-removed
    ((t (:foreground ,red))))

   `(diff-removed
     ((t (:foreground ,"DarkOrange1"))))

   `(diff-indicator-added
     ((t (:foreground ,bright-magenta))))

   `(diff-added
     ((t (:foreground ,green))))

   `(diff-hunk-header
     ((t (:foreground ,white))))

   `(diff-refine-change
     ((t (:background ,magenta))))
   
   `(diff-refine-removed
     ((t (:background ,bright-red))))

   `(diff-refine-added
     ((t (:background ,bright-green))))
     
   ;;;; git
   
   `(git-commit-summary
     ((t (:foreground ,white))))

   ;;;; magit
   
   `(magit-diff-hunk-heading-highlight
     ((t (:background ,bright-magenta))))
   
   `(magit-diff-context
     ((t (:foreground ,white))))
   
   `(magit-diff-context-highlight
     ((t (:foreground ,white))))

   `(magit-branch
     ((t (:foreground ,yellow))))

   `(magit-diff-add
     ((t (:inherit diff-added))))

   `(magit-diff-del
     ((t (:inherit diff-removed))))

   `(magit-diff-file-header
     ((t (:inherit diff-file-header))))

   `(magit-diff-hunk-header
     ((t (:inherit diff-hunk-header))))

   `(magit-diff-none
     ((t (:inherit default))))

   `(magit-header
     ((t (:inherit diff-header))))

   `(magit-item-highlight
     ((t (:background ,black))))

   `(magit-item-mark
      ((t (:background ,black))))

   `(magit-log-graph
     ((t (:foreground ,blue))))

   `(magit-log-head-label-bisect-bad
     ((t (:foreground ,bright-red))))

   `(magit-log-head-label-bisect-good
     ((t (:foreground ,bright-green))))

   `(magit-log-head-label-default
     ((t (:foreground ,bright-magenta))))

   `(magit-log-head-label-local
     ((t (:inherit magit-log-head-label-default :foreground ,bright-magenta))))

   `(magit-log-head-label-patches
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-remote
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-tags
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-message
     ((t (:inherit default))))

   `(magit-log-sha1
     ((t (:foreground ,red))))

   `(magit-section-title
     ((t (:inherit header-line))))

   `(magit-whitespace-warning-face
     ((t (:inherit font-lock-warning))))

   ;;;; compilation
   `(compilation-info
     ((t (:inherit default))))

   `(compilation-warning
     ((t (:inherit font-lock-warning))))

   ;;;; twittering-mode
   `(twittering-username-face
     ((t (:inherit font-lock-keyword-face))))

    `(twittering-uri-face
      ((t (:inherit link))))

    `(twittering-timeline-header-face
      ((t (:foreground ,bright-magenta))))

    `(twittering-timeline-footer-face
      ((t (:inherit twittering-timeline-header-face))))

    ;;;; outline
    `(outline-1
      ((t (:foreground ,red))))

    `(outline-2
      ((t (:foreground ,magenta))))
    
    `(outline-3
      ((t (:foreground ,cyan))))

    `(outline-4
      ((t (:foreground ,cyan))))

    `(outline-5
      ((t (:foreground ,yellow))))

    `(outline-6
      ((t (:foreground ,blue))))

    `(outline-7
      ((t (:foreground ,magenta))))

    `(outline-8
      ((t (:foreground ,bright-yellow))))

    ;;;; org-mode
    `(org-drawer
      ((t (:inherit org-document-info-keyword))))
  
    `(org-document-title
      ((t (:inherit org-document-info-keyword))))

    `(org-document-info
      ((t (:inherit org-document-info-keyword))))

    `(org-meta-line
      ((t (:inherit org-document-info-keyword))))

    `(org-level-1
      ((t (:inherit outline-1))))

    `(org-level-2
      ((t (:inherit outline-2))))

    `(org-level-3
      ((t (:inherit outline-3))))
    
    ;; `(org-level-4
    ;;   ((t (:inherit outline-4))))
    
    `(org-level-4
      ((t (:inherit outline-3))))

    `(org-level-5
      ((t (:inherit outline-5))))

    `(org-level-6
      ((t (:inherit outline-6))))

    `(org-level-7
      ((t (:inherit outline-7))))

    `(org-level-8
      ((t (:inherit outline-8))))

    `(org-hide
      ((t (:foreground ,black))))   ;

    `(org-link
      ((t (:inherit link))))

    `(org-checkbox
      ;; ((t (:background ,black :foreground ,white
      ;;   	       :box (:line-width 1 :style released-button)))))

      ((t (:background ,black :foreground ,white
    		       :box (:line-width 1 :style released-button)))))

    `(org-done
      ((t (:foreground ,green))))
    `(org-headline-done
      ((t (:foreground ,green))))

    `(org-todo
      ((t (:foreground ,bright-magenta))))

    `(org-table
      ((t (:foreground ,white))))

    `(org-date
      ((t (:foreground ,blue))))

    `(org-document-info-keyword
      ((t (:foreground ,blue))))

    `(org-document-info
      ((t (:foreground ,blue))))

    `(org-block-begin-line
     ((t (:background ,black :foreground ,blue))))

    `(org-block-background
      ((t (:background ,black))))

    `(org-block-end-line
      ((t (:inherit org-block-begin-line))))

    `(org-agenda-date-today
      ((t (:foreground ,blue))))

    `(org-agenda-date
      ((t (:foreground ,blue))))

    `(org-agenda-date-weekend
      ((t (:foreground ,red))))

    `(org-agenda-structure
      ((t (:inherit header-line))))

    `(org-warning
      ((t (:inherit font-lock-warning-face))))

    `(org-agenda-clocking
      ((t (:inherit org-date))))

    `(org-deadline-announce
      ((t (:inherit font-lock-warning-face))))

    `(org-formula
      ((t (:inherit font-lock-doc-face))))

    `(org-special-keyword
      ((t (:inherit font-lock-keyword))))

    ;;;; dired+

    `(diredp-compressed-file-suffix
      ((t (:foreground ,red))))

    `(diredp-date-time
      ((t (:foreground ,blue))))

    `(diredp-deletion
      ((t (:foreground ,red))))

    `(diredp-deletion-file-name
      ((t (:foreground ,red :underline t))))

    `(diredp-symlink
      ((t (:foreground ,red))))

    `(diredp-dir-heading
      ((t (:inherit minibuffer-prompt))))

    `(diredp-display-msg
      ((t (:inherit default))))

    `(diredp-exec-priv
      ((t (:foreground ,magenta))))

    `(diredp-write-priv
      ((t (:foreground ,red))))

    `(diredp-read-priv
      ((t (:foreground ,red))))

    `(diredp-dir-priv
      ((t (:foreground ,blue))))

    `(diredp-link-priv
      ((t (:foreground ,red))))

    `(diredp-other-priv
      ((t (:foreground ,red))))

    `(diredp-rare-priv
      ((t (:foreground ,red))))

    `(diredp-no-priv
      ((t (:foreground ,blue))))

    `(diredp-file-names
      ((t (:foreground ,white))))

    `(diredp-file-suffix
      ((t (:inherit dired-file-name))))

    `(diredp-number
      ((t (:foreground ,blue))))

    `(diredp-executable-tag
      ((t (:foreground ,magenta))))

    `(diredp-flag-mark
      ((t (:foreground ,red))))

    `(diredp-flag-mark-line
      ((t (:background ,black))))

    `(diredp-mode-line-marked
      ((t (:foreground ,blue))))

    `(diredp-mode-line-flagged
      ((t (:foreground ,yellow))))

    `(diredp-ignored-file-name
      ((t (:foreground ,bright-black))))

   )

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'itermesque)

(set-cursor-color "DarkOrange1")

;;; itermesque-theme.el ends here
