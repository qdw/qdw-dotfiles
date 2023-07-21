;;; qdw-subatomic256-fork-theme.el --- Fork of subatomic-theme for terminals that support only 256 colors (for example, macOS Terminal.app).

;; Copyright 2012 John Olsson

;; Author: John Olsson &lt;john@cryon.se&gt;

;; Modified by: Chris Corbyn &lt;chris@w3style.co.uk&gt;
;; https://github.com/cryon/subatomic256
;; Version: 1.3

;; Futher modified by Quinn Weaver &lt;quinn.weaver@gmail.com&gt;
;; https://github.com/qdw/qdw-subatomic256-fork-theme

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

(deftheme qdw-subatomic256-fork
  "qdw-subatomic256-fork emacs theme")

(letrec (
      (midnight          "#1c1c1c")
      (midnight-1        "#262626")
      (midnight-2        "#a8a8a8")
      (midnight-3        "#5f5f87")
      (mystic-blue       "#5f5f87")
      (phosphor-amber    "#af5f00")
      (comment-amber phosphor-amber)
      (blueish-purple    "#9164ff")
      (sea-blue          "#1077a3")
      (light-cyan        "#008787")
      (cyan              "#00cdcd")
      (sea-green-cyan    "#2f4f4f")
      (sea-green-cyan+1  "#008080")
      (victory-blue      "#87afd7")
      (defeat-blue       "#005fd7") ;; saturated dark blue
      (full-green        "#00ff00")
      (undergrowth-green "#87af5f")
      (dark-green "#469439")
      ;; (jungle-green      "#afd75f")
      (jungle-green      undergrowth-green)
      ;; (deep-gold         "#ffaf00")
      (deep-gold phosphor-amber)
      (axiomatic-purple  "#af5faf")
      (comment-purple axiomatic-purple) 
      (brick-red         "#d7875f")
      (piggy-pink        "#ffd7d7")
      ;; (dim-gray          "#a9a9a9")
      (dim-gray          "#91908f")
      (color-236         "#303030")
      (darker-gray       color-236)
      ;; (relaxed-white     "#d7d7d7")
      ;; (relaxed-white     "#a3a49a")
      ;; (relaxed-white     "#7a7979")
      ;; (relaxed-white     "#707272")
      (relaxed-white     dim-gray)
      (cold-mud          "#d7afaf")
      ;; (full-white        "#ffffff")
      (full-white        dim-gray)
      (full-black        "#000000")
      (full-red          "#ff0000")
      (full-blue         "#0000ff")
      (full-yellow       "#ffff00")
      (full-magenta      "#ff00ff")
      (low-key-magenta   "#ae2ade")
      (bright-magenta    "#ff00ff")
      (pink-though       "#f5b5ff"))
  (custom-theme-set-faces
   'qdw-subatomic256-fork

   ;; default stuff

   `(default
      ((t (:background ,midnight-1 :foreground ,dim-gray))))

   `(fringe
     ((t (:background ,midnight-1))))

   `(vertical-border
     ((t (:foreground ,midnight-2))))

   `(region
     ((t (:background ,mystic-blue :foreground ,full-white))))

   `(show-paren-match-face
     ((t (:foreground ,full-green :bold t))))

   `(show-paren-mismatch-face
     ((t (:foreground ,full-red :bold t))))

   `(isearch
     ((t (:background ,midnight-2 :foreground ,full-green :bold t))))

   `(lazy-highlight
     ((t (:background ,midnight-2 :foreground ,deep-gold :bold t))))

   `(query-replace
     ((t (:inherit lazy-highlight))))

   `(trailing-whitespace
     ((t (:inherit show-paren-mismatch-face :underline t))))

;  `(modeline-inactive
;     ((t (:foreground ,darker-gray :background ,full-black))))
   
   `(mode-line
     ((t (:foreground ,dim-gray :background ,darker-gray ))))
   
   `(minibuffer-prompt
     ;; ((t (:foreground ,axiomatic-purple :weight bold))))
     ((t (:foreground ,blueish-purple :weight bold))))

     
   `(header-line
     ((t (:background ,midnight-2 :foreground ,relaxed-white :foreground ,full-white :weight bold))))

   `(hl-line
     ((t (:background ,midnight-1))))

   `(highlight-current-line-face
     ((t (:inherit hl-line))))

   ;; `(powerline-active1
   ;;   ((t (:background ,darker-gray))))

   ;; `(powerline-active2
   ;;   ((t (:background ,darker-gray :foreground ,relaxed-white))))

   ;; `(powerline-inactive1
   ;;   ((t (:background ,midnight-2 :foreground ,relaxed-white :foreground ,relaxed-white))))

   ;; `(powerline-inactive2
   ;;   ((t (:background ,midnight-2 :foreground ,relaxed-white))))

   `(escape-glyph
     ((t (:foreground ,cold-mud :weight bold))))

   `(link
     ;; ((t (:foreground ,victory-blue :weight bold :underline t))))
     ((t (:foreground ,blueish-purple :weight bold :underline t))))

   ;; font lock

   `(font-lock-keyword-face
     ;; ((t (:foreground ,deep-gold :weight bold))))
     ((t (:foreground ,phosphor-amber))))

   `(font-lock-function-name-face
     ;; ((t (:foreground ,victory-blue))))
     ;; ((t (:foreground ,deep-gold))))
     ((t (:foreground ,axiomatic-purple))))

   `(font-lock-warning-face
     ((t ((:foreground ,brick-red)))))

   `(font-lock-builtin-face
     ((t ((:foreground ,phosphor-amber)))))

   `(font-lock-variable-name-face
     ((t (:foreground ,blueish-purple))))

   `(font-lock-constant-face
     ((t (:foreground ,relaxed-white, :weight bold :italic t))))

   `(font-lock-type-face
     ;; ((t (:foreground ,victory-blue :weight bold))))
     ((t (:foreground ,defeat-blue :weight bold))))

   `(font-lock-negation-char-face
     ((t (:foreground ,brick-red :weight bold))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,cold-mud))))

   `(font-lock-comment-face
     ((t (:foreground , comment-amber))))

   `(font-lock-string-face
     ((t (:foreground ,jungle-green))))

   `(font-lock-comment-delimiter-face
     ((t (:foreground , comment-amber))))

   `(font-lock-doc-face
     ((t (:foreground ,axiomatic-purple :italic t))))

   ;; flymake

   `(flymake-errline
     ((t (:underline ,full-red))))

   `(flymake-warnline
     ((t (:underline ,full-yellow))))

   ;; eshell

   `(eshell-ls-clutter
     ((t (:inherit font-lock-comment-face))))

   `(eshell-ls-executable
     ((t (:foreground ,jungle-green))))

   `(eshell-ls-directory
     ((t (:foreground ,victory-blue :bold t))))

   `(eshell-ls-archive
     ((t (:foreground ,deep-gold))))

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
     ((t (:foreground ,brick-red :slant italic))))

   `(eshell-ls-product
     ((t (:inherit default :weight bold))))

   `(eshell-ls-readonly
     ((t (:inherit font-lock-comment))))

   `(eshell-ls-special
     ((t (:foreground ,cold-mud))))

   ;; calendar

   `(calendar-today-face
     ((t (:foreground ,jungle-green :bold t))))

   `(holiday-face
     ((t (:foreground ,brick-red))))

   `(diary-face
     ((t (:foreground ,axiomatic-purple))))

   ;; erc

   `(erc-default-face
     ((t (:inherit default))))

   `(erc-current-nick-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-action-face
     ((t (:foreground ,cold-mud))))

   `(erc-dangerous-host-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-highlight-face
     ((t (:weight bold))))

   `(erc-direct-msg-face
     ((t (:foreground ,jungle-green))))

   `(erc-nick-msg-face
     ((t (:foreground ,victory-blue :weight bold))))

   `(erc-fool-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-input-face
     ((t (:inherit default :weight bold))))

   `(erc-error-face
     ((t (:inherit font-lock-warning-face))))

   `(erc-keyword-face
     ((t (:inherit font-lock-keyword-face))))

   `(erc-nick-default-face
     ((t (:inherit default))))

   `(erc-prompt-face
     ((t (:inherit eshell-prompt))))

   `(erc-notice-face
     ((t (:foreground ,axiomatic-purple))))

   `(erc-timestamp-face
     ((t (:inherit font-lock-comment-face))))

   `(erc-pal-face
     ((t (:foreground ,jungle-green))))

   ;; highlight-symbol

   `(highlight-symbol-face
     ((t (:background ,midnight-3))))

   ;; diff

   `(diff-file-header
     ((t (:background ,midnight-1 :foreground ,victory-blue))))

   `(diff-header
     ((t (:inherit default :foreground ,mystic-blue))))

   `(diff-indicator-changed
     ((t (:foreground ,full-yellow :weight bold))))

   `(diff-changed
     ((t (:foreground ,deep-gold))))

   `(diff-indicator-removed
    ((t (:foreground ,full-red :weight bold))))

   `(diff-removed
     ((t (:foreground ,phosphor-amber))))

   `(diff-indicator-added
     ((t (:foreground ,full-green :weight bold))))

   ;; `(diff-added
   ;;   ((t (:foreground ,jungle-green))))

   `(diff-added
     ((t (:foreground ,sea-green-cyan+1))))

   `(diff-hunk-header
     ((t (:foreground ,relaxed-white))))

   ;; `(diff-refine-change
   ;;   ((t (:background ,full-yellow :foreground ,blueish-purple :weight bold))))

   `(diff-refine-change
     ((t (:background ,piggy-pink :weight bold))))
   
   ;; `(diff-refine-removed
   ;;   ((t (:background ,full-red :foreground: ,relaxed-white :weight bold)))
   
   `(diff-refine-removed
     ((t (:background ,piggy-pink))))

   ;; `(diff-refine-added
   ;;   ((t (:background ,full-green :foreground ,blueish-purple :weight bold))))
   `(diff-refine-added
     ((t (:background ,victory-blue :weight bold))))
     
   ;; git
   
   `(git-commit-summary
     ((t (:foreground ,relaxed-white))))

   ;; magit
   
   `(magit-diff-context-highlight
      ((t (:background ,full-black))))

   `(magit-branch
     ((t (:foreground ,jungle-green :weight bold))))

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
     ((t (:background ,midnight-2))))

   `(magit-item-mark
      ((t (:background ,midnight-2))))

   `(magit-log-graph
     ((t (:foreground ,victory-blue))))

   `(magit-log-head-label-bisect-bad
     ((t (:foreground ,brick-red))))

   `(magit-log-head-label-bisect-good
     ((t (:foreground ,jungle-green))))

   `(magit-log-head-label-default
     ((t (:foreground ,axiomatic-purple :weight bold))))

   `(magit-log-head-label-local
     ((t (:inherit magit-log-head-label-default :foreground ,jungle-green))))

   `(magit-log-head-label-patches
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-remote
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-head-label-tags
     ((t (:inherit magit-log-head-label-default))))

   `(magit-log-message
     ((t (:inherit default))))

   `(magit-log-sha1
     ((t (:foreground ,deep-gold))))

   `(magit-section-title
     ((t (:inherit header-line))))

   `(magit-whitespace-warning-face
     ((t (:inherit font-lock-warning))))

   ;; compilation
   `(compilation-info
     ((t (:inherit default))))

   `(compilation-warning
     ((t (:inherit font-lock-warning))))

   ;; twittering-mode
   `(twittering-username-face
     ((t (:inherit font-lock-keyword-face))))

    `(twittering-uri-face
      ((t (:inherit link))))

    `(twittering-timeline-header-face
      ((t (:foreground ,cold-mud :weight bold))))

    `(twittering-timeline-footer-face
      ((t (:inherit twittering-timeline-header-face))))

    ;; outline
    `(outline-1
      ((t (:foreground ,phosphor-amber))))

    `(outline-2
      ((t (:foreground ,cyan))))

    `(outline-3
      ((t (:foreground ,low-key-magenta))))

    `(outline-4
      ((t (:foreground ,low-key-magenta))))

    `(outline-5
      ((t (:foreground ,blueish-purple))))

    `(outline-6
      ((t (:foreground ,defeat-blue))))

    `(outline-7
      ((t (:foreground ,axiomatic-purple))))

    `(outline-8
      ((t (:foreground ,undergrowth-green))))

    ;; org-mode
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
    
    `(org-level-4
      ((t (:inherit outline-4))))

    `(org-level-5
      ((t (:inherit outline-5))))

    `(org-level-6
      ((t (:inherit outline-6))))

    `(org-level-7
      ((t (:inherit outline-7))))

    `(org-level-8
      ((t (:inherit outline-8))))

    `(org-hide
      ((t (:foreground ,midnight-1))))   ;

    `(org-link
      ((t (:inherit link))))

    `(org-checkbox
      ;; ((t (:background ,midnight-1 :foreground ,full-white :weight bold
      ;;   	       :box (:line-width 1 :style released-button)))))

      ((t (:background ,midnight-1 :foreground ,full-white
    		       :box (:line-width 1 :style released-button)))))

    `(org-done
      ((t (:foreground ,jungle-green :weight bold))))
      ;; ((t (:foreground ,jungle-green))))

    ;; `(org-todo
    ;;   ((t (:foreground ,bright-magenta :weight bold))))
    `(org-todo
      ((t (:foreground ,pink-though :weight bold))))

    `(org-table
      ((t (:foreground ,cold-mud))))

    `(org-date
      ((t (:foreground ,deep-gold :weight bold))))

    `(org-document-info-keyword
      ((t (:foreground ,mystic-blue))))

    `(org-document-info
      ((t (:foreground ,cold-mud :weight bold :slant italic))))

    ;; `(org-block-begin-line
    ;;  ((t (:background ,midnight-2 :foreground ,mystic-blue :weight bold))))
    `(org-block-begin-line
     ((t (:background ,midnight-2 :foreground ,mystic-blue bold))))

    `(org-block-background
      ((t (:background ,midnight-1))))

    `(org-block-end-line
      ((t (:inherit org-block-begin-line))))

    ;; `(org-agenda-date-today
    ;;   ((t (:foreground ,jungle-green :background ,midnight-2 :weight bold))))
    `(org-agenda-date-today
      ((t (:foreground ,jungle-green :background ,midnight-2))))

    `(org-agenda-date
      ((t (:foreground ,victory-blue))))

    `(org-agenda-date-weekend
      ((t (:foreground ,deep-gold))))

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

    ;; dired+

    `(diredp-compressed-file-suffix
      ((t (:foreground ,deep-gold :weight bold))))

    `(diredp-date-time
      ((t (:foreground ,mystic-blue))))

    `(diredp-deletion
      ((t (:foreground ,brick-red :weight bold :slant italic))))

    `(diredp-deletion-file-name
      ((t (:foreground ,brick-red :underline t))))

    `(diredp-symlink
      ((t (:foreground ,deep-gold))))

    `(diredp-dir-heading
      ((t (:inherit minibuffer-prompt))))

    `(diredp-display-msg
      ((t (:inherit default))))

    `(diredp-exec-priv
      ((t (:foreground ,jungle-green))))

    `(diredp-write-priv
      ((t (:foreground ,brick-red))))

    `(diredp-read-priv
      ((t (:foreground ,deep-gold))))

    `(diredp-dir-priv
      ((t (:foreground ,victory-blue :weight bold))))

    `(diredp-link-priv
      ((t (:foreground ,deep-gold))))

    `(diredp-other-priv
      ((t (:foreground ,deep-gold :weight bold))))

    `(diredp-rare-priv
      ((t (:foreground ,brick-red :weight bold))))

    `(diredp-no-priv
      ((t (:foreground ,mystic-blue))))

    `(diredp-file-names
      ((t (:foreground ,dim-gray))))

    `(diredp-file-suffix
      ((t (:inherit dired-file-name))))

    `(diredp-number
      ((t (:foreground ,victory-blue))))

    `(diredp-executable-tag
      ((t (:foreground ,jungle-green :weight bold))))

    `(diredp-flag-mark
      ((t (:foreground ,brick-red :weight bold))))

    `(diredp-flag-mark-line
      ((t (:background ,midnight-2))))

    `(diredp-mode-line-marked
      ((t (:foreground ,brick-red))))

    `(diredp-mode-line-flagged
      ((t (:foreground ,deep-gold))))

    `(diredp-ignored-file-name
      ((t (:foreground ,midnight-3))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'qdw-subatomic256-fork)

(set-cursor-color "DarkOrange1")

;;; qdw-subatomic256-fork-theme.el ends here
