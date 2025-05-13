;;; tritium-theme.el --- Fork of the original subatomic-theme
;;; for terminals that support only 256 colors
;;; (for example, macOS Terminal.app).

;; Copyright 2012 John Olsson
;; Author: John Olsson &lt;john@cryon.se&gt;

;; Modified by: Chris Corbyn &lt;chris@w3style.co.uk&gt;
;; https://github.com/cryon/subatomic256
;; Version: 1.3

;; Futher modified by Quinn Weaver &lt;quinn.weaver@gmail.com&gt;
;; https://github.com/qdw/tritium-theme

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see http://www.gnu.org/licenses/

;; (defvar black)
;; (defvar bright-black)
;; (defvar red)
;; (defvar bright-red)
;; (defvar green)
;; (defvar bright-green)
;; (defvar yellow)
;; (defvar bright-yellow)
;; (defvar blue)
;; (defvar bright-blue)
;; (defvar magenta)
;; (defvar bright-magenta)
;; (defvar cyan)
;; (defvar bright-cyan)
;; (defvar white)
;; (defvar bright-white)


(cond

  ((or (string= "truecolor" (getenv "COLORTERM"))
      (string= "24bit" (getenv "COLORTERM")))

   (setq
           ;; black  "#141414"  ;;      Grey15
           black  "#000000"
    bright-black  "#343434"
             red  "#af5f00"  ;;      actually orange
      bright-red  "#ff7f00"  ;;      bright orange; used for the cursor &c.
           green  "#3b8560"
    bright-green  "#00a700"  ;; bright green ("#00ff00" = brightest possible green)
          yellow  "#9164ff"  ;;      actually purple
   bright-yellow  "#b5b500"  ;; bright yellow; "#ffff00"= brightest possible yellow
            blue  "#6495ed"  ;; decent darker blue (from iTerm): "#0d22b9"

            
     bright-blue  "#5172d3"
         magenta  "#af5faf"
  bright-magenta  "#d8a0d0"
            cyan  "#028080"
     bright-cyan  "#4bbfc4"
           white  "#767676"
    bright-white  "#767676")  ;; "#aaaaaa"
  
   (setq
extended-black bright-black
extended-bright-black "#676767"
extended-bright-red "#800000"
extended-bright-white "#a6acb9"))
  
  ;;; For terminals that support only 256 colors
  ;;; (i.e., almost every other terminal in use today)
   ('t 
   (setq
             black  "#141414"  ;; 235  Grey15
          ;; black  "#000000"  ;;  16  Terminal emulator's ANSI black
          ;; black  "color-16" ;;   0  Terminal emulator's background
      bright-black  "#5f5f5f"  ;;  59  Grey37
               red  "#af5f00"  ;; 130  DarkOrange3
        bright-red  "#ff7f00"  ;;  xx  DarkOrange1  ;; no X11 name
             green  "#008700"  ;;  28  Green4
      bright-green  "#00ff00"  ;;  46  Green1
            yellow  "#875fff"  ;;  99  SlateBlue1   ;; actually purple
     bright-yellow  yellow     ;;  99  SlateBlue1   ;; actually purple
              blue  "#00afff"  ;;  39  DeepSkyBlue1
       bright-blue  blue       ;;  39  DeepSkyBlue1
           magenta  "#af00af"  ;; 127  Magenta3
    bright-magenta  "#ff87ff"  ;; 213  Orchid1
              cyan  "#00af87"  ;;  36  DarkCyan
       bright-cyan  cyan       ;;  36  DarkCyan
             white  "#949494"  ;; 246  Grey58
      bright-white  white      ;; 246  Grey58
      ))
 
  ;;; Incomplete: for terminals that support only 16 colors
;; ((condition-1)
;; (setq
;;           black  "#000000"  ;;   0  ANSI black
;;    bright-black  ???        ;;   ?  ANSI bright-black
;;             red  "#800000"  ;;   1  ANSI red
;;      bright-red  red             1  ANSI red
;;           green  "#008000"  ;;   2  ANSI green
;;    bright-green  "#00ff00"  ;;   2  ANSI green
;;          yellow  "#800080"  ;;   5  ANSI magenta
;;   bright-yellow  "#800080"  ;;   5  ANSI magenta
;;            blue  "#800080"  ;;   4  ANSI blue
;;     bright-blue  blue       ;;   4  ANSI blue
;;         magenta  "#800080"  ;;   5  ANSI magenta
;;  bright-magenta  "#800080"  ;;   5  ANSI magenta
;;            cyan  "#008080"  ;;   6  ANSI cyan
;;     bright-cyan  bright-black    ?  ANSI bright-black
;;           white  ????       ;;   ?
;;    bright-white  white      ;;   ?
;;  ))

  ;;; Incomplete: for terminals that support only 8 colors
;; ((condition-2)
;; (setq
;;           black  "#000000"  ;;   0  ANSI black
;;     bright-black white
;;             red  "#800000"  ;;   1  ANSI red
;;      bright-red  red
;;           green  "#008000"  ;;   2  ANSI green
;;    bright-green  green
;;          yellow  "#800080"  ;;   5  ANSI magenta
;;   bright-yellow  yellow
;;            blue  "#800080"  ;;   4  ANSI blue
;;     bright-blue  blue
;;         magenta  "#800080"  ;;   5  ANSI magenta
;;  bright-magenta  magenta
;;            cyan  "#008080"  ;;   6  ANSI cyan
;;     bright-cyan cyan
;;           white  ????       ;;   ?
;;    bright-white  white
;;  ))
) ;; cond

(dolist (prefix '("" "bright-"))
  (dolist (color-name '("black"
                        "red"
                        "green"
                        "yellow"
                        "blue"
                        "magenta"
                        "cyan"
                        "white"))
    (let* ((ansi-color-name (concat "ansi-color-" prefix color-name))
           (ansi-color-symbol (intern ansi-color-name))
           (desired-hex-value (symbol-value (intern color-name))))
      (set ansi-color-symbol desired-hex-value)
      (face-spec-set ansi-color-symbol `((t (:foreground ,desired-hex-value)))))))

(deftheme tritium
  "tritium emacs theme")

(custom-theme-set-faces
   'tritium
   
   `(ansi-color-yellow
     ((t (:foreground ,ansi-color-yellow) t))))
      
(custom-theme-set-faces
   'tritium
   
   ;;;; default stuff
   
   `(cursor
      ((t (:background ,bright-red))))

   `(default
      ((t (:background ,black :foreground ,white))))

   `(forbidden-whitespace
      ((t (:background ,bright-green :foreground ,black))))

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
     ((t (:background ,bright-yellow :foreground ,black :bold nil))))

   `(isearch-fail
     ((t (:background ,cyan :foreground ,black :bold nil))))

   `(lazy-highlight
     ((t (:background ,bright-yellow :foreground ,black :bold nil))))

   `(query-replace
     ((t (:inherit lazy-highlight))))

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
   
   ;;;; customize

   `(widget-field
     ((t (:foreground ,black :background ,bright-red))))
     
   ;;;; window dressing (including the mode-line)
   
   ;; When you're in display-line-numbers-mode,
   ;; this is the face in which line numbers
   ;; are displayed to the left of your code
   `(line-number
     ((t (:foreground ,bright-black))))
     
   `(diredp-mode-line-marked
      ((t (:foreground ,blue))))

   `(diredp-mode-line-flagged
     ((t (:foreground ,yellow))))
     
   `(mode-line
     ((t (:foreground ,blue :background ,bright-black))))

   `(mode-line-inactive
     ((t (:foreground ,cyan :background ,bright-black))))
     
   ;;;; font lock

   `(font-lock-builtin-face
    ((t ((:foreground ,blue)))))
   
   `(font-lock-comment-delimiter-face
     ((t (:foreground ,cyan))))
     
   `(font-lock-comment-face
     ((t (:foreground ,red))))

   `(font-lock-constant-face
     ((t (:foreground ,bright-red))))

   `(font-lock-delimiter-face
     ((t (:foreground ,bright-cyan))))
     
   `(font-lock-doc-face
     ((t (:foreground ,magenta))))

   `(font-lock-doc-markup-face
     ((t (:foreground ,bright-cyan))))
     
   `(font-lock-function-name-face
     ((t (:foreground ,blue))))

   `(font-lock-function-call-face
     ((t (:foreground ,blue))))   
     
   `(font-lock-keyword-face
     ((t (:foreground ,yellow))))

   `(font-lock-preprocessor-face
     ((t (:foreground ,bright-green))))
     
   `(font-lock-property-face
     ((t (:foreground ,bright-cyan))))     

   `(font-lock-property-use-face
     ((t (:foreground ,green))))

   `(font-lock-punctuation-face
     ((t (:foreground ,yellow))))
     
   `(font-lock-string-face
     ((t (:foreground ,green))))

   `(font-lock-type-face
     ((t (:foreground ,magenta))))

   `(font-lock-variable-name-face
     ((t (:foreground ,blue))))

   `(font-lock-variable-use-face
     ((t (:foreground ,magenta))))

   `(font-lock-warning-face
     ((t ((:foreground ,bright-magenta :underline (:style wave :color bright-red))))))
     
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
     
   ;;;; shell-mode
   
   

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
     ((t (:inherit magit-log-head-label-default
          :foreground ,bright-magenta))))

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
     ((t (:background ,bright-green :foreground ,black))))

   ;;;; compilation

   `(compilation-error
     ((t (:foreground ,bright-red :underline nil))))
      
   `(compilation-info
     ((t (:inherit default))))

   `(compilation-line-number
     ((t (:inherit font-lock-keyword-face :underline nil))))
     
   `(compilation-warning
     ((t (:inherit font-lock-warning))))

    ;;;; outline

    `(outline-1
      ((t (:foreground ,blue))))

    `(outline-2
      ((t (:foreground ,red))))
    
    `(outline-3
      ((t (:foreground ,magenta))))
      
    `(outline-4
      ((t (:foreground ,cyan))))

    `(outline-5
      ((t (:foreground ,bright-blue))))

    `(outline-6
      ((t (:foreground ,white))))

    `(outline-7
      ((t (:foreground ,extended-bright-black))))

    `(outline-8
      ((t (:foreground ,extended-black))))

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
      ((t (:foreground ,black))))

    `(org-link
      ((t (:inherit link))))

    `(org-checkbox
      ((t (:background ,black
           :foreground ,white
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
      ((t (:foreground ,bright-red))))

    `(org-document-info-keyword
      ((t (:foreground ,extended-bright-black))))

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

    `(diredp-ignored-file-name
      ((t (:foreground ,bright-black))))
      
    ;;;; Markdown
    
    `(markdown-code-face
      ((t (:noinherit :foreground ,magenta))))

    `(markdown-header-delimiter-face
      ((t :inherit font-lock-comment-delimiter-face)))

    ;;;; whitespace
    `(whitespace-big-indent
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-empty
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-hspace
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-indentation
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-line
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-missing-newline-at-eof
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-newline
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-space
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-space-after-tab
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-space-before-tab
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-tab
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    `(whitespace-trailing
      ((t (:background ,bright-green :foreground ,black))))
      ;; ((t (:inherit forbidden-whitespace))))

    ) ;; custom-theme-set-faces

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tritium)

;;; tritium-theme.el ends here
