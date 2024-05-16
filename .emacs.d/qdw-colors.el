;; (defun 0x (color-name)
;;   (letrec ((rgb (color-name-to-rgb color-name))
;; 	   (hex (color-rgb-to-hex rgb)))
;;     (message "equivalent exchange: %s" color-name)))
					   

(cond
  ;;; For terminals that support full 24-bit color
  ((string= "iTerm.app" (getenv "TERM_PROGRAM"))
   (setq

    black "#262626"
    bright-black "#383838"

    red "#af5f00" ;; actually orange
    bright-red "#ff7f00" ;; bright orange; used for the cursor, inter alia
    
    green "#007f06"
    bright-green "#00ff00"
    
    yellow "#9164ff" ;; actually purple
    bright-yellow yellow
    
    blue "#6495ed"
    bright-blue blue
    
    magenta "#f5b5ff"
    bright-magenta "#af5faf"
    
    cyan "#008080"
    bright-cyan cyan
    
    white "#91908f"
    bright-white white
    ))
  
   ;;; For terminals that support only 256 colors
   ;;; (i.e., almost every terminal in use today)
  ('t 
   (setq
    
    black "#262626"        ;; "Grey15"     == 235
    bright-black "#5f5f5f" ;; "Grey37"     ==  59

    red "#af5f00" ;; "DarkOrange3"         == 130
    bright-red "#ff7f00" ;; "DarkOrange1"  == ???  ;; bright orange; used for the cursor, inter alia. This color has a name in Emacs but not in X11. In SGR 256-color space, it seems to correspond roughly to color-166(?)

    green "#008700"        ;; "Green4"     ==  28
    bright-green "#00ff00" ;; "Green1"     ==  46
    
    yellow "#875fff"       ;; "SlateBlue1" == 99 ;; in Emacs, this displays as purple, despite the X11 name
    bright-yellow yellow

    blue "#00afff" ;; "DeepSkyBlue1"   ==  39
    bright-blue blue

    magenta "#af00af"        ;; "Magenta3" == 127
    bright-magenta "#ff87ff" ;; "Orchid1"  == 213
    
    cyan "#00af87"           ;; "DarkCyan" ==  36
    bright-cyan cyan

    white "#949494"          ;; "Grey58" == 246
    bright-white white
    ))
)

;;; [Untested; might not work] For terminals that support only 16 colors

;; (setq
;;  black "#000000"        ;; "Black (SYSTEM)"  ==  0  == ANSI black
;;  bright-black           ;; "Grey (SYSTEM)"   == 8 == ANSI bright-black
;;  red "#800000"          ;; "Maroon (SYSTEM)" ==  1  == ANSI red
;;  bright-red red
;;  green "#008000"        ;; "Green (SYSTEM)"  ==  2  == ANSI green
;;  bright-green "#00ff00" ;; "Lime"            == 10 == ANSI bright-green
;;  yellow "#800080"       ;; "Purple (SYSTEM)" ==  5  == ANSI magenta
;;  blue "#000080"         ;; "Navy (SYSTEM)"   ==  4  == ANSI blue
;;  bright-blue blue
;;  magenta "#800080"      ;; "Purple (SYSTEM)" ==  5  == ANSI magenta
;;  bright-magenta "Aqua"
;;  cyan "#008080"         ;; "Teal (SYSTEM)"   ==  6  == ANSI cyan
;;  bright-cyan cyan
;;  white bright-black
;;  bright-white white
;;  )

;;; [Untested; might not work] For terminals that support only 8 colors

;; (setq
;; black "#000000"        ;; "Black (SYSTEM)"   ==  0  == ANSI black
;; bright-black "#c0c0c0" ;; "Silver (SYSTEM)"  ==  7  == ANSI white
;; red "#800000"          ;; "Maroon (SYSTEM)"  ==  1  == ANSI red
;; bright-red red
;; green "#008000"        ;; "Green (SYSTEM)"   ==  2  == ANSI green
;; bright-green green
;; yellow "#800080"       ;; "Purple (SYSTEM)"  ==  5  == ANSI magenta
;; bright-yellow yellow
;; blue "#000080"         ;; "Navy (SYSTEM)"    ==  4  == ANSI blue
;; bright-blue blue
;; magenta "#800080"      ;; "Fuchsia (SYSTEM)" == 13  == ANSI bright-magenta
;; bright-magenta         ;; "Red (SYSTEM)"     == 9  == ANSI bright-red
;; cyan "#008080"         ;; "Teal (SYSTEM)"   ==   6  == ANSI cyan
;; bright-cyan cyan
;; white bright-black
;; bright-white white
;; )
