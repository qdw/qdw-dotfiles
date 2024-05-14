;; (defun 0x (color-name)
;;   (letrec ((rgb (color-name-to-rgb color-name))
;; 	   (hex (color-rgb-to-hex rgb)))
;;     (message "equivalent exchange: %s" color-name)))
					   

(cond
  ;;; For terminals that support full 24-bit color
  ((string= "iTerm.app" (getenv "TERM_PROGRAM"))
   (setq-default

    black "#262626"
    bright-black "#383838"

    red "#af5f00" ;; actually orange
    bright-red red
    
    ;; green "#007f06"
    green "#2b7f46"
    bright-green "#00ff00"
        
    yellow "#7b5dee"  ;; actually a blueish purple
    ;; yellow "#7d4cb7" ;; actually a blueish purple
    bright-yellow "#af5faf" ;; actually a reddish purple
    
    blue "#6495ed"
    bright-blue blue
    
    magenta "#af42c2"
    ;; bright-magenta "#e475dc"
    bright-magenta "#f66ce2"
    
    ;; cyan "#00747f"
    cyan "#007e8a"
    bright-cyan "#008080"
    
    white "#91908f"
    bright-white white
    ))
  
   ;;; For terminals that support only 256 colors
   ;;; (i.e., almost every terminal in use today)
  ('t 
   (setq-default
    
    black "#262626"        ;; "Grey15"     == 235
    bright-black "#5f5f5f" ;; "Grey37"     ==  59

    red "#af5f00" ;; "DarkOrange3"         == 130
    bright-red red

    green "#008700"        ;; "Green4"     ==  28
    bright-green "#00ff00" ;; "Green1"     ==  46
    
    yellow "#875fff"       ;; "SlateBlue1" == 99 ;; in Emacs, this displays as purple, despite the X11 name
    bright-yellow yellow

    blue "#00afff" ;; "DeepSkyBlue1"   ==  39
    bright-blue blue

    magenta "#cd00cd"        ;; "Magenta3" == 127
    bright-magenta "#ff87ff" ;; "Orchid1"  == 213
    
    cyan "#00af87"           ;; "DarkCyan" ==  36
    bright-cyan cyan

    white "#949494"          ;; "Grey58" == 246
    bright-white white
    ))
)

;;; [Untested; might not work] For terminals that support only 16 colors

;; (setq-default
;;  black "#000000"        ;; "Black (SYSTEM)"  ==  0 == ANSI black
;;  bright-black           ;; "Grey (SYSTEM)"   == 8  == ANSI bright-black
;;  red "#800000"          ;; "Maroon (SYSTEM)" ==  1 == ANSI red
;;  bright-red red
;;  green "#008000"        ;; "Green (SYSTEM)"  ==  2 == ANSI green
;;  bright-green "#00ff00" ;; "Lime"            == 10 == ANSI bright-green
;;  yellow "#800080"       ;; "Purple (SYSTEM)" ==  5 == ANSI magenta
;;  blue "#000080"         ;; "Navy (SYSTEM)"   ==  4 == ANSI blue
;;  bright-blue blue
;;  magenta "#800080"      ;; "Purple (SYSTEM)" ==  5 == ANSI magenta
;;  bright-magenta         ;; "Fuchsia (SYSTEM)" == 13  == ANSI bright-magenta
;;  cyan "#008080"         ;; "Teal (SYSTEM)"   ==  6 == ANSI cyan
;;  bright-cyan cyan
;;  white bright-black
;;  bright-white white
;;  )

;;; [Untested; might not work] For terminals that support only 8 colors

;; (setq-default
;; black "#000000"          ;; "Black (SYSTEM)"   == 0 == ANSI black
;; bright-black "#c0c0c0"   ;; "Silver (SYSTEM)"  == 7 == ANSI white
;; red "#800000"            ;; "Maroon (SYSTEM)"  == 1 == ANSI red
;; bright-red red
;; green "#008000"          ;; "Green (SYSTEM)"   == 2 == ANSI green
;; bright-green green
;; yellow "#800080"         ;; "Purple (SYSTEM)"  == 5 == ANSI magenta
;; bright-yellow yellow
;; blue "#000080"           ;; "Navy (SYSTEM)"    == 4 == ANSI blue
;; bright-blue blue
;; magenta "#800080"        ;; "Purple (SYSTEM)"  == 5 == ANSI magenta
;; bright-magenta "#800000" ;; "Maroon (SYSTEM)"  == 1 == ANSI red
;; cyan "#008080"           ;; "Teal (SYSTEM)"    == 6 == ANSI cyan
;; bright-cyan cyan
;; white bright-black
;; bright-white white
;; )
