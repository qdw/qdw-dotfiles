;;; For terminals that support full 24-bit color
(setq

    black "#0d2a34"        ;; "Grey15"     == 235
    ;; black "#000000" ;; Use the terminal emulator's ANSI-black color.    

    bright-black "#4c4c4c"

    red "#a26321" ;; actually orange
    bright-red "#ed7233" ;; bright orange; used for the cursor, inter alia
    
    green "#377f46"
    bright-green "#9ffc4e"
    
    yellow "#7558f5" ;; actually lavender
    bright-yellow yellow
    
    blue "#4280be"
    bright-blue blue
    
    magenta "#a249bb"
    bright-magenta "#b35eae"
    
    cyan "#377e89"
    bright-cyan cyan
    
    white "#869395"
    bright-white white
    )
    
(defvar *colors-loaded-p* 't)
