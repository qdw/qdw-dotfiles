(setqu
 
 qdw-black "#141414"
 qdw-red "#af5f00" ;; actually orange
 qdw-green "#007f06"
 qdw-yellow "#9164ff" ;; actually purple
 qdw-blue "#6495ed"
 qdw-cyan "#008080"
 qdw-magenta "#af5faf"
 qdw-white "#91908f"

 
 qdw-bright-black "#383838"
 qdw-bright-red "#ff7f00" ;; actually bright orange
 qdw-bright-green "#00ff00"
 qdw-bright-yellow "#ffff00"
 qdw-bright-blue "#6161ff"
 qdw-bright-cyan "#4bbfc4"
 qdw-bright-magenta "#f5b5ff"
 qdw-bright-white "#aaaaaa"

 
 qdw-ansi-bright-red "#973030"
 )


 (defun list-colors-qdw ()
  (interactive)
 
  (message "qdw-black: \"%s\"" qdw-black)
  (message "qdw-red: \"%s\"" qdw-red)
  (message "qdw-green: \"%s\"" qdw-green)
  (message "qdw-yellow: \"%s\"" qdw-yellow)
  (message "qdw-blue: \"%s\"" qdw-blue)
  (message "qdw-cyan: \"%s\"" qdw-cyan)
  (message "qdw-magenta: \"%s\"" qdw-magenta)
  (message "qdw-white: \"%s\"" qdw-white)

  (message "qdw-bright-black: \"%s\"" qdw-bright-black)
  (message "qdw-bright-red: \"%s\"" qdw-bright-red)
  (message "qdw-bright-green: \"%s\"" qdw-bright-green)
  (message "qdw-bright-yellow: \"%s\"" qdw-bright-yellow)
  (message "qdw-bright-blue: \"%s\"" blue)
  (message "qdw-bright-cyan: \"%s\"" qdw-bright-cyan)
  (message "qdw-bright-magenta: \"%s\"" magenta)
  (message "qdw-bright-white: \"%s\"" qdw-bright-white)
  (message "qdw-ansi-bright-red: \"%s\"" qdw-ansi-bright-red))


 (defun 0x (color-name)
   (letrec ((rgb (color-name-to-rgb color-name))
	    (hex (color-rgb-to-hex rgb)))
     (message "equivalent exchange: \"%s\"" color-name)))
