;;; -*- lexical-binding: t -*-

(setq
 qdw-black  "#141414"  ;;      Grey15
 ;; black  "#000000"
 qdw-bright-black  "#343434"
 qdw-red  "#af5f00"  ;;      actually orange
 qdw-bright-red  "#ff7f00"  ;;      bright orange; used for the cursor &c.
 qdw-green  "#3b8560"
 qdw-bright-green  "#00a700"  ;; bright green ("#00ff00" = brightest possible green)
 qdw-yellow  "#9164ff"  ;;      actually purple
 qdw-bright-yellow  "#b5b500"  ;; bright yellow; "#ffff00"= brightest possible yellow
 qdw-blue  "#6495ed"  ;; decent darker blue (from iTerm): "#0d22b9"
 qdw-bright-blue  "#6161ff"
 qdw-magenta  "#af5faf"
 qdw-bright-magenta  "#d8a0d0"
 qdw-cyan  "#028080"
 qdw-bright-cyan  "#4bbfc4"
 qdw-white  "#878787"
 qdw-bright-white  "#878787"  ;; "#aaaaaa"
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
  (message "extended-bright-red: \"%s\"" extended-bright-red))
